------------------------------------------------------------------------------
-- Copyright (c) 2011, Natacha Porté                                        --
--                                                                          --
-- Permission to use, copy, modify, and distribute this software for any    --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES --
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         --
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  --
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   --
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    --
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  --
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Natools.Getopt_Long is a native Ada implementation of getopt_long()      --
-- processor for command line arguments.                                    --
--                                                                          --
-- This package is generic, and its only formal parameter is a descrete     --
-- type supposed to cover all command-line options.                         --
--                                                                          --
-- Configuration objects hold the list of recognized options and parameters --
-- about how to process them. Options can have a single-character short     --
-- name or a multiple-character long name. Moreover, there is no limit to   --
-- the number of flag names referring to the same Option_Id value.          --
--                                                                          --
-- Once the Configuration object has been filled with flags recognized      --
-- by the client, the actual command-line arguments can be processed,       --
-- using the handler callbacks from a Handlers.Callback'Class object.       --
--                                                                          --
-- Callback subprograms for normal operation are Option, for command-line   --
-- flags identified by their Option_Id, and Argument, for top-level command --
-- line arguments. There are also callbacks for error conditions (missing   --
-- or unexpected argument, unknown option), whose implementation in         --
-- Handlers.Callback are simply to raise Option_Error with an appropriate   --
-- message.                                                                 --
------------------------------------------------------------------------------


with Ada.Command_Line;
private with Ada.Containers.Indefinite_Ordered_Maps;

generic
   type Option_Id is (<>);

package Natools.Getopt_Long is
   pragma Preelaborate (Getopt_Long);

   Null_Long_Name : constant String := "";
   Null_Short_Name : constant Character := Character'Val (0);



   ------------------------------------------
   -- Holder for both short and long names --
   ------------------------------------------

   type Name_Style is (Long, Short);

   type Any_Name (Style : Name_Style; Size : Positive) is record
      case Style is
         when Short =>
            Short : Character;
         when Long =>
            Long : String (1 .. Size);
      end case;
   end record;

   function To_Name (Long_Name : String) return Any_Name;
   function To_Name (Short_Name : Character) return Any_Name;
   function Image (Name : Any_Name) return String;



   ------------------------
   -- Callback interface --
   ------------------------

   Option_Error : exception;

   package Handlers is

      type Callback is abstract tagged null record;

      procedure Option
        (Handler  : in out Callback;
         Id       : Option_Id;
         Argument : String)
         is abstract;
         --  Callback for successfully-parsed options.

      procedure Argument
        (Handler  : in out Callback;
         Argument : String)
         is abstract;
         --  Callback for non-flag arguments.

      procedure Missing_Argument
        (Handler : in out Callback;
         Id      : Option_Id;
         Name    : Any_Name);
         --  Raise Option_Error (default error handler).

      procedure Unexpected_Argument
        (Handler  : in out Callback;
         Id       : Option_Id;
         Name     : Any_Name;
         Argument : String);
         --  Raise Option_Error (default error handler).

      procedure Unknown_Option
        (Handler : in out Callback;
         Name    : Any_Name);
         --  Raise Option_Error (default error handler).

   end Handlers;



   ----------------------------
   -- Configuration database --
   ----------------------------

   type Argument_Requirement is
     (No_Argument, Required_Argument, Optional_Argument);

   type Configuration is tagged private;


   -- Simple parameters --

   function Posixly_Correct (Config : Configuration) return Boolean;

   procedure Posixly_Correct
     (Config : in out Configuration;
      To     : Boolean := True);

   function Long_Only (Config : Configuration) return Boolean;

   procedure Use_Long_Only
     (Config : in out Configuration;
      Value  : Boolean := True);


   -- Option list management --

   procedure Add_Option
     (Config     : in out Configuration;
      Long_Name  : String;
      Short_Name : Character;
      Has_Arg    : Argument_Requirement;
      Id         : Option_Id);
      --  Add an option with both a short and a long name to the database.

   procedure Add_Option
     (Config     : in out Configuration;
      Long_Name  : String;
      Has_Arg    : Argument_Requirement;
      Id         : Option_Id);
      --  Add an option with only a long name to the database.

   procedure Add_Option
     (Config     : in out Configuration;
      Short_Name : Character;
      Has_Arg    : Argument_Requirement;
      Id         : Option_Id);
      --  Add an option with only a short name to the database.

   procedure Del_Option
     (Config     : in out Configuration;
      Id         : Option_Id);
      --  Remove from the database an option identified by its id.

   procedure Del_Option
     (Config     : in out Configuration;
      Long_Name  : String);
      --  Remove from the database an option identified by its long name.

   procedure Del_Option
     (Config     : in out Configuration;
      Short_Name : Character);
      --  Remove from the database an option identified by its short name.


   -- Formatting subprograms --

   function Format_Long_Names
     (Config      : Configuration;
      Id          : Option_Id;
      Separator   : String := ", ";
      Name_Prefix : String := "--")
      return String;
      --  Return a human-readable list of long names for the given option.

   function Format_Names
     (Config            : Configuration;
      Id                : Option_Id;
      Separator         : String := ", ";
      Long_Name_Prefix  : String := "--";
      Short_Name_Prefix : String := "-";
      Short_First       : Boolean := True)
      return String;
      --  Return a human-readable list of all names for the given option.

   function Format_Short_Names
     (Config      : Configuration;
      Id          : Option_Id;
      Separator   : String := ", ";
      Name_Prefix : String := "-")
      return String;
      --  Return a human-readable list of short names for the given option.

   function Get_Long_Name
     (Config     : Configuration;
      Id         : Option_Id;
      Index      : Positive := 1)
      return String;
      --  Return the "Index"th long name for the given option id.
      --  Raise Constraint_Error when Index is not
      --     in range 1 .. Get_Long_Name_Count (Config, Id)

   function Get_Long_Name_Count
     (Config     : Configuration;
      Id         : Option_Id)
      return Natural;
      --  Return the number of long names for the given option id.

   function Get_Short_Name_Count
     (Config     : Configuration;
      Id         : Option_Id)
      return Natural;
      --  Return the number of short names for the given option id.

   function Get_Short_Names
     (Config     : Configuration;
      Id         : Option_Id)
      return String;
      --  Return a string containing the characters for short names for
      --    the given option id.

   procedure Iterate
     (Config  : Configuration;
      Process : not null access procedure (Id : Option_Id;
                                           Long_Name : String;
                                           Short_Name : Character;
                                           Has_Arg : Argument_Requirement));
      --  Iterate over all options, starting with options having a short name,
      --    followed by options having only a long name, sorted respectively by
      --    short and long name.
      --  Process is called for each option; for options lacking a long name,
      --    Long_Name is "", and for options lacking a short name, Short_Name
      --    is Character'Val (0).



   --------------------------------------
   -- Command line argument processing --
   --------------------------------------

   procedure Process
     (Config : Configuration;
      Handler : in out Handlers.Callback'Class;
      Argument_Count : not null access function return Natural
        := Ada.Command_Line.Argument_Count'Access;
      Argument : not null access function (Number : Positive) return String
        := Ada.Command_Line.Argument'Access);
      --  Process system command line argument list, using the provided option
      --    definitions and handler callbacks.

private

   type Option (Long_Name_Length : Natural) is record
      Id : Option_Id;
      Has_Arg : Argument_Requirement;
      Long_Name : String (1 .. Long_Name_Length);
      Short_Name : Character;
   end record;

   package Long_Option_Maps is
      new Ada.Containers.Indefinite_Ordered_Maps (String, Option);

   package Short_Option_Maps is
      new Ada.Containers.Indefinite_Ordered_Maps (Character, Option);

   type Configuration is tagged record
      By_Long_Name : Long_Option_Maps.Map;
      By_Short_Name : Short_Option_Maps.Map;
      Posixly_Correct : Boolean := True;
      Long_Only : Boolean := False;
   end record;

end Natools.Getopt_Long;
