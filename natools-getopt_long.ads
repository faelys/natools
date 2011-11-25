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
-- type supposed to cover all command-line flags, including a special value --
-- for non-flag command-line arguments.                                     --
--                                                                          --
-- Option_Definitions objects hold the list of recognized flags. Flags can  --
-- have a single-character short name or a multiple-character long name.    --
-- Moreover, there is no limit to the number of flag names referring to the --
-- same Option_Id value.                                                    --
--                                                                          --
-- Once the Option_Definitions object has been filled with flags recognized --
-- by the client, the actual command-line arguments can be processed.       --
-- Process subprogram uses an Option_Definitions objects and a callback     --
-- procedure that is repeatedly called for each command-line flag and       --
-- argument found in the command line.                                      --
--                                                                          --
-- Process also optionally uses callbacks for error conditions, which       --
-- allows the client application to recover from it and allow command-line  --
-- processing to continue. If there is no error callback (null access),     --
-- an Option_Error exception is raised.                                     --
------------------------------------------------------------------------------


with Ada.Command_Line;
private with Ada.Containers.Indefinite_Ordered_Maps;

generic
   type Option_Id is (<>);

package Natools.Getopt_Long is
   pragma Preelaborate (Getopt_Long);

   Option_Error : exception;

   Null_Long_Name : constant String := "";
   Null_Short_Name : constant Character := Character'Val (0);

   type Argument_Requirement is
     (No_Argument, Required_Argument, Optional_Argument);

   type Option_Definitions is tagged private;

   procedure Add_Option
     (Options    : in out Option_Definitions;
      Long_Name  : String;
      Short_Name : Character;
      Has_Arg    : Argument_Requirement;
      Id         : Option_Id);
      --  Add an option with both a short and a long name to the database.

   procedure Add_Option
     (Options    : in out Option_Definitions;
      Long_Name  : String;
      Has_Arg    : Argument_Requirement;
      Id         : Option_Id);
      --  Add an option with only a long name to the database.

   procedure Add_Option
     (Options    : in out Option_Definitions;
      Short_Name : Character;
      Has_Arg    : Argument_Requirement;
      Id         : Option_Id);
      --  Add an option with only a short name to the database.

   procedure Del_Option
     (Options    : in out Option_Definitions;
      Id         : Option_Id);
      --  Remove from the database an option identified by its id.

   procedure Del_Option
     (Options    : in out Option_Definitions;
      Long_Name  : String);
      --  Remove from the database an option identified by its long name.

   procedure Del_Option
     (Options    : in out Option_Definitions;
      Short_Name : Character);
      --  Remove from the database an option identified by its short name.

   function Format_Long_Names
     (Options     : Option_Definitions;
      Id          : Option_Id;
      Separator   : String := ", ";
      Name_Prefix : String := "--")
      return String;
      --  Return a human-readable list of long names for the given option.

   function Format_Names
     (Options           : Option_Definitions;
      Id                : Option_Id;
      Separator         : String := ", ";
      Long_Name_Prefix  : String := "--";
      Short_Name_Prefix : String := "-";
      Short_First       : Boolean := True)
      return String;
      --  Return a human-readable list of all names for the given option.

   function Format_Short_Names
     (Options     : Option_Definitions;
      Id          : Option_Id;
      Separator   : String := ", ";
      Name_Prefix : String := "-")
      return String;
      --  Return a human-readable list of short names for the given option.

   function Get_Long_Name
     (Options    : Option_Definitions;
      Id         : Option_Id;
      Index      : Positive := 1)
      return String;
      --  Return the "Index"th long name for the given option id.
      --  Raise Constraint_Error when Index is not
      --     in range 1 .. Get_Long_Name_Count (Options, Id)

   function Get_Long_Name_Count
     (Options    : Option_Definitions;
      Id         : Option_Id)
      return Natural;
      --  Return the number of long names for the given option id.

   function Get_Short_Name_Count
     (Options    : Option_Definitions;
      Id         : Option_Id)
      return Natural;
      --  Return the number of short names for the given option id.

   function Get_Short_Names
     (Options    : Option_Definitions;
      Id         : Option_Id)
      return String;
      --  Return a string containing the characters for short names for
      --    the given option id.

   procedure Iterate
     (Options : Option_Definitions;
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

   procedure Process
     (Options : Option_Definitions;
      Top_Level_Argument : Option_Id;
      Callback : not null access procedure (Id : Option_Id;
                                            Argument : String);
      Missing_Argument : access procedure (Id : Option_Id) := null;
      Unexpected_Argument : access procedure (Id : Option_Id;
                                              Arg : String) := null;
      Unknown_Long_Option : access procedure (Name : String) := null;
      Unknown_Short_Option : access procedure (Name : Character) := null;
      Posixly_Correct : Boolean := True;
      Long_Only : Boolean := False;
      Argument_Count : not null access function return Natural
        := Ada.Command_Line.Argument_Count'Access;
      Argument : not null access function (Number : Positive) return String
        := Ada.Command_Line.Argument'Access);
      --  Process system command line argument list, using the provided option
      --    definitions. Callback is called for each identified option with its
      --    idea and the option argument if any, or the empty string otherwise.
      --  When encountering a command-line argument not attached to an option,
      --    Callback is called with Top_Level_Argument and the argument string.
      --  When encontering an option missing a required argument or an unkonwn
      --    option name, the relevant callback is called if not null, otherwise
      --    Option_Error is raised.

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

   type Option_Definitions is tagged record
      By_Long_Name : Long_Option_Maps.Map;
      By_Short_Name : Short_Option_Maps.Map;
   end record;

end Natools.Getopt_Long;
