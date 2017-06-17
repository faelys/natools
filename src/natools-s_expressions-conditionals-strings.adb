------------------------------------------------------------------------------
-- Copyright (c) 2015-2017, Natacha Porté                                   --
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

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Natools.Static_Maps.S_Expressions.Conditionals.Strings;

package body Natools.S_Expressions.Conditionals.Strings is

   package Fixed renames Ada.Strings.Fixed;


   function Conditional_On_Atoms
     (Context : in Strings.Context;
      Arguments : in out Lockable.Descriptor'Class;
      Element : access function (Context : in Strings.Context;
                                 Data : in Atom)
                                return Boolean;
      Conjunction : in Boolean)
     return Boolean;
      --  Evaluate Element on all atoms of Arguments and combine them

   function Contains (Context : in Strings.Context; Data : in Atom)
     return Boolean;
      --  Check whether Context contains Data

   function Is_Equal_To (Context : in Strings.Context; Data : in Atom)
     return Boolean;
      --  Check whether Context is equal to Data

   function Is_Prefix (Context : in Strings.Context; Data : in Atom)
     return Boolean;
      --  Check whether Context starts with Data

   function To_Lower (Item : in Character) return Character
     renames Ada.Characters.Handling.To_Lower;
      --  Clearer name for lower case translation, used for case-insentivity



   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Conditional_On_Atoms
     (Context : in Strings.Context;
      Arguments : in out Lockable.Descriptor'Class;
      Element : access function (Context : in Strings.Context;
                                 Data : in Atom)
                                return Boolean;
      Conjunction : in Boolean)
     return Boolean
   is
      Result : Boolean := not Conjunction;
      Event : Events.Event := Arguments.Current_Event;
   begin
      while Event = Events.Add_Atom loop
         Result := Element.all (Context, Arguments.Current_Atom);
         exit when Result /= Conjunction;
         Arguments.Next (Event);
      end loop;

      return Result;
   end Conditional_On_Atoms;


   function Contains (Context : in Strings.Context; Data : in Atom)
     return Boolean
   is
      Str_Value : String := To_String (Data);
   begin
      if Context.Settings.Case_Sensitive then
         return Fixed.Index
           (Context.Data.all,
            Str_Value,
            Str_Value'First,
            Ada.Strings.Forward) > 0;
      else
         Fixed.Translate (Str_Value, To_Lower'Access);
         return Fixed.Index
           (Context.Data.all,
            Str_Value,
            Str_Value'First,
            Ada.Strings.Forward,
            Ada.Characters.Handling.To_Lower'Access) > 0;
      end if;
   end Contains;


   function Is_Equal_To (Context : in Strings.Context; Data : in Atom)
     return Boolean is
   begin
      if Context.Data.all'Length /= Data'Length then
         return False;
      end if;

      if Context.Settings.Case_Sensitive then
         return Context.Data.all = To_String (Data);
      else
         return Fixed.Translate (Context.Data.all, To_Lower'Access)
           = Fixed.Translate (To_String (Data), To_Lower'Access);
      end if;
   end Is_Equal_To;


   function Is_Prefix (Context : in Strings.Context; Data : in Atom)
     return Boolean is
   begin
      if Context.Data.all'Length < Data'Length then
         return False;
      end if;

      declare
         Prefix : String renames Context.Data.all
           (Context.Data.all'First
            .. Context.Data.all'First + Data'Length - 1);
      begin
         if Context.Settings.Case_Sensitive then
            return Prefix = To_String (Data);
         else
            return Fixed.Translate (Prefix, To_Lower'Access)
              = Fixed.Translate (To_String (Data), To_Lower'Access);
         end if;
      end;
   end Is_Prefix;



   ---------------------------
   -- Evaluation Primitives --
   ---------------------------

   function Parametric_Evaluate
     (Context : in Strings.Context;
      Name : in Atom;
      Arguments : in out Lockable.Descriptor'Class)
     return Boolean
   is
      use Natools.Static_Maps.S_Expressions.Conditionals.Strings;
   begin
      case To_Parametric (To_String (Name)) is
         when Unknown_Parametric_Condition =>
            if Context.Parametric_Fallback /= null then
               return Context.Parametric_Fallback
                 (Context.Settings, Name, Arguments);
            else
               raise Constraint_Error with "Unknown parametric condition """
                 & To_String (Name) & '"';
            end if;

         when Case_Insensitive =>
            declare
               New_Context : Strings.Context := Context;
            begin
               New_Context.Settings.Case_Sensitive := False;
               return Evaluate (New_Context, Arguments);
            end;

         when Case_Sensitive =>
            declare
               New_Context : Strings.Context := Context;
            begin
               New_Context.Settings.Case_Sensitive := True;
               return Evaluate (New_Context, Arguments);
            end;

         when Contains_All =>
            return Conditional_On_Atoms
              (Context, Arguments, Contains'Access, True);

         when Contains_Any =>
            return Conditional_On_Atoms
              (Context, Arguments, Contains'Access, False);

         when Is_Equal_To =>
            return Conditional_On_Atoms
              (Context, Arguments, Is_Equal_To'Access, False);

         when Starts_With =>
            return Conditional_On_Atoms
              (Context, Arguments, Is_Prefix'Access, False);
      end case;
   end Parametric_Evaluate;


   function Simple_Evaluate
     (Context : in Strings.Context;
      Name : in Atom)
     return Boolean
   is
      use Natools.Static_Maps.S_Expressions.Conditionals.Strings;
   begin
      case To_Simple (To_String (Name)) is
         when Unknown_Simple_Condition =>
            if Context.Parametric_Fallback /= null then
               return Context.Simple_Fallback (Context.Settings, Name);
            else
               raise Constraint_Error with "Unknown simple condition """
                 & To_String (Name) & '"';
            end if;

         when Is_ASCII =>
            for I in Context.Data.all'Range loop
               if Context.Data (I)
                 not in Character'Val (0) .. Character'Val (127)
               then
                  return False;
               end if;
            end loop;
            return True;

         when Is_Empty =>
            return Context.Data.all'Length = 0;
      end case;
   end Simple_Evaluate;



   --------------------------
   -- Evaluation Shortcuts --
   --------------------------

   function Evaluate
     (Text : in String;
      Expression : in out Lockable.Descriptor'Class)
     return Boolean
   is
      Aliased_Text : aliased constant String := Text;
      Context : constant Strings.Context
        := (Data => Aliased_Text'Access,
            Parametric_Fallback => null,
            Simple_Fallback => null,
            Settings => <>);
   begin
      return Evaluate (Context, Expression);
   end Evaluate;

end Natools.S_Expressions.Conditionals.Strings;
