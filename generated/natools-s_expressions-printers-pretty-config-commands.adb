--  Generated at 2014-06-02 19:12:04 +0000 by Natools.Static_Hash_Maps
--  from ../src/natools-s_expressions-printers-pretty-config-commands.sx

with Natools.S_Expressions.Printers.Pretty.Config.Main_Cmd;
with Natools.S_Expressions.Printers.Pretty.Config.Newline_Cmd;
with Natools.S_Expressions.Printers.Pretty.Config.Quoted_Cmd;
with Natools.S_Expressions.Printers.Pretty.Config.Commands.SC;
with Natools.S_Expressions.Printers.Pretty.Config.Atom_Enc;
with Natools.S_Expressions.Printers.Pretty.Config.Commands.CE;
with Natools.S_Expressions.Printers.Pretty.Config.Hex_Casing;
with Natools.S_Expressions.Printers.Pretty.Config.Newline_Enc;
with Natools.S_Expressions.Printers.Pretty.Config.Quoted_Esc;
with Natools.S_Expressions.Printers.Pretty.Config.Quoted_Opt;
with Natools.S_Expressions.Printers.Pretty.Config.Token_Opt;

package body Natools.S_Expressions.Printers.Pretty.Config.Commands is

   function Main (Key : String) return Main_Command is
      N : constant Natural
        := Natools.S_Expressions.Printers.Pretty.Config.Main_Cmd.Hash (Key);
   begin
      if Map_1_Keys (N).all = Key then
         return Map_1_Elements (N);
      else
         raise Constraint_Error with "Key """ & Key & """ not in map";
      end if;
   end Main;


   function Newline (Key : String) return Newline_Command is
      N : constant Natural
        := Natools.S_Expressions.Printers.Pretty.Config.Newline_Cmd.Hash (Key);
   begin
      if Map_2_Keys (N).all = Key then
         return Map_2_Elements (N);
      else
         raise Constraint_Error with "Key """ & Key & """ not in map";
      end if;
   end Newline;


   function Quoted_String (Key : String) return Quoted_String_Command is
      N : constant Natural
        := Natools.S_Expressions.Printers.Pretty.Config.Quoted_Cmd.Hash (Key);
   begin
      if Map_3_Keys (N).all = Key then
         return Map_3_Elements (N);
      else
         raise Constraint_Error with "Key """ & Key & """ not in map";
      end if;
   end Quoted_String;


   function Separator (Key : String) return Separator_Command is
      N : constant Natural
        := Natools.S_Expressions.Printers.Pretty.Config.Commands.SC.Hash (Key);
   begin
      if Map_4_Keys (N).all = Key then
         return Map_4_Elements (N);
      else
         raise Constraint_Error with "Key """ & Key & """ not in map";
      end if;
   end Separator;


   function To_Atom_Encoding (Key : String) return Atom_Encoding is
      N : constant Natural
        := Natools.S_Expressions.Printers.Pretty.Config.Atom_Enc.Hash (Key);
   begin
      if Map_5_Keys (N).all = Key then
         return Map_5_Elements (N);
      else
         raise Constraint_Error with "Key """ & Key & """ not in map";
      end if;
   end To_Atom_Encoding;


   function To_Character_Encoding (Key : String) return Character_Encoding is
      N : constant Natural
        := Natools.S_Expressions.Printers.Pretty.Config.Commands.CE.Hash (Key);
   begin
      if Map_6_Keys (N).all = Key then
         return Map_6_Elements (N);
      else
         raise Constraint_Error with "Key """ & Key & """ not in map";
      end if;
   end To_Character_Encoding;


   function To_Hex_Casing (Key : String) return Encodings.Hex_Casing is
      N : constant Natural
        := Natools.S_Expressions.Printers.Pretty.Config.Hex_Casing.Hash (Key);
   begin
      if Map_7_Keys (N).all = Key then
         return Map_7_Elements (N);
      else
         raise Constraint_Error with "Key """ & Key & """ not in map";
      end if;
   end To_Hex_Casing;


   function To_Newline_Encoding (Key : String) return Newline_Encoding is
      N : constant Natural
        := Natools.S_Expressions.Printers.Pretty.Config.Newline_Enc.Hash (Key);
   begin
      if Map_8_Keys (N).all = Key then
         return Map_8_Elements (N);
      else
         raise Constraint_Error with "Key """ & Key & """ not in map";
      end if;
   end To_Newline_Encoding;


   function To_Quoted_Escape (Key : String) return Quoted_Escape_Type is
      N : constant Natural
        := Natools.S_Expressions.Printers.Pretty.Config.Quoted_Esc.Hash (Key);
   begin
      if Map_9_Keys (N).all = Key then
         return Map_9_Elements (N);
      else
         raise Constraint_Error with "Key """ & Key & """ not in map";
      end if;
   end To_Quoted_Escape;


   function To_Quoted_Option (Key : String) return Quoted_Option is
      N : constant Natural
        := Natools.S_Expressions.Printers.Pretty.Config.Quoted_Opt.Hash (Key);
   begin
      if Map_10_Keys (N).all = Key then
         return Map_10_Elements (N);
      else
         raise Constraint_Error with "Key """ & Key & """ not in map";
      end if;
   end To_Quoted_Option;


   function To_Token_Option (Key : String) return Token_Option is
      N : constant Natural
        := Natools.S_Expressions.Printers.Pretty.Config.Token_Opt.Hash (Key);
   begin
      if Map_11_Keys (N).all = Key then
         return Map_11_Elements (N);
      else
         raise Constraint_Error with "Key """ & Key & """ not in map";
      end if;
   end To_Token_Option;

end Natools.S_Expressions.Printers.Pretty.Config.Commands;
