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
function Natools.S_Expressions.Printers.Pretty.Config.Commands.T
  return Boolean is
begin
   for I in Map_1_Keys'Range loop
      if Natools.S_Expressions.Printers.Pretty.Config.Main_Cmd.Hash
           (Map_1_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   for I in Map_2_Keys'Range loop
      if Natools.S_Expressions.Printers.Pretty.Config.Newline_Cmd.Hash
           (Map_2_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   for I in Map_3_Keys'Range loop
      if Natools.S_Expressions.Printers.Pretty.Config.Quoted_Cmd.Hash
           (Map_3_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   for I in Map_4_Keys'Range loop
      if Natools.S_Expressions.Printers.Pretty.Config.Commands.SC.Hash
           (Map_4_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   for I in Map_5_Keys'Range loop
      if Natools.S_Expressions.Printers.Pretty.Config.Atom_Enc.Hash
           (Map_5_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   for I in Map_6_Keys'Range loop
      if Natools.S_Expressions.Printers.Pretty.Config.Commands.CE.Hash
           (Map_6_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   for I in Map_7_Keys'Range loop
      if Natools.S_Expressions.Printers.Pretty.Config.Hex_Casing.Hash
           (Map_7_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   for I in Map_8_Keys'Range loop
      if Natools.S_Expressions.Printers.Pretty.Config.Newline_Enc.Hash
           (Map_8_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   for I in Map_9_Keys'Range loop
      if Natools.S_Expressions.Printers.Pretty.Config.Quoted_Esc.Hash
           (Map_9_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   for I in Map_10_Keys'Range loop
      if Natools.S_Expressions.Printers.Pretty.Config.Quoted_Opt.Hash
           (Map_10_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   for I in Map_11_Keys'Range loop
      if Natools.S_Expressions.Printers.Pretty.Config.Token_Opt.Hash
           (Map_11_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   return True;
end Natools.S_Expressions.Printers.Pretty.Config.Commands.T;
