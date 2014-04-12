------------------------------------------------------------------------------
-- Copyright (c) 2014, Natacha Porté                                        --
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

package body Natools.S_Expressions.Atom_Buffers is

   procedure Preallocate (Buffer : in out Atom_Buffer; Length : in Count) is
      Old_Size, New_Size : Count := 0;
   begin
      if Buffer.Used + Length <= Buffer.Available then
         return;
      end if;

      Old_Size := Buffer.Available;
      New_Size := Buffer.Used + Length;

      if Buffer.Ref.Is_Empty then
         declare
            function Create return Atom;

            function Create return Atom is
            begin
               return Atom'(1 .. New_Size => <>);
            end Create;
         begin
            Buffer.Ref.Replace (Create'Access);
         end;
      else
         declare
            function Create return Atom;

            Old_Accessor : constant Atom_Refs.Accessor := Buffer.Ref.Query;

            function Create return Atom is
            begin
               return Result : Atom (1 .. New_Size) do
                  Result (1 .. Old_Size) := Old_Accessor.Data.all;
               end return;
            end Create;
         begin
            Buffer.Ref.Replace (Create'Access);
         end;
      end if;

      Buffer.Available := New_Size;
   end Preallocate;


   procedure Append (Buffer : in out Atom_Buffer; Data : in Atom) is
   begin
      if Data'Length > 0 then
         Preallocate (Buffer, Data'Length);
         Buffer.Ref.Update.Data.all
           (Buffer.Used + 1 .. Buffer.Used + Data'Length)
           := Data;
         Buffer.Used := Buffer.Used + Data'Length;
      end if;
   end Append;


   procedure Append (Buffer : in out Atom_Buffer; Data : in Octet) is
   begin
      Preallocate (Buffer, 1);
      Buffer.Ref.Update.Data.all (Buffer.Used + 1) := Data;
      Buffer.Used := Buffer.Used + 1;
   end Append;


   procedure Append_Reverse (Buffer : in out Atom_Buffer; Data : in Atom) is
      procedure Process (Target : in out Atom);

      procedure Process (Target : in out Atom) is
      begin
         for I in reverse Data'Range loop
            Buffer.Used := Buffer.Used + 1;
            Target (Buffer.Used) := Data (I);
         end loop;
      end Process;
   begin
      Preallocate (Buffer, Data'Length);
      Buffer.Ref.Update (Process'Access);
   end Append_Reverse;


   procedure Invert (Buffer : in out Atom_Buffer) is
      procedure Process (Data : in out Atom);

      procedure Process (Data : in out Atom) is
         Low : Count := Data'First;
         High : Count := Buffer.Used;
         Tmp : Octet;
      begin
         while Low < High loop
            Tmp := Data (Low);
            Data (Low) := Data (High);
            Data (High) := Tmp;
            Low := Low + 1;
            High := High - 1;
         end loop;
      end Process;
   begin
      if not Buffer.Ref.Is_Empty then
         Buffer.Ref.Update (Process'Access);
      end if;
   end Invert;


   function Length (Buffer : Atom_Buffer) return Count is
   begin
      return Buffer.Used;
   end Length;


   function Data (Buffer : Atom_Buffer) return Atom is
   begin
      if Buffer.Ref.Is_Empty then
         pragma Assert (Buffer.Available = 0 and Buffer.Used = 0);
         return Null_Atom;
      else
         return Buffer.Ref.Query.Data.all (1 .. Buffer.Used);
      end if;
   end Data;


   function Raw_Query (Buffer : Atom_Buffer) return Atom_Refs.Accessor is
      function Create return Atom;

      function Create return Atom is
      begin
         return Null_Atom;
      end Create;
   begin
      if Buffer.Ref.Is_Empty then
         declare
            Tmp_Ref : constant Atom_Refs.Reference
              := Atom_Refs.Create (Create'Access);
         begin
            return Tmp_Ref.Query;
         end;
      else
         return Buffer.Ref.Query;
      end if;
   end Raw_Query;


   procedure Query
     (Buffer : in Atom_Buffer;
      Process : not null access procedure (Data : in Atom)) is
   begin
      if Buffer.Ref.Is_Empty then
         Process.all (Null_Atom);
      else
         Process.all (Buffer.Ref.Query.Data.all (1 .. Buffer.Used));
      end if;
   end Query;


   procedure Read
     (Buffer : in Atom_Buffer;
      Data : out Atom;
      Length : out Count)
   is
      Transmit : constant Count := Count'Min (Data'Length, Buffer.Used);
   begin
      Length := Buffer.Used;

      if Buffer.Ref.Is_Empty then
         pragma Assert (Length = 0);
         null;
      else
         Data (Data'First .. Data'First + Transmit - 1)
           := Buffer.Ref.Query.Data.all (1 .. Transmit);
      end if;
   end Read;


   function Element (Buffer : Atom_Buffer; Position : Count) return Octet is
   begin
      return Buffer.Ref.Query.Data.all (Position);
   end Element;


   procedure Pop (Buffer : in out Atom_Buffer; Data : out Octet) is
   begin
      Data := Buffer.Ref.Query.Data.all (Buffer.Used);
      Buffer.Used := Buffer.Used - 1;
   end Pop;


   procedure Hard_Reset (Buffer : in out Atom_Buffer) is
   begin
      Buffer.Ref.Reset;
      Buffer.Available := 0;
      Buffer.Used := 0;
   end Hard_Reset;


   procedure Soft_Reset (Buffer : in out Atom_Buffer) is
   begin
      Buffer.Used := 0;
   end Soft_Reset;

end Natools.S_Expressions.Atom_Buffers;
