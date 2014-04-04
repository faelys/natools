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

with Ada.Unchecked_Conversion;

package body Natools.HMAC is

   function To_Stream_Element_Array (Key : String)
     return Ada.Streams.Stream_Element_Array;
   pragma Inline (To_Stream_Element_Array);
      --  Convert a String into a Stream_Element_Array

   function Pad
     (Key : Ada.Streams.Stream_Element_Array;
      Pattern : Ada.Streams.Stream_Element)
     return Ada.Streams.Stream_Element_Array;
      --  Scramble Key with the given pattern

   Outer_Pattern : constant Ada.Streams.Stream_Element := 16#5C#;
   Inner_Pattern : constant Ada.Streams.Stream_Element := 16#36#;


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function To_Stream_Element_Array (Key : String)
     return Ada.Streams.Stream_Element_Array
   is
      subtype Ad_Hoc_String is String (Key'Range);
      subtype Ad_Hoc_Array
        is Ada.Streams.Stream_Element_Array (1 .. Key'Length);

      function Unchecked_Conversion is new Ada.Unchecked_Conversion
        (Ad_Hoc_String, Ad_Hoc_Array);
   begin
      return Unchecked_Conversion (Key);
   end To_Stream_Element_Array;


   function Pad
     (Key : Ada.Streams.Stream_Element_Array;
      Pattern : Ada.Streams.Stream_Element)
     return Ada.Streams.Stream_Element_Array
   is
      use type Ada.Streams.Stream_Element;

      Result : Ada.Streams.Stream_Element_Array (Key'Range);
   begin
      for I in Result'Range loop
         Result (I) := Key (I) xor Pattern;
      end loop;

      return Result;
   end Pad;



   --------------------
   -- HAMC Interface --
   --------------------

   procedure Setup
     (C : out Context;
      Key : in Ada.Streams.Stream_Element_Array) is
   begin
      C := Create (Key);
   end Setup;


   procedure Setup
     (C : out Context;
      Key : in String) is
   begin
      C := Create (Key);
   end Setup;


   function Create (Key : Ada.Streams.Stream_Element_Array) return Context is
      Result : Context
        := (Key => (others => 0),
            Hash => Initial_Context);

      use type Ada.Streams.Stream_Element_Count;
   begin
      if Key'Length <= Block_Size_In_SE then
         Result.Key (1 .. Key'Length) := Key;
      else
         declare
            Local_Hash : Hash_Context := Initial_Context;
         begin
            Update (Local_Hash, Key);

            declare
               Hashed_Key : constant Ada.Streams.Stream_Element_Array
                 := Digest (Local_Hash);
            begin
               Result.Key (1 .. Hashed_Key'Length) := Hashed_Key;
            end;
         end;
      end if;

      Update (Result.Hash, Pad (Result.Key, Inner_Pattern));

      return Result;
   end Create;


   function Create (Key : String) return Context is
   begin
      return Create (To_Stream_Element_Array (Key));
   end Create;


   procedure Update
     (C : in out Context;
      Input : in Ada.Streams.Stream_Element_Array) is
   begin
      Update (C.Hash, Input);
   end Update;


   procedure Update
     (C : in out Context;
      Input : in String) is
   begin
      Update (C.Hash, To_Stream_Element_Array (Input));
   end Update;


   function Digest (C : Context) return Ada.Streams.Stream_Element_Array is
      Local_Hash : Hash_Context := Initial_Context;
   begin
      Update (Local_Hash, Pad (C.Key, Outer_Pattern));
      Update (Local_Hash, Digest (C.Hash));
      return Digest (Local_Hash);
   end Digest;


   function Digest (Key : String; Message : Ada.Streams.Stream_Element_Array)
     return Ada.Streams.Stream_Element_Array
   is
      Local_Context : Context := Create (Key);
   begin
      Update (Local_Context, Message);
      return Digest (Local_Context);
   end Digest;


   function Digest (Key, Message : Ada.Streams.Stream_Element_Array)
     return Ada.Streams.Stream_Element_Array
   is
      Local_Context : Context := Create (Key);
   begin
      Update (Local_Context, Message);
      return Digest (Local_Context);
   end Digest;

end Natools.HMAC;
