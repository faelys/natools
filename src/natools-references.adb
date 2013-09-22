------------------------------------------------------------------------------
-- Copyright (c) 2013, Natacha Porté                                        --
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

with Ada.Unchecked_Deallocation;

package body Natools.References is

   ---------------------------------
   -- Low-level memory management --
   ---------------------------------

   overriding procedure Adjust (Object : in out Reference) is
   begin
      if Object.Count /= null then
         Object.Count.all := Object.Count.all + 1;
      end if;
   end Adjust;


   overriding procedure Finalize (Object : in out Reference) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Held_Data, Data_Access);
      procedure Free is
        new Ada.Unchecked_Deallocation (Counter, Counter_Access);
   begin
      if Object.Count /= null then
         Object.Count.all := Object.Count.all - 1;

         if Object.Count.all = 0 then
            Free (Object.Count);
            Free (Object.Data);
         else
            Object.Count := null;
            Object.Data := null;
         end if;
      end if;
   end Finalize;



   -----------------------------------------
   -- Object construction and destruction --
   -----------------------------------------

   function Create
     (Constructor : not null access function return Held_Data)
      return Reference is
   begin
      return (Ada.Finalization.Controlled with
         Data => new Held_Data'(Constructor.all),
         Count => new Counter'(1));
   end Create;


   procedure Replace
     (Ref : in out Reference;
      Constructor : not null access function return Held_Data) is
   begin
      Finalize (Ref);
      Ref.Data := new Held_Data'(Constructor.all);
      Ref.Count := new Counter'(1);
   end Replace;


   procedure Reset (Ref : in out Reference) is
   begin
      Finalize (Ref);
   end Reset;


   function Is_Empty (Ref : Reference) return Boolean is
   begin
      return Ref.Count = null;
   end Is_Empty;


   function "=" (Left, Right : Reference) return Boolean is
   begin
      return Left.Data = Right.Data;
   end "=";



   ----------------------
   -- Dereferenciation --
   ----------------------

   function Query (Ref : in Reference) return Accessor is
   begin
      return Accessor'(Data => Ref.Data, Parent => Ref);
   end Query;


   function Update (Ref : in Reference) return Mutator is
   begin
      return Mutator'(Data => Ref.Data, Parent => Ref);
   end Update;


   procedure Query
     (Ref : in Reference;
      Process : not null access procedure (Object : in Held_Data)) is
   begin
      Process.all (Ref.Data.all);
   end Query;


   procedure Update
     (Ref : in Reference;
      Process : not null access procedure (Object : in out Held_Data)) is
   begin
      Process.all (Ref.Data.all);
   end Update;

end Natools.References;

