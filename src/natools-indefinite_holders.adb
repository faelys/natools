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

------------------------------------------------------------------------------
-- Natools.Indefinite_Vectors is an implementation of a subset of Ada 2012  --
-- Containers.Indefinite_Holders, compatible with Ada 2005.                 --
------------------------------------------------------------------------------

package body Natools.Indefinite_Holders is

   function "=" (Left, Right : Holder) return Boolean is
   begin
      if Left.Ref = Right.Ref then
         return True;
      elsif Left.Ref = null or Right.Ref = null then
         return False;
      else
         return Left.Ref.all = Right.Ref.all;
      end if;
   end "=";


   function To_Holder (New_Item : Element_Type) return Holder is
   begin
      return Holder'(Ada.Finalization.Controlled with
         Ref => new Element_Type'(New_Item));
   end To_Holder;


   function Is_Empty (Container : Holder) return Boolean is
   begin
      return Container.Ref = null;
   end Is_Empty;


   procedure Clear (Container : in out Holder) is
   begin
      Free (Container.Ref);
   end Clear;


   function Element (Container : Holder) return Element_Type is
   begin
      if Container.Ref = null then
         raise Constraint_Error with "Element called with an empty holder";
      else
         return Container.Ref.all;
      end if;
   end Element;


   procedure Replace_Element (Container : in out Holder;
                              New_Item  : in     Element_Type) is
   begin
      Free (Container.Ref);
      Container.Ref := new Element_Type'(New_Item);
   end Replace_Element;


   procedure Query_Element
     (Container : in Holder;
      Process   : not null access procedure (Element : in Element_Type)) is
   begin
      if Container.Ref = null then
         raise Constraint_Error
           with "Query_Element called with an empty holder";
      else
         Process.all (Container.Ref.all);
      end if;
   end Query_Element;


   procedure Update_Element
     (Container : in out Holder;
      Process   : not null access procedure (Element : in out Element_Type)) is
   begin
      if Container.Ref = null then
         raise Constraint_Error
           with "Update_Element called with an empty holder";
      else
         Process.all (Container.Ref.all);
      end if;
   end Update_Element;


   function Reference (Container : Holder) return access Element_Type is
   begin
      return Container.Ref;
   end Reference;


   procedure Assign (Target : in out Holder; Source : in Holder) is
   begin
      Free (Target.Ref);
      if Source.Ref /= null then
         Target.Ref := new Element_Type'(Source.Ref.all);
      end if;
   end Assign;


   function Copy (Source : Holder) return Holder is
      Result : Holder;
   begin
      if Source.Ref /= null then
         Result.Ref := new Element_Type'(Source.Ref.all);
      end if;

      return Result;
   end Copy;


   procedure Move (Target : in out Holder; Source : in out Holder) is
   begin
      Free (Target.Ref);
      Target.Ref := Source.Ref;
      Source.Ref := null;
   end Move;

   overriding procedure Adjust (Object : in out Holder) is
      New_Ref : Element_Access := null;
   begin
      if Object.Ref /= null then
         New_Ref := new Element_Type'(Object.Ref.all);
         Object.Ref := New_Ref;
      end if;
   end Adjust;


   overriding procedure Finalize (Object : in out Holder) is
   begin
      Free (Object.Ref);
   end Finalize;

end Natools.Indefinite_Holders;
