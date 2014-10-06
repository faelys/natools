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

package body Natools.Constant_Indefinite_Ordered_Maps is

   --------------------------
   -- Sorted Array Backend --
   --------------------------

   function Create
     (Size : Index_Type;
      Key_Factory : not null access function (Index : Index_Type)
         return Key_Type;
      Element_Factory : not null access function (Index : Index_Type)
         return Element_Type)
     return Backend_Array
   is
      function Node_Factory (Index : Index_Type) return Node with Inline;

      function Node_Factory (Index : Index_Type) return Node is
      begin
         return
           (Key => new Key_Type'(Key_Factory (Index)),
            Element => new Element_Type'(Element_Factory (Index)));
      end Node_Factory;

      First_Node : constant Node := Node_Factory (1);
   begin
      return Result : Backend_Array
        := (Ada.Finalization.Limited_Controlled with
            Size => Size,
            Nodes => (others => First_Node),
            Finalized => False)
      do
         if Size >= 2 then
            for I in 2 .. Size loop
               Result.Nodes (I) := Node_Factory (I);
            end loop;
         end if;
      end return;
   end Create;


   function Make_Backend
     (Size : Count_Type;
      Key_Factory : not null access function (Index : Index_Type)
         return Key_Type;
      Element_Factory : not null access function (Index : Index_Type)
         return Element_Type)
     return Backend_Refs.Immutable_Reference
   is
      function Create return Backend_Array;

      function Create return Backend_Array is
      begin
         return Create (Size, Key_Factory, Element_Factory);
      end Create;
   begin
      if Size = 0 then
         return Backend_Refs.Null_Immutable_Reference;
      else
         return Backend_Refs.Create (Create'Access);
      end if;
   end Make_Backend;


   function Make_Backend (Map : Unsafe_Maps.Map)
     return Backend_Refs.Immutable_Reference
   is
      function Create return Backend_Array;
      function Element (Index : Index_Type) return Element_Type;
      function Key (Index : Index_Type) return Key_Type;
      procedure Update_Cursor (Index : in Index_Type);
      function Is_Valid (Nodes : Node_Array) return Boolean;

      Length : constant Count_Type := Map.Length;
      Cursor : Unsafe_Maps.Cursor := Map.First;
      I : Index_Type := 1;

      function Create return Backend_Array is
      begin
         return Create (Length, Key'Access, Element'Access);
      end Create;

      function Element (Index : Index_Type) return Element_Type is
      begin
         Update_Cursor (Index);
         return Unsafe_Maps.Element (Cursor);
      end Element;

      function Is_Valid (Nodes : Node_Array) return Boolean is
      begin
         return (for all J in Nodes'First + 1 .. Nodes'Last
            => Nodes (J - 1).Key.all < Nodes (J).Key.all);
      end Is_Valid;

      function Key (Index : Index_Type) return Key_Type is
      begin
         Update_Cursor (Index);
         pragma Assert (Unsafe_Maps.Has_Element (Cursor));
         return Unsafe_Maps.Key (Cursor);
      end Key;

      procedure Update_Cursor (Index : in Index_Type) is
      begin
         if Index = I + 1 then
            Unsafe_Maps.Next (Cursor);
            I := I + 1;
         elsif Index /= I then
            raise Program_Error with "Unexpected index value"
              & Index_Type'Image (Index)
              & " (previous value"
              & Index_Type'Image (I)
              & ')';
         end if;
      end Update_Cursor;

      Result : Backend_Refs.Immutable_Reference;
   begin
      if Length = 0 then
         return Backend_Refs.Null_Immutable_Reference;
      end if;

      Result := Backend_Refs.Create (Create'Access);
      pragma Assert (I = Length);
      pragma Assert (Unsafe_Maps."=" (Cursor, Map.Last));
      pragma Assert (Is_Valid (Result.Query.Data.Nodes));
      return Result;
   end Make_Backend;


   overriding procedure Finalize (Object : in out Backend_Array) is
      Key : Key_Access;
      Element : Element_Access;
   begin
      if not Object.Finalized then
         for I in Object.Nodes'Range loop
            Key := Object.Nodes (I).Key;
            Element := Object.Nodes (I).Element;
            Free (Key);
            Free (Element);
         end loop;
         Object.Finalized := True;
      end if;
   end Finalize;


   procedure Search
     (Nodes : in Node_Array;
      Key : in Key_Type;
      Floor : out Count_Type;
      Ceiling : out Count_Type)
   is
      Middle : Index_Type;
   begin
      Floor := 0;
      Ceiling := 0;

      if Nodes'Length = 0 then
         return;
      end if;

      Floor := Nodes'First;
      if Key < Nodes (Floor).Key.all then
         Ceiling := Floor;
         Floor := 0;
         return;
      elsif not (Nodes (Floor).Key.all < Key) then
         Ceiling := Floor;
         return;
      end if;

      Ceiling := Nodes'Last;
      if Nodes (Ceiling).Key.all < Key then
         Floor := Ceiling;
         Ceiling := 0;
         return;
      elsif not (Key < Nodes (Ceiling).Key.all) then
         Floor := Ceiling;
         return;
      end if;

      while Ceiling - Floor >= 2 loop
         Middle := Floor + (Ceiling - Floor) / 2;

         if Nodes (Middle).Key.all < Key then
            Floor := Middle;
         elsif Key < Nodes (Middle).Key.all then
            Ceiling := Middle;
         else
            Floor := Middle;
            Ceiling := Middle;
            return;
         end if;
      end loop;

      return;
   end Search;



   -----------------------
   -- Cursor Operations --
   -----------------------

   function "<" (Left, Right : Cursor) return Boolean is
   begin
      return Key (Left) < Key (Right);
   end "<";


   function ">" (Left, Right : Cursor) return Boolean is
   begin
      return Key (Right) < Key (Left);
   end ">";


   function "<" (Left : Cursor; Right : Key_Type) return Boolean is
   begin
      return Key (Left) < Right;
   end "<";


   function ">" (Left : Cursor; Right : Key_Type) return Boolean is
   begin
      return Right < Key (Left);
   end ">";


   function "<" (Left : Key_Type; Right : Cursor) return Boolean is
   begin
      return Left < Key (Right);
   end "<";


   function ">" (Left : Key_Type; Right : Cursor) return Boolean is
   begin
      return Key (Right) < Left;
   end ">";


   procedure Clear (Position : in out Cursor) is
   begin
      Position := No_Element;
   end Clear;


   function Element (Position : Cursor) return Element_Type is
   begin
      return Position.Backend.Query.Data.Nodes (Position.Index).Element.all;
   end Element;


   function Key (Position : Cursor) return Key_Type is
   begin
      return Position.Backend.Query.Data.Nodes (Position.Index).Key.all;
   end Key;


   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Is_Empty
        or else Position.Index >= Position.Backend.Query.Data.Size
      then
         return No_Element;
      else
         return
           (Is_Empty => False,
            Backend => Position.Backend,
            Index => Position.Index + 1);
      end if;
   end Next;


   procedure Next (Position : in out Cursor) is
   begin
      if Position.Is_Empty then
         null;
      elsif Position.Index >= Position.Backend.Query.Data.Size then
         Position := No_Element;
      else
         Position.Index := Position.Index + 1;
      end if;
   end Next;


   function Previous (Position : Cursor) return Cursor is
   begin
      if Position.Is_Empty or else Position.Index = 1 then
         return No_Element;
      else
         return
           (Is_Empty => False,
            Backend => Position.Backend,
            Index => Position.Index - 1);
      end if;
   end Previous;


   procedure Previous (Position : in out Cursor) is
   begin
      if Position.Is_Empty then
         null;
      elsif Position.Index = 1 then
         Position := No_Element;
      else
         Position.Index := Position.Index - 1;
      end if;
   end Previous;


   procedure Query_Element
     (Position : in Cursor;
      Process : not null access procedure (Key : in Key_Type;
                                           Element : in Element_Type))
   is
      Accessor : constant Backend_Refs.Accessor := Position.Backend.Query;
   begin
      Process.all
        (Accessor.Data.Nodes (Position.Index).Key.all,
         Accessor.Data.Nodes (Position.Index).Element.all);
   end Query_Element;



   -----------------------------
   -- Non-Standard Operations --
   -----------------------------

   function Create (Source : Unsafe_Maps.Map) return Constant_Map is
   begin
      return (Backend => Make_Backend (Source));
   end Create;


   procedure Replace
     (Container : in out Constant_Map;
      New_Items : in Unsafe_Maps.Map) is
   begin
      Container.Backend := Make_Backend (New_Items);
   end Replace;


   function To_Unsafe_Map (Container : Constant_Map) return Unsafe_Maps.Map is
      Result : Unsafe_Maps.Map;
   begin
      if Container.Backend.Is_Empty then
         return Result;
      end if;

      declare
         Accessor : constant Backend_Refs.Accessor := Container.Backend.Query;
      begin
         for I in Accessor.Data.Nodes'Range loop
            Result.Insert
              (Accessor.Data.Nodes (I).Key.all,
               Accessor.Data.Nodes (I).Element.all);
         end loop;
      end;

      return Result;
   end To_Unsafe_Map;



   -----------------------------
   -- Constant Map Operations --
   -----------------------------

   function "=" (Left, Right : Constant_Map) return Boolean is
      use type Backend_Refs.Immutable_Reference;
   begin
      return Left.Backend = Right.Backend;
   end "=";


   function Ceiling (Container : Constant_Map; Key : Key_Type) return Cursor is
      Floor, Ceiling : Count_Type;
   begin
      if Container.Is_Empty then
         return No_Element;
      end if;

      Search (Container.Backend.Query.Data.Nodes, Key, Floor, Ceiling);

      if Ceiling > 0 then
         return (Is_Empty => False,
            Backend => Container.Backend,
            Index => Ceiling);
      else
         return No_Element;
      end if;
   end Ceiling;


   procedure Clear (Container : in out Constant_Map) is
   begin
      Container.Backend.Reset;
   end Clear;


   function Contains (Container : Constant_Map; Key : Key_Type)
     return Boolean
   is
      Floor, Ceiling : Count_Type;
   begin
      if Container.Is_Empty then
         return False;
      end if;

      Search (Container.Backend.Query.Data.Nodes, Key, Floor, Ceiling);
      return Floor = Ceiling;
   end Contains;


   function Element (Container : Constant_Map; Key : Key_Type)
     return Element_Type is
   begin
      return Element (Find (Container, Key));
   end Element;


   function Find (Container : Constant_Map; Key : Key_Type) return Cursor is
      Floor, Ceiling : Count_Type;
   begin
      if Container.Is_Empty then
         return No_Element;
      end if;

      Search (Container.Backend.Query.Data.Nodes, Key, Floor, Ceiling);

      if Floor = Ceiling then
         return (Is_Empty => False,
            Backend => Container.Backend,
            Index => Floor);
      else
         return No_Element;
      end if;
   end Find;


   function First (Container : Constant_Map) return Cursor is
   begin
      if Container.Is_Empty then
         return No_Element;
      else
         return (Is_Empty => False,
            Backend => Container.Backend,
            Index => 1);
      end if;
   end First;


   function First_Element (Container : Constant_Map) return Element_Type is
      Accessor : constant Backend_Refs.Accessor := Container.Backend.Query;
   begin
      return Accessor.Data.Nodes (1).Element.all;
   end First_Element;


   function First_Key (Container : Constant_Map) return Key_Type is
      Accessor : constant Backend_Refs.Accessor := Container.Backend.Query;
   begin
      return Accessor.Data.Nodes (1).Key.all;
   end First_Key;


   function Floor (Container : Constant_Map; Key : Key_Type) return Cursor is
      Floor, Ceiling : Count_Type;
   begin
      if Container.Is_Empty then
         return No_Element;
      end if;

      Search (Container.Backend.Query.Data.Nodes, Key, Floor, Ceiling);

      if Floor > 0 then
         return (Is_Empty => False,
            Backend => Container.Backend,
            Index => Floor);
      else
         return No_Element;
      end if;
   end Floor;


   procedure Iterate
     (Container : in Constant_Map;
      Process : not null access procedure (Position : in Cursor))
   is
      Position : Cursor :=
        (Is_Empty => False,
         Backend => Container.Backend,
         Index => 1);
   begin
      if Container.Backend.Is_Empty then
         return;
      end if;

      for I in Container.Backend.Query.Data.Nodes'Range loop
         Position.Index := I;
         Process.all (Position);
      end loop;
   end Iterate;


   function Last (Container : Constant_Map) return Cursor is
   begin
      if Container.Is_Empty then
         return No_Element;
      else
         return (Is_Empty => False,
            Backend => Container.Backend,
            Index => Container.Backend.Query.Data.Size);
      end if;
   end Last;


   function Last_Element (Container : Constant_Map) return Element_Type is
      Accessor : constant Backend_Refs.Accessor := Container.Backend.Query;
   begin
      return Accessor.Data.Nodes (Accessor.Data.Size).Element.all;
   end Last_Element;


   function Last_Key (Container : Constant_Map) return Key_Type is
      Accessor : constant Backend_Refs.Accessor := Container.Backend.Query;
   begin
      return Accessor.Data.Nodes (Accessor.Data.Size).Key.all;
   end Last_Key;


   function Length (Container : Constant_Map)
     return Ada.Containers.Count_Type is
   begin
      if Container.Backend.Is_Empty then
         return 0;
      else
         return Container.Backend.Query.Data.Size;
      end if;
   end Length;


   procedure Move (Target, Source : in out Constant_Map) is
   begin
      Target.Backend := Source.Backend;
      Source.Backend.Reset;
   end Move;


   procedure Reverse_Iterate
     (Container : in Constant_Map;
      Process : not null access procedure (Position : in Cursor))
   is
      Position : Cursor :=
        (Is_Empty => False,
         Backend => Container.Backend,
         Index => 1);
   begin
      if Container.Backend.Is_Empty then
         return;
      end if;

      for I in reverse Container.Backend.Query.Data.Nodes'Range loop
         Position.Index := I;
         Process.all (Position);
      end loop;
   end Reverse_Iterate;



   ------------------------------
   -- Updatable Map Operations --
   ------------------------------

   procedure Update_Element
     (Container : in out Updatable_Map;
      Position : in Cursor;
      Process : not null access procedure (Key : in Key_Type;
                                           Element : in out Element_Type))
   is
      Accessor : constant Backend_Refs.Accessor := Position.Backend.Query;
   begin
      Process.all
        (Accessor.Data.Nodes (Position.Index).Key.all,
         Accessor.Data.Nodes (Position.Index).Element.all);
   end Update_Element;

end Natools.Constant_Indefinite_Ordered_Maps;
