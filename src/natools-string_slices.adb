------------------------------------------------------------------------------
-- Copyright (c) 2013-2017, Natacha Porté                                   --
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

package body Natools.String_Slices is

   use type String_Refs.Immutable_Reference;


   -----------------------------
   -- String_Range primitives --
   -----------------------------

   function Is_In (Point : Natural; Reference : String_Range) return Boolean is
   begin
      return Point >= Reference.First
        and Point < Reference.First + Reference.Length;
   end Is_In;


   function Is_Subrange (Sample, Reference : String_Range) return Boolean is
   begin
      return Sample.Length = 0
        or else (Sample.First >= Reference.First
                 and then Sample.First + Sample.Length
                            <= Reference.First + Reference.Length);
   end Is_Subrange;


   function Last (Self : String_Range) return Natural is
   begin
      return Self.First + Self.Length - 1;
   end Last;


   function To_Range (First : Positive; Last : Natural) return String_Range is
   begin
      if Last >= First then
         return (First => First, Length => Last - First + 1);
      else
         return (First => First, Length => 0);
      end if;
   end To_Range;


   function Get_Range (S : String) return String_Range is
   begin
      return (S'First, S'Length);
   end Get_Range;


   procedure Set_First (Self : in out String_Range; New_First : in Positive) is
   begin
      if New_First >= Self.First + Self.Length then
         Self.Length := 0;
      else
         Self.Length := Self.Length - (New_First - Self.First);
      end if;

      Self.First := New_First;
   end Set_First;


   procedure Set_Last (Self : in out String_Range; New_Last : in Natural) is
   begin
      if New_Last < Self.First then
         Self.Length := 0;
      else
         Self.Length := New_Last - Self.First + 1;
      end if;
   end Set_Last;


   procedure Set_Length
     (Self : in out String_Range; New_Length : in Natural) is
   begin
      Self.Length := New_Length;
   end Set_Length;


   function Image (Interval : String_Range) return String is
      First_Img : String := Integer'Image (Interval.First);
   begin
      pragma Assert (First_Img (First_Img'First) = ' ');

      if Interval.Length = 0 then
         return "empty at" & First_Img;
      end if;

      First_Img (First_Img'First) := '[';

      if Interval.Length = 1 then
         return First_Img & ']';
      else
         return First_Img
           & ','
           & Integer'Image (Last (Interval))
           & ']';
      end if;
   end Image;



   --------------------------
   -- Conversion functions --
   --------------------------

   function New_Slice
     (First : Positive;
      Last : Natural;
      Initialize : not null access procedure (S : out String))
     return Slice
   is
      Data : constant  String_Refs.Data_Access := new String (First .. Last);
      Ref : constant String_Refs.Immutable_Reference
        := String_Refs.Create (Data);
   begin
      Initialize (Data.all);
      return Slice'(Bounds => (First, Last + 1 - First), Ref => Ref);
   end New_Slice;


   function To_Slice (S : String) return Slice is
      function Create return String;

      function Create return String is
      begin
         return S;
      end Create;
   begin
      return Slice'(Bounds => (S'First, S'Length),
                    Ref    => String_Refs.Create (Create'Access));
   end To_Slice;


   function To_String (S : Slice) return String is
   begin
      if S.Ref.Is_Empty then
         return "";
      else
         return S.Ref.Query.Data.all (S.Bounds.First .. Last (S.Bounds));
      end if;
   end To_String;



   ---------------
   -- Accessors --
   ---------------

   procedure Export (S : in Slice; Output : out String) is
   begin
      if not S.Ref.Is_Empty then
         Output := S.Ref.Query.Data.all (S.Bounds.First .. Last (S.Bounds));
      end if;
   end Export;


   procedure Query
     (S : in Slice;
      Process : not null access procedure (Text : in String)) is
   begin
      if S.Bounds.Length = 0 or else S.Ref.Is_Empty then
         Process.all ("");
      else
         Process.all
           (S.Ref.Query.Data.all (S.Bounds.First .. Last (S.Bounds)));
      end if;
   end Query;


   function Get_Range (S : Slice) return String_Range is
   begin
      return S.Bounds;
   end Get_Range;


   function First (S : Slice) return Positive is
   begin
      return S.Bounds.First;
   end First;


   function Last (S : Slice) return Natural is
   begin
      return Last (S.Bounds);
   end Last;


   function Length (S : Slice) return Natural is
   begin
      return S.Bounds.Length;
   end Length;



   ---------------
   -- Extenders --
   ---------------

   function Parent (S : Slice) return Slice is
   begin
      if S.Ref.Is_Empty then
         return Slice'(others => <>);
      else
         return Slice'(Bounds => Get_Range (S.Ref.Query.Data.all),
                       Ref    => S.Ref);
      end if;
   end Parent;


   function Extend (S : Slice; New_Range : in String_Range) return Slice is
   begin
      if not Is_Subrange (New_Range, Get_Range (S.Ref.Query.Data.all)) then
         raise Constraint_Error with "Extend slice beyond complete range";
      end if;

      return Slice'(Bounds => New_Range,
                    Ref    => S.Ref);
   end Extend;


   function Extend (S : Slice; First : Positive; Last : Natural)
     return Slice is
   begin
      return Extend (S, To_Range (First, Last));
   end Extend;


   procedure Extend (S : in out Slice; New_Range : in String_Range) is
   begin
      if not Is_Subrange (New_Range, Get_Range (S.Ref.Query.Data.all)) then
         raise Constraint_Error with "Extend slice beyond complete range";
      end if;

      S.Bounds := New_Range;
   end Extend;


   procedure Extend
     (S : in out Slice; First : in Positive; Last : in Natural) is
   begin
      Extend (S, To_Range (First, Last));
   end Extend;



   -----------------
   -- Restrictors --
   -----------------

   function Subslice (S : Slice; New_Range : String_Range) return Slice is
   begin
      if S.Ref.Is_Empty then
         if New_Range.Length = 0 then
            return Slice'(Bounds => New_Range, Ref => <>);
         else
            raise Constraint_Error with "Subslice of null slice";
         end if;
      end if;

      if not Is_Subrange (New_Range, S.Bounds) then
         raise Constraint_Error with "Subslice out of parent range";
      end if;

      return Slice'(Bounds => New_Range,
                    Ref    => S.Ref);
   end Subslice;


   function Subslice (S : Slice; First : Positive; Last : Natural)
     return Slice is
   begin
      return Subslice (S, To_Range (First, Last));
   end Subslice;


   procedure Restrict (S : in out Slice; New_Range : in String_Range) is
   begin
      if S.Ref.Is_Empty and New_Range.Length /= 0 then
         raise Constraint_Error with "Restrict of null slice";
      end if;

      if not Is_Subrange (New_Range, S.Bounds) then
         raise Constraint_Error with "Restriction with not a subrange";
      end if;

      S.Bounds := New_Range;
   end Restrict;


   procedure Restrict
     (S : in out Slice; First : in Positive; Last : in Natural) is
   begin
      Restrict (S, To_Range (First, Last));
   end Restrict;


   procedure Set_First (S : in out Slice; New_First : in Positive) is
   begin
      if New_First < S.Bounds.First then
         raise Constraint_Error with "New_First out of slice range";
      end if;

      Set_First (S.Bounds, New_First);
   end Set_First;


   procedure Set_Last (S : in out Slice; New_Last : in Natural) is
   begin
      if New_Last > Last (S.Bounds) then
         raise Constraint_Error with "New_Last out of slice range";
      end if;

      Set_Last (S.Bounds, New_Last);
   end Set_Last;


   procedure Set_Length (S : in out Slice; New_Length : in Natural) is
   begin
      if New_Length > S.Bounds.Length then
         raise Constraint_Error with "New_Length out of slice range";
      end if;

      S.Bounds.Length := New_Length;
   end Set_Length;



   ----------------------
   -- Slice comparison --
   ----------------------

   function Is_Empty (S : Slice) return Boolean is
   begin
      return S.Bounds.Length = 0 or else S.Ref.Is_Empty;
   end Is_Empty;


   function Is_Null (S : Slice) return Boolean is
   begin
      return S.Ref.Is_Empty;
   end Is_Null;


   function Is_Related (Left, Right : Slice) return Boolean is
   begin
      return Left.Ref = Right.Ref;
   end Is_Related;


   function Is_Subslice (S, Reference : Slice) return Boolean is
   begin
      return S.Ref = Reference.Ref
        and then Is_Subrange (S.Bounds, Reference.Bounds);
   end Is_Subslice;



   ------------------
   -- Constructors --
   ------------------

   function Duplicate (S : Slice) return Slice is
      function Factory return String;

      function Factory return String is
      begin
         return S.Ref.Query.Data.all;
      end Factory;
   begin
      if S.Bounds.Length = 0 or else S.Ref.Is_Empty then
         return Null_Slice;
      else
         return Slice'(Bounds => S.Bounds,
                       Ref => String_Refs.Create (Factory'Access));
      end if;
   end Duplicate;

end Natools.String_Slices;
