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
-- Natools.Chunked_Strings is a string container designed for large amount  --
-- of data and efficient accumulation (append). Most subprograms are the    --
-- direct copy of Unbounded_String equivalent from LRM, with the same       --
-- semantics.                                                               --
--                                                                          --
-- The implementation uses fixed-size "chunks" of memory, so that large     --
-- strings do not have to be stored in a contiguous space. This also allows --
-- more efficient appends, since only the last chunk might need dynamic     --
-- resize.                                                                  --
-- Moreover the last chunk is constrained to have a size multiple of        --
-- Allocation_Unit, so if Allocation_Unit = Chunk_Size, no string resize    --
-- ever happen.                                                             --
--                                                                          --
-- The list of chunks is stored as a usual dynamic array, so append         --
-- operations are still linear (when a new chunk has to be created), they   --
-- are just O(Size / Chunk_Size) instead of O(Size). For suitable values of --
-- Chunk_Size, that should be a significant improuvement.                   --
--                                                                          --
-- Chunk_Size and Allocation_Unit are defined per Chunked_String, which     --
-- allows to use suitable parameters depending on the expected string size. --
-- Generic parameters control the default values, e.g. in operations like   --
-- "&" which don't allow to specify them.                                   --
------------------------------------------------------------------------------

with Ada.Strings.Maps;
with Natools.Accumulators;

private with Ada.Finalization;

generic
   Default_Allocation_Unit : Positive := 64;
   Default_Chunk_Size : Positive := 4096;

package Natools.Chunked_Strings is
   pragma Preelaborate (Chunked_Strings);

   package Maps renames Ada.Strings.Maps;

   type Chunked_String is new Natools.Accumulators.String_Accumulator
      with private;

   function Build (Depth : Positive)
      return Natools.Accumulators.String_Accumulator'Class;
      --  Returns a new empty chunked string
      --  Ignores its Depth argument
      --  Can be used with Natools.Accumulators.String_Accumulator_Linked_Lists

   function Duplicate (Source : in Chunked_String) return Chunked_String;
      --  returns a copy of the given chunked string

   procedure Free_Extra_Memory (From : in out Chunked_String);
      --  Release as much memory as possible without altering the contents

   procedure Hard_Reset (Str : in out Chunked_String);
      --  Empty the string and free all possible memory

   procedure Preallocate (Str : in out Chunked_String; Size : Natural);
      --  Allocate enough memory to reach Size without subsequent reallocation

   procedure Soft_Reset (Str : in out Chunked_String);
      --  Empty the string for reuse

   procedure To_String (Source : Chunked_String; Output : out String);
      --  Write the contents of the chunked string into the output string,
      --    which must be large enough.


   -------------------------------------------
   -- String_Accumulator specific interface --
   -------------------------------------------

   --  Append, Length and To_String are part of the standard interface
   --  Hard_Reset and Soft_Reset are already in the specific interface

   function Tail (Source : in Chunked_String; Size : in Natural) return String;

   procedure Unappend (From : in out Chunked_String; Text : in String);


   ------------------------
   -- Standard interface --
   ------------------------

   --  All the following declarations are copied from Unbounded_String
   --  interface and have exactly the same semantics.
   --  Subprogram that create new Chunked_String objects also have
   --  Chunk_Size and Allocation_Unit optional parameters.

   Null_Chunked_String : constant Chunked_String;

   function Length (Source : in Chunked_String) return Natural;

   type String_Access is access all String;
   procedure Free (X : in out String_Access);

   --  Conversion, Concatenation, and Selection functions

   function To_Chunked_String
     (Source          : in String;
      Chunk_Size      : in Positive := Default_Chunk_Size;
      Allocation_Unit : in Positive := Default_Allocation_Unit)
      return Chunked_String;

   function To_Chunked_String
     (Length          : in Natural;
      Chunk_Size      : in Positive := Default_Chunk_Size;
      Allocation_Unit : in Positive := Default_Allocation_Unit)
      return Chunked_String;

   function To_String (Source : in Chunked_String) return String;

   procedure Set_Chunked_String
     (Target          :    out Chunked_String;
      Source          : in     String;
      Chunk_Size      : in     Positive := Default_Chunk_Size;
      Allocation_Unit : in     Positive := Default_Allocation_Unit);

   procedure Append (Source   : in out Chunked_String;
                     New_Item : in Chunked_String);

   procedure Append (Source   : in out Chunked_String;
                     New_Item : in String);

   procedure Append (Source   : in out Chunked_String;
                     New_Item : in Character);

   function "&" (Left, Right : in Chunked_String)
      return Chunked_String;

   function "&" (Left : in Chunked_String; Right : in String)
      return Chunked_String;

   function "&" (Left : in String; Right : in Chunked_String)
      return Chunked_String;

   function "&" (Left : in Chunked_String; Right : in Character)
      return Chunked_String;

   function "&" (Left : in Character; Right : in Chunked_String)
      return Chunked_String;

   function Element (Source : in Chunked_String;
                     Index  : in Positive)
      return Character;
   pragma Inline (Element);

   procedure Replace_Element (Source : in out Chunked_String;
                              Index  : in Positive;
                              By     : in Character);

   function Slice (Source : in Chunked_String;
                   Low    : in Positive;
                   High   : in Natural)
      return String;

   function Chunked_Slice
     (Source          : in Chunked_String;
      Low             : in Positive;
      High            : in Natural;
      Chunk_Size      : in Positive := Default_Chunk_Size;
      Allocation_Unit : in Positive := Default_Allocation_Unit)
      return Chunked_String;

   procedure Chunked_Slice
     (Source          : in     Chunked_String;
      Target          :    out Chunked_String;
      Low             : in     Positive;
      High            : in     Natural;
      Chunk_Size      : in     Positive := Default_Chunk_Size;
      Allocation_Unit : in     Positive := Default_Allocation_Unit);

   function "=" (Left, Right : in Chunked_String) return Boolean;

   function "=" (Left : in Chunked_String; Right : in String)
      return Boolean;

   function "=" (Left : in String; Right : in Chunked_String)
      return Boolean;

   function "<" (Left, Right : in Chunked_String) return Boolean;

   function "<" (Left : in Chunked_String; Right : in String)
      return Boolean;

   function "<" (Left : in String; Right : in Chunked_String)
      return Boolean;

   function "<=" (Left, Right : in Chunked_String) return Boolean;

   function "<=" (Left : in Chunked_String; Right : in String)
      return Boolean;

   function "<=" (Left : in String; Right : in Chunked_String)
      return Boolean;

   function ">" (Left, Right : in Chunked_String) return Boolean;

   function ">" (Left : in Chunked_String; Right : in String)
      return Boolean;

   function ">" (Left : in String; Right : in Chunked_String)
      return Boolean;

   function ">=" (Left, Right : in Chunked_String) return Boolean;

   function ">=" (Left : in Chunked_String; Right : in String)
      return Boolean;

   function ">=" (Left : in String; Right : in Chunked_String)
      return Boolean;

   function Index (Source  : in Chunked_String;
                   Pattern : in String;
                   From    : in Positive;
                   Going   : in Ada.Strings.Direction := Ada.Strings.Forward;
                   Mapping : in Maps.Character_Mapping := Maps.Identity)
      return Natural;

   function Index (Source  : in Chunked_String;
                   Pattern : in String;
                   From    : in Positive;
                   Going   : in Ada.Strings.Direction := Ada.Strings.Forward;
                   Mapping : in Maps.Character_Mapping_Function)
      return Natural;

   function Index (Source  : in Chunked_String;
                   Pattern : in String;
                   Going   : in Ada.Strings.Direction := Ada.Strings.Forward;
                   Mapping : in Maps.Character_Mapping := Maps.Identity)
      return Natural;

   function Index (Source  : in Chunked_String;
                   Pattern : in String;
                   Going   : in Ada.Strings.Direction := Ada.Strings.Forward;
                   Mapping : in Maps.Character_Mapping_Function)
      return Natural;

   function Index (Source : in Chunked_String;
                   Set    : in Maps.Character_Set;
                   From   : in Positive;
                   Test   : in Ada.Strings.Membership := Ada.Strings.Inside;
                   Going  : in Ada.Strings.Direction := Ada.Strings.Forward)
      return Natural;

   function Index (Source : in Chunked_String;
                   Set    : in Maps.Character_Set;
                   Test   : in Ada.Strings.Membership := Ada.Strings.Inside;
                   Going  : in Ada.Strings.Direction := Ada.Strings.Forward)
      return Natural;

   function Index_Non_Blank (Source : in Chunked_String;
                             From   : in Positive;
                             Going  : in Ada.Strings.Direction
                                         := Ada.Strings.Forward)
      return Natural;

   function Index_Non_Blank (Source : in Chunked_String;
                             Going  : in Ada.Strings.Direction
                                         := Ada.Strings.Forward)
      return Natural;

   function Count (Source  : in Chunked_String;
                   Pattern : in String;
                   Mapping : in Maps.Character_Mapping := Maps.Identity)
      return Natural;

   function Count (Source  : in Chunked_String;
                   Pattern : in String;
                   Mapping : in Maps.Character_Mapping_Function)
      return Natural;

   function Count (Source  : in Chunked_String;
                   Set     : in Maps.Character_Set)
      return Natural;

   procedure Find_Token (Source : in     Chunked_String;
                         Set    : in     Maps.Character_Set;
                         Test   : in     Ada.Strings.Membership;
                         First  :    out Positive;
                         Last   :    out Natural);

   --  String translation subprograms

   function Translate (Source  : in Chunked_String;
                       Mapping : in Maps.Character_Mapping)
      return Chunked_String;

   procedure Translate (Source  : in out Chunked_String;
                        Mapping : in     Maps.Character_Mapping);

   function Translate (Source  : in Chunked_String;
                       Mapping : in Maps.Character_Mapping_Function)
      return Chunked_String;

   procedure Translate (Source  : in out Chunked_String;
                        Mapping : in     Maps.Character_Mapping_Function);

   --  String transformation subprograms

   function Replace_Slice (Source : in Chunked_String;
                           Low    : in Positive;
                           High   : in Natural;
                           By     : in String)
      return Chunked_String;

   procedure Replace_Slice (Source : in out Chunked_String;
                            Low    : in     Positive;
                            High   : in     Natural;
                            By     : in     String);

   function Insert (Source   : in Chunked_String;
                    Before   : in Positive;
                    New_Item : in String)
      return Chunked_String;

   procedure Insert (Source   : in out Chunked_String;
                     Before   : in     Positive;
                     New_Item : in     String);

   function Overwrite (Source   : in Chunked_String;
                       Position : in Positive;
                       New_Item : in String)
      return Chunked_String;

   procedure Overwrite (Source   : in out Chunked_String;
                        Position : in     Positive;
                        New_Item : in     String);

   function Delete (Source  : in Chunked_String;
                    From    : in Positive;
                    Through : in Natural)
      return Chunked_String;

   procedure Delete (Source  : in out Chunked_String;
                     From    : in     Positive;
                     Through : in     Natural);

   function Trim (Source : in Chunked_String;
                  Side   : in Ada.Strings.Trim_End)
      return Chunked_String;

   procedure Trim (Source : in out Chunked_String;
                   Side   : in     Ada.Strings.Trim_End);

   function Trim (Source : in Chunked_String;
                  Left   : in Maps.Character_Set;
                  Right  : in Maps.Character_Set)
      return Chunked_String;

   procedure Trim (Source : in out Chunked_String;
                   Left   : in     Maps.Character_Set;
                   Right  : in     Maps.Character_Set);

   function Head (Source          : in Chunked_String;
                  Count           : in Natural;
                  Pad             : in Character := Ada.Strings.Space;
                  Chunk_Size      : in Natural := 0; -- use value from Source
                  Allocation_Unit : in Natural := 0) -- use value from Source
      return Chunked_String;

   procedure Head (Source : in out Chunked_String;
                   Count  : in     Natural;
                   Pad    : in     Character := Ada.Strings.Space);

   function Tail (Source          : in Chunked_String;
                  Count           : in Natural;
                  Pad             : in Character := Ada.Strings.Space;
                  Chunk_Size      : in Natural := 0; -- use value from Source
                  Allocation_Unit : in Natural := 0) -- use value from Source
      return Chunked_String;

   procedure Tail (Source : in out Chunked_String;
                   Count  : in     Natural;
                   Pad    : in     Character := Ada.Strings.Space);

   function "*" (Left  : in Natural;
                 Right : in Character)
      return Chunked_String;

   function "*" (Left  : in Natural;
                 Right : in String)
      return Chunked_String;

   function "*" (Left  : in Natural;
                 Right : in Chunked_String)
      return Chunked_String;

private
   type Chunk_Array is array (Positive range <>) of String_Access;
   type Chunk_Array_Access is access all Chunk_Array;

   type Chunked_String is new Ada.Finalization.Controlled
                          and Natools.Accumulators.String_Accumulator
   with record
      Chunk_Size      : Positive := Default_Chunk_Size;
      Allocation_Unit : Positive := Default_Allocation_Unit;
      Size            : Natural := 0;
      Data            : Chunk_Array_Access := null;
   end record;

   overriding procedure Initialize (Object : in out Chunked_String);
   overriding procedure Adjust     (Object : in out Chunked_String);
   overriding procedure Finalize   (Object : in out Chunked_String);
      --  Controlled type methods

   function Is_Valid (Source : in Chunked_String) return Boolean;
      --  Internal consistency checks

   Null_Chunked_String : constant Chunked_String :=
     (Ada.Finalization.Controlled with
      Chunk_Size      => Default_Chunk_Size,
      Allocation_Unit => Default_Allocation_Unit,
      Size            => 0,
      Data            => null);

end Natools.Chunked_Strings;
