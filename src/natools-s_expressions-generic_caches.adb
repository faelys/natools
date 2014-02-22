------------------------------------------------------------------------------
-- Copyright (c) 2013-2014, Natacha Porté                                   --
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

package body Natools.S_Expressions.Generic_Caches is

   --------------------
   -- Tree Interface --
   --------------------

   procedure Append
     (Exp : in out Tree;
      Kind : in Node_Kind;
      Data : in Atom_Access := null)
   is
      N : Node_Access;
   begin
      case Kind is
         when Atom_Node =>
            N := new Node'(Kind => Atom_Node,
              Parent | Next => null, Data => Data);
         when List_Node =>
            N := new Node'(Kind => List_Node, Parent | Next | Child => null);
      end case;

      if Exp.Root = null then
         pragma Assert (Exp.Last = null);
         Exp.Root := N;
      else
         pragma Assert (Exp.Last /= null);

         if Exp.Opening then
            pragma Assert (Exp.Last.Kind = List_Node);
            pragma Assert (Exp.Last.Child = null);
            Exp.Last.Child := N;
            N.Parent := Exp.Last;
         else
            pragma Assert (Exp.Last.Next = null);
            Exp.Last.Next := N;
            N.Parent := Exp.Last.Parent;
         end if;
      end if;

      Exp.Last := N;
      Exp.Opening := Kind = List_Node;
   end Append;


   procedure Close_List (Exp : in out Tree) is
   begin
      if Exp.Opening then
         Exp.Opening := False;
      elsif Exp.Last /= null and then Exp.Last.Parent /= null then
         Exp.Last := Exp.Last.Parent;
      end if;
   end Close_List;


   function Create_Tree return Tree is
   begin
      return Tree'(Ada.Finalization.Limited_Controlled
        with Root | Last => null, Opening => False);
   end Create_Tree;


   function Duplicate (Source : Tree) return Tree is
      function Dup_List (First, Parent : Node_Access) return Node_Access;
      function Dup_Node (N, Parent : Node_Access) return Node_Access;

      New_Last : Node_Access := null;

      function Dup_List (First, Parent : Node_Access) return Node_Access is
         Source : Node_Access := First;
         Result, Target : Node_Access;
      begin
         if First = null then
            return null;
         end if;
         Result := Dup_Node (First, Parent);
         Target := Result;
         loop
            Source := Source.Next;
            exit when Source = null;
            Target.Next := Dup_Node (Source, Parent);
            Target := Target.Next;
         end loop;
         return Result;
      end Dup_List;

      function Dup_Node (N, Parent : Node_Access) return Node_Access is
         Result : Node_Access;
      begin
         if N = null then
            return null;
         end if;

         case N.Kind is
            when Atom_Node =>
               Result := new Node'(Kind => Atom_Node,
                 Parent => Parent,
                 Next => null,
                 Data => new Atom'(N.Data.all));
            when List_Node =>
               Result := new Node'(Kind => List_Node,
                 Parent => Parent,
                 Next => null,
                 Child => null);
               Result.Child := Dup_List (N.Child, Result);
         end case;

         if N = Source.Last then
            New_Last := Result;
         end if;

         return Result;
      end Dup_Node;
   begin
      return Result : Tree do
         Result.Root := Dup_List (Source.Root, null);
         pragma Assert ((New_Last = null) = (Source.Last = null));
         Result.Last := New_Last;
         Result.Opening := Source.Opening;
      end return;
   end Duplicate;


   overriding procedure Finalize (Object : in out Tree) is
      procedure List_Free (First : in out Node_Access);

      procedure List_Free (First : in out Node_Access) is
         Next : Node_Access := First;
         Cur : Node_Access;
      begin
         while Next /= null loop
            Cur := Next;

            case Cur.Kind is
               when Atom_Node =>
                  Unchecked_Free (Cur.Data);
               when List_Node =>
                  List_Free (Cur.Child);
            end case;

            Next := Cur.Next;
            Unchecked_Free (Cur);
         end loop;

         First := null;
      end List_Free;
   begin
      List_Free (Object.Root);
      Object.Last := null;
      Object.Opening := False;
   end Finalize;



   -----------------------
   -- Writing Interface --
   -----------------------

   function Duplicate (Cache : Reference) return Reference is
      function Dup_Tree return Tree;

      function Dup_Tree return Tree is
      begin
         return Duplicate (Cache.Exp.Query.Data.all);
      end Dup_Tree;
   begin
      return Reference'(Exp => Trees.Create (Dup_Tree'Access));
   end Duplicate;



   -----------------------
   -- Printer Interface --
   -----------------------

   overriding procedure Open_List (Output : in out Reference) is
   begin
      Output.Exp.Update.Data.Append (List_Node);
   end Open_List;


   overriding procedure Append_Atom
     (Output : in out Reference; Data : in Atom) is
   begin
      if Output.Exp.Is_Empty then
         Output.Exp.Replace (Create_Tree'Access);
      end if;

      Output.Exp.Update.Data.Append (Atom_Node, new Atom'(Data));
   end Append_Atom;


   overriding procedure Close_List (Output : in out Reference) is
   begin
      Output.Exp.Update.Data.Close_List;
   end Close_List;



   -------------------------
   -- Reading Subprograms --
   -------------------------

   function First (Cache : Reference'Class) return Cursor is
      N : Node_Access;
   begin
      if Cache.Exp.Is_Empty then
         return Cursor'(others => <>);
      else
         N := Cache.Exp.Query.Data.Root;
         pragma Assert (N /= null);
         return Cursor'(Exp => Cache.Exp, Position => N,
           Opening => N.Kind = List_Node);
      end if;
   end First;



   --------------------------
   -- Descriptor Interface --
   --------------------------

   overriding function Current_Event (Object : in Cursor)
     return Events.Event is
   begin
      if Object.Position = null then
         return Events.End_Of_Input;
      end if;

      case Object.Position.Kind is
         when Atom_Node =>
            return Events.Add_Atom;
         when List_Node =>
            if Object.Opening then
               return Events.Open_List;
            else
               return Events.Close_List;
            end if;
      end case;
   end Current_Event;


   overriding function Current_Atom (Object : in Cursor) return Atom is
   begin
      if Object.Position = null or else Object.Position.Kind /= Atom_Node then
         raise Program_Error;
      end if;

      return Object.Position.Data.all;
   end Current_Atom;


   overriding function Current_Level (Object : in Cursor) return Natural is
      Result : Natural := 0;
      N : Node_Access := Object.Position;
   begin
      if Object.Position /= null
        and then Object.Position.Kind = List_Node
        and then Object.Opening
      then
         Result := Result + 1;
      end if;

      while N /= null loop
         Result := Result + 1;
         N := N.Parent;
      end loop;

      return Natural'Max (Result, 1) - 1;
   end Current_Level;


   overriding procedure Query_Atom
     (Object : in Cursor;
      Process : not null access procedure (Data : in Atom)) is
   begin
      if Object.Position = null or else Object.Position.Kind /= Atom_Node then
         raise Program_Error;
      end if;

      Process.all (Object.Position.Data.all);
   end Query_Atom;


   overriding procedure Read_Atom
     (Object : in Cursor;
      Data : out Atom;
      Length : out Count)
   is
      Transferred : Count;
   begin
      if Object.Position = null or else Object.Position.Kind /= Atom_Node then
         raise Program_Error;
      end if;

      Length := Object.Position.Data'Length;
      Transferred := Count'Min (Data'Length, Length);
      Data (Data'First .. Data'First + Transferred - 1)
        := Object.Position.Data (Object.Position.Data'First
             .. Object.Position.Data'First + Transferred - 1);
   end Read_Atom;


   overriding procedure Next
     (Object : in out Cursor;
      Event : out Events.Event) is
   begin
      if Object.Position = null then
         Event := Events.Error;
         return;
      end if;

      if Object.Opening then
         pragma Assert (Object.Position.Kind = List_Node);
         if Object.Position.Child = null then
            Object.Opening := False;
         else
            pragma Assert (Object.Position.Child.Parent = Object.Position);
            Object.Position := Object.Position.Child;
            Object.Opening := Object.Position.Kind = List_Node;
         end if;
      elsif Object.Position.Next /= null then
         pragma Assert (Object.Position.Next.Parent = Object.Position.Parent);
         Object.Position := Object.Position.Next;
         Object.Opening := Object.Position.Kind = List_Node;
      elsif Object.Position.Parent /= null then
         pragma Assert (Object.Position.Parent.Kind = List_Node);
         Object.Position := Object.Position.Parent;
         Object.Opening := False;
      else
         Object.Position := null;
      end if;

      Event := Object.Current_Event;
   end Next;

end Natools.S_Expressions.Generic_Caches;

