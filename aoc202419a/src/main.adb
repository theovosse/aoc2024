pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;

procedure Main is

   --  Using a trie structure in fixed memory (checking array indices is easier than pointers)

   Max_Trie_Index : constant Natural := 1000;

   type Trie_Node_Index is new Natural range 0 .. Max_Trie_Index;

   Start_Trie_Index : constant Trie_Node_Index := 0;

   type Next_Trie_Level is array (Character range 'a' .. 'z') of Trie_Node_Index;

   type Trie_Node is record
      Terminal : Boolean;
      Next : Next_Trie_Level;
   end record;

   Trie : array (Trie_Node_Index) of Trie_Node;
   Nr_Trie_Nodes : Trie_Node_Index := 0;

   function Allocate_Trie_Node return Trie_Node_Index is
      Index : constant Trie_Node_Index := Nr_Trie_Nodes;
   begin
      Nr_Trie_Nodes := @ + 1;
      Trie (Index) := (False, [others => Start_Trie_Index]);
      return Index;
   end Allocate_Trie_Node;

   procedure Init_Trie is
      Index : constant Trie_Node_Index := Allocate_Trie_Node;
   begin
      pragma Unreferenced (Index);
   end Init_Trie;

   --  Insert `Alt` in the trie. It's possible to optimize the trie
   --  afterwards, but that's unnecessary for this problem set.
   procedure Add_Alternative (Alt : String) is
      Ptr : Trie_Node_Index := Start_Trie_Index;
      Ch : Character;
   begin
      for I in Alt'Range loop
         Ch := Alt (I);
         if Trie (Ptr).Next (Ch) = Start_Trie_Index then
            Trie (Ptr).Next (Ch) := Allocate_Trie_Node;
         end if;
         Ptr := Trie (Ptr).Next (Ch);
      end loop;
      Trie (Ptr).Terminal := True;
   end Add_Alternative;

   --  Split `Str` into parts, and add each one to the trie
   procedure Make_Pattern (Str : Unbounded_String; Sep : String) is
      Cur : Positive := 1;
      Pos : Natural;
   begin
      Init_Trie;
      loop
         Pos := Index (Str, Sep, Cur);
         exit when Pos = 0;
         Add_Alternative (Slice (Str, Cur, Pos - 1));
         Cur := Pos + Sep'Length;
      end loop;
      Add_Alternative (Slice (Str, Cur, Length (Str)));
   end Make_Pattern;

   package Index_Map is new Ada.Containers.Ordered_Maps (Trie_Node_Index, Long_Integer);
   use Index_Map;

   type State_Map_Index is new Natural range 0 .. 1;

   --  Starting at the trie root, expand the set of all current positions in the
   --  trie for each next character in `Str`. A `Terminal` node allows starting
   --  again at the root. The string is accepted when the final set contains at
   --  least one `Terminal` node.
   --  The implementation switches between two sets of positions in the trie, to
   --  avoid copying the previous set to the new set.
   function Matches (Str : Unbounded_String) return Long_Integer is
      States : array (State_Map_Index) of Index_Map.Map;
      Current_State_Map : State_Map_Index := 0;
      Ch : Character;
      Next_State : Trie_Node_Index;
      Count : Long_Integer;
      Total : Long_Integer := 0;
   begin
      States (Current_State_Map).Insert (Start_Trie_Index, 1);
      for I in 1 .. Length (Str) loop
         Ch := Element (Str, I);
         Current_State_Map := 1 - @;
         States (Current_State_Map) := Empty_Map;
         for Current_State in States (1 - Current_State_Map).Iterate loop
            Next_State := Trie (Key (Current_State)).Next (Ch);
            Count := Element (Current_State);
            if Next_State /= Start_Trie_Index then
               if States (Current_State_Map).Contains (Next_State) then
                  States (Current_State_Map).Replace (Next_State, Element (States (Current_State_Map), Next_State) + Count);
               else
                  States (Current_State_Map).Include (Next_State, Count);
               end if;
               if Trie (Next_State).Terminal then
                  --  Restart when the next state
                  if States (Current_State_Map).Contains (Start_Trie_Index)
                  then
                     Replace (States (Current_State_Map), Start_Trie_Index, Element (States (Current_State_Map), Start_Trie_Index) + Count);
                  else
                     States (Current_State_Map).Include (Start_Trie_Index, Count);
                  end if;
               end if;
            end if;
         end loop;
      end loop;
      for State in States (Current_State_Map).Iterate loop
         if Trie (Key (State)).Terminal then
            Total := @ + Element (State);
         end if;
      end loop;
      return Total;
   end Matches;

   procedure Read_Document is
      Line : Unbounded_String;
      Nr_Possible : Long_Integer := 0;
      Nr_Matches : Long_Integer;
      Total_Nr_Matches : Long_Integer := 0;
   begin
      Line := Get_Line;
      Make_Pattern (Line, ", ");
      Line := Get_Line;
      while not End_Of_File loop
         Line := Get_Line;
         Nr_Matches := Matches (Line);
         if Nr_Matches > 0 then
            Nr_Possible := @ + 1;
            Total_Nr_Matches := @ + Nr_Matches;
         end if;
      end loop;
      Put_Line (Nr_Possible'Image);
      Put_Line (Total_Nr_Matches'Image);
   end Read_Document;

begin
   Read_Document;
end Main;
