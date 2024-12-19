pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;

procedure Main is

   Max_Trie_Index : constant Natural := 1000;

   type Trie_Node;

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

   package Index_Set is new Ada.Containers.Ordered_Sets (Trie_Node_Index);

   type State_Set_Index is new Natural range 0 .. 1;

   function Matches (Str : Unbounded_String) return Boolean is
      States : array (State_Set_Index) of Index_Set.Set;
      Current_State_Set : State_Set_Index := 0;
      Ch : Character;
      Next_State : Trie_Node_Index;
   begin
      States (Current_State_Set).Insert (Start_Trie_Index);
      for I in 1 .. Length (Str) loop
         Ch := Element (Str, I);
         Current_State_Set := 1 - @;
         States (Current_State_Set) := Index_Set.Empty_Set;
         for Current_State of States (1 - Current_State_Set) loop
            Next_State := Trie (Current_State).Next (Ch);
            if Next_State /= Start_Trie_Index then
               States (Current_State_Set).Include (Next_State);
               if Trie (Next_State).Terminal and then not States (Current_State_Set).Contains (Start_Trie_Index) then
                  --  Restart when the next state
                  States (Current_State_Set).Include (Start_Trie_Index);
               end if;
            end if;
         end loop;
      end loop;
      return (for some Index of States (Current_State_Set) => Trie (Index).Terminal);
   end Matches;

   procedure Read_Document is
      Line : Unbounded_String;
      Nr_Possible : Natural := 0;
   begin
      Line := Get_Line;
      Make_Pattern (Line, ", ");
      Line := Get_Line;
      while not End_Of_File loop
         Line := Get_Line;
         if Matches (Line) then
            Nr_Possible := @ + 1;
         end if;
      end loop;
      Put_Line (Nr_Possible'Image);
   end Read_Document;

begin
   Read_Document;
end Main;
