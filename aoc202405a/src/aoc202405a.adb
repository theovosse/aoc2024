with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Integer_Vectors;
with Ada.Strings.Fixed, Ada.Strings.Maps;
use Ada.Strings, Ada.Strings.Fixed, Ada.Strings.Maps;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Generic_Sort;

procedure Aoc202405a is

   package Int_Set is new Ada.Containers.Ordered_Sets (Element_Type => Integer);

   function "="(A, B : Int_Set.Set) return Boolean is
   begin
      return A.Length = B.Length and then (for all Element of A => B.Contains (Element));
   end "=";

   package Int_Map_To_Int_Set is new Ada.Containers.Ordered_Maps (Key_Type => Integer, Element_Type => Int_Set.Set);

   Partial_Ordering : Int_Map_To_Int_Set.Map;

   procedure Add_Constraint (Line : Unbounded_String) is
      Separator_Position : constant Natural := Index (Line, "|", 1);
      Before, After : Natural;
   begin
      Before := Natural'Value (Slice (Line, 1, Separator_Position - 1));
      After := Natural'Value (Slice (Line, Separator_Position + 1, Length (Line)));
      if Partial_Ordering.Contains (Before) then
         Partial_Ordering (Before).Insert (After);
      else
         declare
            New_Set : Int_Set.Set;
         begin
            New_Set.Insert (After);
            Partial_Ordering.Insert (Before, New_Set);
         end;
      end if;
   end Add_Constraint;

   procedure Constraint_Closure is
      Change : Boolean;
   begin
      loop
         Change := False;
         exit when not Change;
      end loop;
   end Constraint_Closure;

   function Split_Line (Line : String)
      return Integer_Vectors.Vector is
      List : Integer_Vectors.Vector;
      Pos : Natural := Line'First;
      Start, Finish : Natural;
      Separator : constant Character_Set := To_Set (',');
   begin
      while Pos <= Line'Last loop
         Find_Token (Line, Separator, Pos, Outside, Start, Finish);
         if Finish = 0 then
            --  Put_Line ("last = " & Line (Start .. Line'Last));
            List.Append (Integer'Value (Line (Start .. Line'Last)));
            exit;
         end if;
         --  Put_Line ("num = " & Line (Start .. Finish));
         List.Append (Integer'Value (Line (Start .. Finish)));
         Pos := Finish + 1;
      end loop;
      return List;
   end Split_Line;

   function Cannot_Precede (Before, After : Integer) return Boolean is
   begin
      return Partial_Ordering.Contains (After) and then
             Partial_Ordering (After).Contains (Before);
   end Cannot_Precede;

   function Check_Order (List : Integer_Vectors.Vector) return Boolean is
   begin
      for I in List.First_Index .. List.Last_Index loop
         for J in List.First_Index .. I - 1 loop
            if Cannot_Precede (List (J), List (I)) then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Check_Order;

   function Page_Number_Less_Than (A, B : Integer) return Boolean is
   begin
      return Partial_Ordering.Contains (A) and then
             Partial_Ordering (A).Contains (B);
   end Page_Number_Less_Than;

   function Fix_List (Page_Number_List : Integer_Vectors.Vector) return Integer_Vectors.Vector is
   begin
      return Page_Number_List;
   end Fix_List;

   procedure Page_Number_Swap (I, J : Natural) is
      Tmp : constant Integer := Page_Number_List (I);
   begin
      Page_Number_List (I) := Page_Number_List (J);
      Page_Number_List (J) := Tmp;
   end Page_Number_Swap;

   function Index_Of_Smallest_Value (L : Integer_Vectors.Vector; I, Last_Index : Natural) return Natural is
      Index : Natural := I;
      Minimum : Integer : L (I);
   begin
      for J in I + 1 .. Last_Index loop
         if 
      end loop;
      return Index;
   end Index_Of_Smallest_Value;

   procedure Sort_Page_Number_List (L : Integer_Vectors.Vector) is
      Last_Index : constant Natural := Length (L) - 1;
   begin
      for I in 0 .. Last_Index - 1 loop
         Page_Number_Swap (I, Index_Of_Smallest_Value (L, I, Last_Index));
      end loop;
   end Sort_Page_Number_List;

   Sum_Of_Correct : Integer := 0;
   Sum_Of_Fixed : Integer := 0;

begin
   --  Read constraints until empty line
   while not End_Of_File loop
      declare
         Line : constant Unbounded_String := Ada.Text_IO.Unbounded_IO.Get_Line;
      begin
         exit when Length (Line) = 0;
         Add_Constraint (Line);
      end;
   end loop;
   Constraint_Closure;
   --  Process the rest of the input as a comma separated numbers
   while not End_Of_File loop
      declare
         Line : constant Unbounded_String := Ada.Text_IO.Unbounded_IO.Get_Line;
         Page_Number_List : Integer_Vectors.Vector := Split_Line (To_String (Line));
         procedure Printlist (Msg : String) is
         begin
            Put (Msg & ": ");
            for P of Page_Number_List loop
               Put (Integer'Image (P));
            end loop;
            Put_Line ("");
         end Printlist;
      begin
         if Check_Order (Page_Number_List) then
            Sum_Of_Correct := @ + Page_Number_List (Integer (Page_Number_List.Length) / 2);
         else
            Printlist ("Wrong");
            Sort_Page_Number_List (Page_Number_List);
            Printlist ("Sorted");
            Sum_Of_Fixed := @ + Page_Number_List (Integer (Page_Number_List.Length) / 2);
         end if;
      end;
   end loop;
   Put_Line ("Correct:" & Integer'Image (Sum_Of_Correct));
   Put_Line ("Fixed:" & Integer'Image (Sum_Of_Fixed));
end Aoc202405a;
