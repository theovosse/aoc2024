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
            List.Append (Integer'Value (Line (Start .. Line'Last)));
            exit;
         end if;
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
   --  Process the rest of the input as a lists of page numbers
   while not End_Of_File loop
      declare
         Line : constant Unbounded_String := Ada.Text_IO.Unbounded_IO.Get_Line;
         Page_Number_List : Integer_Vectors.Vector := Split_Line (To_String (Line));
         function Page_Number_In_Order (I, J : Natural) return Boolean is
         begin
            return Page_Number_Less_Than (Page_Number_List (I), Page_Number_List (J));
         end Page_Number_In_Order;
         procedure Page_Number_Swap (I, J : Natural) is
            Tmp : constant Integer := Page_Number_List (I);
         begin
            Page_Number_List (I) := Page_Number_List (J);
            Page_Number_List (J) := Tmp;
         end Page_Number_Swap;
         procedure Sort_Page_Number_List is new Ada.Containers.Generic_Sort
            (Index_Type => Natural, Before => Page_Number_In_Order,
             Swap => Page_Number_Swap);
      begin
         if Check_Order (Page_Number_List) then
            Sum_Of_Correct := @ + Page_Number_List (Integer (Page_Number_List.Length) / 2);
         else
            Sort_Page_Number_List (0, Integer (Page_Number_List.Length) - 1);
            Sum_Of_Fixed := @ + Page_Number_List (Integer (Page_Number_List.Length) / 2);
         end if;
      end;
   end loop;
   Put_Line ("Correct:" & Integer'Image (Sum_Of_Correct));
   Put_Line ("Fixed:" & Integer'Image (Sum_Of_Fixed));
end Aoc202405a;
