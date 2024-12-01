with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Integer_Vectors;
with Integer_Vectors_Sorting;

procedure Aoc202401a is
   Col1, Col2 : Integer;
   List1, List2 : Integer_Vectors.Vector;

   procedure read_two_lists is
   begin
      while not End_Of_File loop
         Get (Col1);
         List1.Append (Col1);
         Get (Col2);
         List2.Append (Col2);
      end loop;
   end read_two_lists;

   function diff_sum_of_two_lists return Integer is
      I : Integer := 0;
      Elt1, Elt2 : Integer;
      DiffSum : Integer := 0;
   begin
      while I < Integer (List1.Length) loop
         Elt1 := List1.Element (Index => I);
         Elt2 := List2.Element (Index => I);
         DiffSum := DiffSum +
            (if Elt1 < Elt2 then Elt2 - Elt1 else Elt1 - Elt2);
         I := I + 1;
      end loop;
      return DiffSum;
   end diff_sum_of_two_lists;

begin
   read_two_lists;
   Integer_Vectors_Sorting.Sort (List1);
   Integer_Vectors_Sorting.Sort (List2);
   Put_Line (Integer'Image (diff_sum_of_two_lists));
end Aoc202401a;
