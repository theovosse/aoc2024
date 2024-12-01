with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Integer_Vectors; use Integer_Vectors;
with Integer_Vectors_Sorting;

procedure Aoc202401b is
   Col1, Col2 : Integer;
   List1, List2 : Integer_Vectors.Vector;

   procedure Read_Two_Lists is
   begin
      while not End_Of_File loop
         Get (Col1);
         List1.Append (Col1);
         Get (Col2);
         List2.Append (Col2);
      end loop;
   end Read_Two_Lists;

   function Similarity_Of_Two_Lists return Integer is
      Cur1 : Cursor := List1.First;
      Cur2 : Cursor := List2.First;
      Elt1, Elt2 : Integer;
      N1, N2 : Integer;
      Similarity : Integer := 0;
   begin
      while Cur1 /= No_Element and then Cur2 /= No_Element loop
         Elt1 := Element (Cur1);
         Elt2 := Element (Cur2);
         if Elt1 = Elt2 then
            N1 := 1;
            Cur1 := Next (Cur1);
            while Cur1 /= No_Element and then Element (Cur1) = Elt1 loop
               Cur1 := Next (Cur1);
               N1 := N1 + 1;
            end loop;
            N2 := 1;
            Cur2 := Next (Cur2);
            while Cur2 /= No_Element and then Element (Cur2) = Elt2 loop
               Cur2 := Next (Cur2);
               N2 := N2 + 1;
            end loop;
            Similarity := Similarity + N1 * N2 * Elt2;
         elsif Elt1 < Elt2 then
            Cur1 := Next (Cur1);
         else
            Cur2 := Next (Cur2);
         end if;
      end loop;
      return Similarity;
   end Similarity_Of_Two_Lists;

   procedure Sort_Two_Lists is
   begin
      Integer_Vectors_Sorting.Sort (List1);
      Integer_Vectors_Sorting.Sort (List2);
   end Sort_Two_Lists;

begin
   Read_Two_Lists;
   Sort_Two_Lists;
   Put_Line (Integer'Image (Similarity_Of_Two_Lists));
end Aoc202401b;
