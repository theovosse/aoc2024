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

      --  Given a List, a valid Cursor and the value at the cursor, this
      --  function counts the number of subsequent elements with the same
      --  value, progresses the cursor to the first element with another
      --  value, and returns the count.
      function Progress_List_Across_Value
         (List : in out Integer_Vectors.Vector;
          Cur : in out Cursor;
          Elt : Integer) return Integer is
         N : Integer := 0;
      begin
         N := 1;
         Cur := Next (Cur);
         while Cur /= No_Element and then Element (Cur) = Elt loop
            Cur := Next (Cur);
            N := N + 1;
         end loop;
         return N;
      end Progress_List_Across_Value;

   begin
      while Cur1 /= No_Element and then Cur2 /= No_Element loop
         Elt1 := Element (Cur1);
         Elt2 := Element (Cur2);
         if Elt1 = Elt2 then
            N1 := Progress_List_Across_Value (List1, Cur1, Elt1);
            N2 := Progress_List_Across_Value (List2, Cur2, Elt2);
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
