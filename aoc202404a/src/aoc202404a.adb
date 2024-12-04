with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with String_Vectors;

procedure Aoc202404a is

   PatLen : constant Integer := 4;
   Document : String_Vectors.Vector;
   Line_Width : Integer := 0;
   Total_Count : Natural := 0;

   function max (A, B : Integer) return Integer is
      (if A > B then A else B);

   procedure Read_Document is
   begin
      while not End_Of_File (Standard_Input) loop
         declare
            Line : constant Unbounded_String :=
                Ada.Text_IO.Unbounded_IO.Get_Line;
         begin
            Line_Width := Length (Line);
            Document.Append (Line);
         end;
      end loop;
      Put_Line ("Doc is" & Document.Length'Image & " x"
                         & Integer'Image (Line_Width));
   end Read_Document;

   function Scan_Document (Delta_I, Delta_J : Integer) return Natural is
      Min_I : constant Integer := max (0, (-Delta_I) * (PatLen - 1));
      Max_I : constant Integer := Line_Width - max (1, Delta_I * PatLen);
      Min_J : constant Integer := max (1, 1 - Delta_J * (PatLen - 1));
      Max_J : constant Integer := Line_Width - max (0, Delta_J * (PatLen - 1));
      Count : Natural := 0;

      function Match (I, J : Integer) return Boolean is
      begin
         return Element (Document (I), J) = 'X' and then
                Element (Document (I + Delta_I), J + Delta_J) = 'M' and then
                Element (Document (I + 2 * Delta_I), J + 2 * Delta_J) = 'A' and then
                Element (Document (I + 3 * Delta_I), J + 3 * Delta_J) = 'S';
      end Match;
   begin
      for I in Min_I .. Max_I loop
         for J in Min_J .. Max_J loop
            if Match (I, J)
            then
               Count := Count + 1;
            end if;
         end loop;
      end loop;
      return Count;
   end Scan_Document;

begin
   Read_Document;
   for Delta_I in -1 .. 1 loop
      for Delta_J in -1 .. 1 loop
         if Delta_I /= 0 or else Delta_J /= 0 then
            Total_Count := Total_Count + Scan_Document (Delta_I, Delta_J);
         end if;
      end loop;
   end loop;
   Put_Line (Integer'Image (Total_Count));
end Aoc202404a;
