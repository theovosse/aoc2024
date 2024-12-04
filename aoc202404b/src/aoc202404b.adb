with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with String_Vectors;

procedure Aoc202404b is

   PatLen : constant Integer := 3;
   Document : String_Vectors.Vector;
   Line_Width : Integer := 0;
   Total_Count : Natural := 0;

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

   function Scan_Document return Natural is
      Max_I : constant Integer := Line_Width - PatLen;
      Max_J : constant Integer := Line_Width - (PatLen - 1);
      Count : Natural := 0;

      --  I, J is the top-left corner
      function Match (I, J : Integer) return Boolean is
      begin
         return Element (Document (I + 1), J + 1) = 'A' and then
                ((Element (Document (I),     J)     = 'M' and then
                  Element (Document (I + 2), J + 2) = 'S') or else
                 (Element (Document (I),     J)     = 'S' and then
                  Element (Document (I + 2), J + 2) = 'M')) and then
                ((Element (Document (I),     J + 2) = 'M' and then
                  Element (Document (I + 2), J)     = 'S') or else
                 (Element (Document (I),     J + 2) = 'S' and then
                  Element (Document (I + 2), J)     = 'M'));
      end Match;
   begin
      for I in 0 .. Max_I loop
         for J in 1 .. Max_J loop
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
   Total_Count := Scan_Document;
   Put_Line (Integer'Image (Total_Count));
end Aoc202404b;
