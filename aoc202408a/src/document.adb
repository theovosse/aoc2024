with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;

package body Document is

   procedure Add_Line (Line : Unbounded_String) is
   begin
      Line_Width := Line.Length;
      Matrix.Append (Line);
      Nr_Lines := @ + 1;
   end Add_Line;

   procedure Read is
   begin
      while not End_Of_File
      loop
         Add_Line (Ada.Text_IO.Unbounded_IO.Get_Line);
      end loop;
   end Read;

end Document;