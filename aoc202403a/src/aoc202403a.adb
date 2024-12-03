with Ada.Text_IO; use Ada.Text_IO;
with Parse_Mul; use Parse_Mul;

procedure Aoc202403a is
begin
   while not End_Of_File (Standard_Input) loop
      declare
         Ch : Character;
      begin
         Get_Immediate (Standard_Input, Ch);
         Process_Char (Ch);
      end;
   end loop;
   Put_Line (Integer'Image (Get_Result));
end Aoc202403a;
