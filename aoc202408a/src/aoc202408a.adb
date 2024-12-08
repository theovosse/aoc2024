with Ada.Text_IO; use Ada.Text_IO;
with Document;
with Problem1;
with Position;

procedure Aoc202408a is
begin
   Document.Read;
   Problem1.Solve;
   Put_Line (Integer'Image (Position.Nr_Anti_Node_Positions));
end Aoc202408a;
