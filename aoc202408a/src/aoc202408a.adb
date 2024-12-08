with Ada.Text_IO; use Ada.Text_IO;
with Document;
with Problem1;
with Position;
with Problem2;
with Check_Anti_Node_Positions;

procedure Aoc202408a is
begin
   Document.Read;
   Check_Anti_Node_Positions.Init;
   Put_Line (" +++ SOLVE 1 +++");
   Problem1.Solve;
   Put_Line (Integer'Image (Position.Nr_Anti_Node_Positions));
   Put_Line (" +++ SOLVE 2 +++");
   Problem2.Solve;
   Put_Line (Integer'Image (Position.Nr_Anti_Node_Positions));
end Aoc202408a;
