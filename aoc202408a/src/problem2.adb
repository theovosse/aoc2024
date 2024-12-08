with Document; use Document;
with Check_Anti_Node_Positions; use Check_Anti_Node_Positions;

package body Problem2 is

   procedure Solve is
   begin
      Determine_Anti_Node_Positions (0, Line_Width + Nr_Lines);
   end Solve;

end Problem2;