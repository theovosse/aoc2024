with Position; use Position;
with Document; use Document;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Problem1 is

   procedure Init is
      Node : Character;
      Position : Position_Type;
   begin
      for I in 0 .. Integer (Matrix.Length) - 1 loop
         Position.I := I;
         for J in 1 .. Line_Width loop
            Position.J := J;
            Node := Element (Matrix (I), J);
            if Node /= '.' then
               if Node_To_Position_List.Contains (Node) then
                  Node_To_Position_List (Node).Append (Position);
               else
                  declare
                     New_Position_List : Position_Vectors.Vector;
                  begin
                     New_Position_List.Append (Position);
                     Node_To_Position_List.Insert (Node, New_Position_List);
                  end;
               end if;
            end if;
         end loop;
      end loop;
   end Init;

   function Legal_Position (I, J : Integer) return Boolean is
      (0 <= I and then I < Nr_Lines and then 1 <= J and then J <= Line_Width);

   procedure Add_Anti_Node_Position (I, J : Integer) is
   begin
      if Anti_Node_Positions.Contains (I) then
         if not Anti_Node_Positions (I).Contains (J) then
            Anti_Node_Positions (I).Insert (J);
            Nr_Anti_Node_Positions := @ + 1;
         end if;
      else
         declare
            New_Integer_Set : Integer_Sets.Set;
         begin
            New_Integer_Set.Insert (J);
            Anti_Node_Positions.Insert (I, New_Integer_Set);
            Nr_Anti_Node_Positions := @ + 1;
         end;
      end if;
   end Add_Anti_Node_Position;

   procedure Add_Anti_Node_Pair (P1, P2 : Position_Type) is
      Delta_I : constant Integer := P1.I - P2.I;
      Delta_J : constant Integer := P1.J - P2.J;
      I, J : Integer;
   begin
      I := P1.I + Delta_I;
      J := P1.J + Delta_J;
      if Legal_Position (I, J) then
         Add_Anti_Node_Position (I, J);
      end if;
      I := P2.I - Delta_I;
      J := P2.J - Delta_J;
      if Legal_Position (I, J) then
         Add_Anti_Node_Position (I, J);
      end if;
   end Add_Anti_Node_Pair;

   procedure Determine_Anti_Node_Positions is
   begin
      for PL of Node_To_Position_List loop
         for I1 in 0 .. Integer (PL.Last_Index) - 1 loop
            for I2 in I1 + 1 .. Integer (PL.Last_Index) loop
               Add_Anti_Node_Pair (PL (I1), PL (I2));
            end loop;
         end loop;
      end loop;
   end Determine_Anti_Node_Positions;

   procedure Solve is
   begin
      Init;
      Determine_Anti_Node_Positions;
   end Solve;

end Problem1;