package Check_Anti_Node_Positions is

   procedure Init;

   function Legal_Position (I, J : Integer) return Boolean;

   procedure Add_Anti_Node_Position (I, J : Integer);

   procedure Determine_Anti_Node_Positions (Min, Max : Integer);

end Check_Anti_Node_Positions;