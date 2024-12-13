pragma Ada_2022;
pragma Extensions_Allowed (All);
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps;

procedure Aoc202413a is

   Bad_Problem : exception;
   Offset : constant Long_Integer := 10000000000000; --  set to 0 for problem set 1

   procedure Solve (X_A, Y_A, X_B, Y_B, X_T, Y_T : Long_Integer; N, M : out Long_Integer) is
   begin
      if X_B * Y_A = X_A * Y_B then
         --  This can be solved by dividing X_T by X_A and X_B (or Y_T by Y_A and Y_B when X_B is 0)
         --  and checking if either solution fits. If both fit, pick the cheapest. If there is a
         --  fractional difference, something else must be added. But my problem didn't have this
         --  issue, so here we are.
         raise Bad_Problem with "linear dependence";
      elsif X_B /= 0 then
         N := (X_T * Y_B - X_B * Y_T) / (X_A * Y_B - X_B * Y_A);
         M := (X_T - N * X_A) / X_B;
      elsif X_A /= 0 then
         M := (X_T * Y_A - X_A * Y_T) / (X_B * Y_A - X_A * Y_B);
         N := (X_T - M * X_B) / X_A;
      else
         raise Bad_Problem with "X_A = 0 and X_B = 0";
      end if;
   end Solve;

   function Check (X_A, Y_A, X_B, Y_B, X_T, Y_T, N, M : Long_Integer) return Boolean is
      (N * X_A + M * X_B = X_T and then N * Y_A + M * Y_B = Y_T);

   Comma : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (',');
   Plus : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ('+');
   Equals : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ('=');

   procedure Parse_Param (Line : Unbounded_String; Sep : Ada.Strings.Maps.Character_Set; X, Y : out Long_Integer) is
      Pos1, Pos2 : Natural;
   begin
      Pos1 := Index (Line, Sep, 1);
      raise Bad_Problem when Pos1 = 0;
      Pos2 := Index (Line, Comma, Pos1 + 1);
      raise Bad_Problem when Pos2 = 0;
      X := Long_Integer'Value (Slice (Line, Pos1 + 1, Pos2 - 1));
      Pos1 := Index (Line, Sep, Pos2 + 1);
      raise Bad_Problem when Pos1 = 0;
      Y := Long_Integer'Value (Slice (Line, Pos1 + 1, Length (Line)));
   end Parse_Param;

   procedure Read_Document is
      Line : Unbounded_String;
      X_A, Y_A, X_B, Y_B, X_T, Y_T, N, M, Cost : Long_Integer;
      Cost_Sum : Long_Integer := 0;
   begin
      loop
         Line := Ada.Text_IO.Unbounded_IO.Get_Line;
         Parse_Param (Line, Plus, X_A, Y_A);
         Line := Ada.Text_IO.Unbounded_IO.Get_Line;
         Parse_Param (Line, Plus, X_B, Y_B);
         Line := Ada.Text_IO.Unbounded_IO.Get_Line;
         Parse_Param (Line, Equals, X_T, Y_T);
         Solve (X_A, Y_A, X_B, Y_B, Offset + X_T, Offset + Y_T, N, M);
         if Check (X_A, Y_A, X_B, Y_B, Offset + X_T, Offset + Y_T, N, M) then
            Cost := 3 * N + M;
            Cost_Sum := @ + Cost;
         end if;
         exit when End_Of_File;
         Line := Ada.Text_IO.Unbounded_IO.Get_Line;
      end loop;
      Put_Line ("Total" & Long_Integer'Image (Cost_Sum));
   end Read_Document;

begin
   Read_Document;
end Aoc202413a;
