with Ada.Text_IO; use Ada.Text_IO;
with Calibrations;
with Check_Calibration;

procedure Aoc202407b is
   Sum : Long_Integer := 0;
begin
   for I in Calibrations.Calibration_List'Range loop
      if Check_Calibration.Check_Operators
            (Calibrations.Calibration_List (I))
      then
         Sum := Sum + Calibrations.Calibration_List (I).Goal;
      end if;
   end loop;
   Put_Line (Long_Integer'Image (Sum));
end Aoc202407b;
