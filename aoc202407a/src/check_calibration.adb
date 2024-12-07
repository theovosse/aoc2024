with Calibrations; use Calibrations;

package body Check_Calibration is

   function Check_Rec (
      C : Calibration; Idx : Natural; Rest_Goal : Long_Integer
   ) return Boolean is
   begin
      if Idx = 0 then
         return C.Operands (0) = Rest_Goal;
      end if;
      if Rest_Goal mod C.Operands (Idx) = 0 and then
         Check_Rec (C, Idx - 1, Rest_Goal / C.Operands (Idx))
      then
         return True;
      end if;
      return Check_Rec (C, Idx - 1, Rest_Goal - C.Operands (Idx));
   end Check_Rec;

   function Check_Operators (C : Calibration) return Boolean is
   begin
      return Check_Rec (C, C.Operands'Last, C.Goal);
   end Check_Operators;

end Check_Calibration;