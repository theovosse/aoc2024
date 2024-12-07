with Calibrations; use Calibrations;

package body Check_Calibration is

   --  Attempt to insert operators backwards.
   --  At each step, Rest_Goal represents what the preceding expression
   --  (running from 0 to Idx) must yield.
   --  The reason is efficiency. Evaluation is strictly left-to-right, so
   --  if the last operator is a multiplication, the goal must be a multiple of
   --  it. If not, it can only be an addition. This reduces the search space
   --  considerably.
   function Check_Rec (
      C : Calibration; Idx : Natural; Rest_Goal : Long_Integer
   ) return Boolean is
   begin
      --  When there's only one value, it must match the goal. If it isn't,
      --  the operator assignment failed.
      if Idx = 0 then
         return C.Operands (0) = Rest_Goal;
      end if;
      --  Check for multiplication or addition by reducing the
      --  goal with the final operand.
      return
         (Rest_Goal mod C.Operands (Idx) = 0 and then
          Check_Rec (C, Idx - 1, Rest_Goal / C.Operands (Idx)))
         or else
         Check_Rec (C, Idx - 1, Rest_Goal - C.Operands (Idx));
   end Check_Rec;

   function Check_Operators (C : Calibration) return Boolean is
   begin
      return Check_Rec (C, C.Operands'Last, C.Goal);
   end Check_Operators;

end Check_Calibration;