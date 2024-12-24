package body Gates is

   function Gate_Output (Operator : Unbounded_String; Input1, Input2 : Boolean) return Boolean is
   begin
      if Operator = "AND" then
         return Input1 and Input2;
      elsif Operator = "OR" then
         return Input1 or Input2;
      elsif Operator = "XOR" then
         return Input1 xor Input2;
      end if;
      raise Bad_Operator;
   end Gate_Output;

end Gates;