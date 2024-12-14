package Scanf is

   No_Match : exception;

   procedure Read_Pat (Str : String);

   function Read_Until_Newline return Boolean;

   function Read_Int return Integer;

end Scanf;