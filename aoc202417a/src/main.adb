with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is

   type Register_Value is mod 2**31;
   type Registers_Type is array (0 .. 2) of Register_Value;

   Bad_Program : exception;
   Register_Init : Registers_Type;
   Program : array (0 .. 1000) of Natural range 0 .. 7;
   Program_Length : Natural := 0;

   --  Reads the registers and program, but they must be
   --  separated by spaces and newlines, so the input needs
   --  to be edited a bit.
   procedure Read_Memory is
      function Get_Register_Value return Register_Value is
         N : Integer;
      begin
         Get (N);
         return Register_Value (N);
      end Get_Register_Value;
   begin
      Register_Init (0) := Get_Register_Value;
      Register_Init (1) := Get_Register_Value;
      Register_Init (2) := Get_Register_Value;
      while not End_Of_File loop
         Get (Program (Program_Length));
         Program_Length := @ + 1;
      end loop;
   end Read_Memory;

   procedure Run_Program is
      PC : Natural := 0;
      Registers : Registers_Type := Register_Init;
      Operation, Operand : Natural range 0 .. 7;
      Has_Output : Boolean := False;

      function Combo return Register_Value is
      begin
         case Operand is
            when 0 | 1 | 2 | 3 => return Register_Value (Operand);
            when 4 => return Registers (0);
            when 5 => return Registers (1);
            when 6 => return Registers (2);
            when others => raise Bad_Program;
         end case;
      end Combo;

      procedure Adv is
      begin
         for I in 1 .. Combo loop
            Registers (0) := @ / 2;
         end loop;
      end Adv;

      procedure Bxl is
      begin
         Registers (1) := @ xor Register_Value (Operand);
      end Bxl;

      procedure Bst is
      begin
         Registers (1) := Combo mod 8;
      end Bst;

      procedure Jnz is
      begin
         if Registers (0) /= 0 then
            PC := Operand;
         end if;
      end Jnz;

      procedure Bxc is
      begin
         Registers (1) := @ xor Registers (2);
      end Bxc;

      procedure Output is
         Value : constant Register_Value := Combo mod 8;
      begin
         if Has_Output then
            Put (',');
         end if;
         Put (Character'Val (Value + 48));
         Has_Output := True;
      end Output;

      procedure Bdv is
      begin
         Registers (1) := Registers (0);
         for I in 1 .. Combo loop
            Registers (1) := @ / 2;
         end loop;
      end Bdv;

      procedure Cdv is
      begin
         Registers (2) := Registers (0);
         for I in 1 .. Combo loop
            Registers (2) := @ / 2;
         end loop;
      end Cdv;
   begin
      while PC < Program_Length loop
         Operation := Program (PC);
         Operand := Program (PC + 1);
         --  Put_Line ("PC =" & PC'Image & ", Len =" & Program_Length'Image);
         --  Put_Line ("Op =" & Operation'Image & ", Op =" & Operand'Image & ", Combo =" & Combo'Image);
         PC := @ + 2;
         case Operation is
            when 0 => Adv;
            when 1 => Bxl;
            when 2 => Bst;
            when 3 => Jnz;
            when 4 => Bxc;
            when 5 => Output;
            when 6 => Bdv;
            when 7 => Cdv;
         end case;
         --  Put_Line ("A =" & Registers (0)'Image & ", B =" & Registers (1)'Image & ", C =" & Registers (2)'Image);
      end loop;
      if Has_Output then
         Put_Line ("");
      end if;
   end Run_Program;

begin
   Read_Memory;
   Run_Program;
end Main;
