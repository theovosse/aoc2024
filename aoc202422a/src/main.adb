pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is

   type Rnd_Type is mod 2**64;

   function Next_Secret (N : Rnd_Type) return Rnd_Type is
      A : constant Rnd_Type := N * 64;
      N1 : constant Rnd_Type := N xor A;
      N2 : constant Rnd_Type := N1 and 16#ffffff#;
      D : constant Rnd_Type := N2 / 32;
      N3 : constant Rnd_Type := D xor N2;
      N4 : constant Rnd_Type := N3 and 16#ffffff#;
      G : constant Rnd_Type := N4 * 2048;
      N5 : constant Rnd_Type := N4 xor G;
      N6 : constant Rnd_Type := N5 and 16#ffffff#;
   begin
      return N6;
   end Next_Secret;

   function Repeat_Next_Secret (Secret : Integer; Nr_Reps : Natural) return Rnd_Type is
      Result : Rnd_Type := Rnd_Type (Secret);
   begin
      for I in 1 .. Nr_Reps loop
         Result := Next_Secret (Result);
      end loop;
      return Result;
   end Repeat_Next_Secret;

   procedure Process_Document is
      N : Integer;
      Sum : Rnd_Type := 0;
   begin
      while not End_Of_File loop
         Get (N);
         Sum := @ + Repeat_Next_Secret (N, 2000);
      end loop;
      Put_Line (Sum'Image);
   end Process_Document;

begin
   Process_Document;
end Main;
