with Ada.Text_IO; use Ada.Text_IO;

procedure Aoc202411a is

   type Nat_Arr is array (Natural range <>) of Long_Integer;

   --  ProblemTest : constant Nat_Arr := [125, 17];
   Problem1 : constant Nat_Arr :=
      [5, 127, 680267, 39260, 0, 26, 3553, 5851995];
   Nr_Splits : constant Natural := 25;

   function Nr_Digits_Of (Num : Long_Integer) return Natural is
      Nr_Digits_Of_Num : Natural := 0;
      N : Long_Integer := Num;
   begin
      loop
         N := @ / 10;
         Nr_Digits_Of_Num := Nr_Digits_Of_Num + 1;
         exit when N = 0;
      end loop;
      return Nr_Digits_Of_Num;
   end Nr_Digits_Of;

   function Pow_10 (N : Natural) return Long_Integer is
      Value : Long_Integer := 1;
   begin
      for I in 1 .. N loop
         Value := @ * 10;
      end loop;
      return Value;
   end Pow_10;

   type Split_Result is array (1 .. 2) of Long_Integer;

   function Split_Number (N : Long_Integer; Nr : Natural) return Split_Result is
      Nr_Pow_10 : constant Long_Integer := Pow_10 (Nr);
   begin
      return [N / Nr_Pow_10, N mod Nr_Pow_10];
   end Split_Number;

   function Count_Stone_Split (Stone : Long_Integer; Nr_Splits : Natural)
   return Long_Integer is
      Nr_Digits : Natural;
      Split : Split_Result;
   begin
      if Nr_Splits = 0 then
         return 1;
      end if;
      if Stone = 0 then
         return Count_Stone_Split (1, Nr_Splits - 1);
      end if;
      Nr_Digits := Nr_Digits_Of (Stone);
      if Nr_Digits mod 2 = 0 then
         Split := Split_Number (Stone, Nr_Digits / 2);
         return Count_Stone_Split (Split (1), Nr_Splits - 1) +
                Count_Stone_Split (Split (2), Nr_Splits - 1);
      end if;
      return Count_Stone_Split (Stone * 2024, Nr_Splits - 1);
   end Count_Stone_Split;

   function Count_Splits (Problem : Nat_Arr; Nr_Splits : Natural)
   return Long_Integer is
      Sum : Long_Integer := 0;
   begin
      for Stone of Problem loop
         Sum := @ + Count_Stone_Split (Stone, Nr_Splits);
      end loop;
      return Sum;
   end Count_Splits;

begin
   Put_Line (Long_Integer'Image (Count_Splits (Problem1, Nr_Splits)));
end Aoc202411a;
