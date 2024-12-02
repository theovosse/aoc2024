with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed, Ada.Strings.Maps;
use Ada.Strings, Ada.Strings.Fixed, Ada.Strings.Maps;

procedure Aoc202402a is
   type Report_Index is range 0 .. 1000;
   type Report_Array is array (Report_Index) of Integer;

   NSafe : Natural := 0;
   NPartiallySafe : Natural := 0;
   Report : Report_Array;
   NValues : Report_Index := 0;

   function Sign (I : Integer) return Integer is
      (if I < 0 then -1 elsif I = 0 then 0 else 1);

   procedure Tokenize
      (Str : String;
       Report : in out Report_Array;
       NValues : in out Report_Index) is
      Pos : Natural := Str'First;
      Whitespace : constant Character_Set := To_Set (' ');
      Start, Finish : Natural;
   begin
      NValues := 0;
      while Pos <= Str'Last loop
         Find_Token (Str, Whitespace, Pos, Outside, Start, Finish);
         exit when Finish = 0;
         Report (NValues) := Integer'Value (Str (Start .. Finish));
         NValues := @ + 1;
         Pos := Finish + 1;
      end loop;
   end Tokenize;

   function Read_Line
      (Report : in out Report_Array; NValues : in out Report_Index)
      return Boolean is
   begin
      NValues := 0;
      if End_Of_File then
         return False;
      end if;
      Tokenize (Get_Line, Report, NValues);
      return True;
   end Read_Line;

   function Report_Is_Safe (
      Report : Report_Array;
      NValues : Report_Index
   ) return Boolean is
      Diff, AbsDiff, Direction : Integer;
   begin
      if NValues < 2 then
         return False;
      end if;
      Direction := Sign (Report (1) - Report (0));
      for I in 1 .. NValues - 1 loop
         Diff := Report (I) - Report (I - 1);
         AbsDiff := abs (Diff);
         if AbsDiff = 0 or else AbsDiff > 3 or else Sign (Diff) /= Direction
         then
            return False;
         end if;
      end loop;
      return True;
   end Report_Is_Safe;

   function Report_Minus_1_Is_Safe (
      Report : Report_Array;
      NValues : Report_Index;
      Exclude : Report_Index
   ) return Boolean is
      function Skip_Exclude (I : Report_Index) return Report_Index is
         (if I = Exclude then Exclude + 1 else I);
      Diff, AbsDiff, Direction : Integer;
      Prev : Report_Index := Skip_Exclude (0);
      Cur : Report_Index := Skip_Exclude (Prev + 1);
   begin
      if NValues < 2 then
         return False;
      end if;
      Direction := Sign (Report (Cur) - Report (Prev));
      while Cur < NValues loop
         Diff := Report (Cur) - Report (Prev);
         AbsDiff := abs (Diff);
         if AbsDiff = 0 or else AbsDiff > 3 or else Sign (Diff) /= Direction
         then
            return False;
         end if;
         Prev := Cur;
         Cur := Skip_Exclude (Cur + 1);
      end loop;
      return True;
   end Report_Minus_1_Is_Safe;

begin
   while Read_Line (Report, NValues) loop
      if Report_Is_Safe (Report, NValues)
      then
         NSafe := @ + 1;
      elsif (for some Excl in 0 .. NValues - 1 =>
             Report_Minus_1_Is_Safe (Report, NValues, Excl))
      then
         NPartiallySafe := @ + 1;
      end if;
   end loop;
   Put_Line (Natural'Image (NSafe));
   Put_Line (Natural'Image (NPartiallySafe));
   Put_Line (Natural'Image (NSafe + NPartiallySafe));
end Aoc202402a;
