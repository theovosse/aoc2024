with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Characters.Latin_1;

package body Scanf is

   procedure Read_Pat (Str : String) is
      Pos : Natural := 1;
      Ch : Character;
   begin
      while Pos <= Str'Length loop
         if End_Of_File then
            raise No_Match;
         end if;
         Get_Immediate (Ch);
         if Ch /= Str (Pos) then
            raise No_Match;
         end if;
         Pos := @ + 1;
      end loop;
   end Read_Pat;

   function Read_Until_Newline return Boolean is
      Ch : Character;
   begin
      while not End_Of_File loop
         Get_Immediate (Ch);
         if Ch = Ada.Characters.Latin_1.LF then
            return True;
         end if;
      end loop;
      return False;
   end Read_Until_Newline;

   function Read_Int return Integer is
      Value : Integer;
   begin
      Get (Value);
      return Value;
   end Read_Int;

end Scanf;