--  This approach uses GNAT.Regpat. It however suffers from the
--  usual problem of regexp matchers: it backtracks, and can take
--  a very long time on unruly input.

pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Regpat; use GNAT.Regpat;

procedure Main is

   Matcher : Pattern_Matcher (10000);

   procedure Make_Pattern (Str : Unbounded_String; Sep : String) is
      Cur : Positive := 1;
      Pos : Natural;
      Pattern : Unbounded_String := To_Unbounded_String ("^(");
   begin
      loop
         Pos := Index (Str, Sep, Cur);
         exit when Pos = 0;
         Pattern := @ & Slice (Str, Cur, Pos - 1) & "|";
         Cur := Pos + Sep'Length;
      end loop;
      Pattern := @ & Slice (Str, Cur, Length (Str)) & ")*$";
      Put_Line (Pattern);
      GNAT.Regpat.Compile (Matcher, To_String (Pattern));
   end Make_Pattern;

   function Check_Possible (Line : Unbounded_String) return Boolean is
   begin
      return Match (Matcher, To_String (Line));
   end Check_Possible;

   procedure Read_Document is
      Line : Unbounded_String;
      Nr_Possible : Natural := 0;
   begin
      Line := Get_Line;
      Make_Pattern (Line, ", ");
      Line := Get_Line;
      while not End_Of_File loop
         Line := Get_Line;
         Put_Line (Line);
         if Check_Possible (Line) then
            Nr_Possible := @ + 1;
         end if;
      end loop;
      Put_Line (Nr_Possible'Image);
   end Read_Document;

begin
   Read_Document;
end Main;
