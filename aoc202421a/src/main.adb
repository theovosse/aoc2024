pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

procedure Main is
   type Num_Keypad_Range is new Natural range 0 .. 10;
   type Num_Keypad_Paths is
      array (Num_Keypad_Range, Num_Keypad_Range) of Unbounded_String;
   type Dir_Keypad_Type is (Up, Action, Left, Down, Right);
   type Dir_Keypad_Paths is
      array (Dir_Keypad_Type'Range, Dir_Keypad_Type'Range) of
      Unbounded_String;
   type Coordinate is record
      Row, Col : Integer;
   end record;

   Bad_Document : exception;

   Shortest_Num_Keypad_Paths : Num_Keypad_Paths;
   Num_Keypad_Pos : constant array (Num_Keypad_Range) of Coordinate := [
      7 => (0, 0), 8 => (0, 1), 9 => (0, 2),
      4 => (1, 0), 5 => (1, 1), 6 => (1, 2),
      1 => (2, 0), 2 => (2, 1), 3 => (2, 2),
                   0 => (3, 1), 10 => (3, 2)
   ];
   Num_Keypad_Gap : constant Coordinate := (3, 0);
   Shortest_Dir_Keypad_Paths : Dir_Keypad_Paths;
   Dir_Keypad_Pos : constant array (Dir_Keypad_Type'Range) of Coordinate := [
                      Up   => (0, 1), Action => (0, 2),
      Left => (1, 0), Down => (1, 1), Right  => (1, 2)
   ];
   Dir_Keypad_Gap : constant Coordinate := (0, 0);

   --  Returns key presses from start to finish (avoiding the gap) optimized
   --  for repeated key presses (which keeps the indirect layer small).
   function Dist_Str (Start, Finish, Gap : Coordinate; Prefer_Horizontal_First : Boolean) return Unbounded_String is
      Str : Unbounded_String;
      function Vertical_First_Goes_Through_Gap return Boolean is
      begin
         --  This works because the gaps are in a corner
         return Start.Col = Gap.Col and then Gap.Row = Finish.Row;
      end Vertical_First_Goes_Through_Gap;
      function Horizontal_First_Goes_Through_Gap return Boolean is
      begin
         return Start.Row = Gap.Row and then Gap.Col = Finish.Col;
      end Horizontal_First_Goes_Through_Gap;
      procedure Horizontal is
      begin
         for Col in Start.Col + 1 .. Finish.Col loop
            Str := @ & '>';
         end loop;
         for Col in Finish.Col + 1 .. Start.Col loop
            Str := @ & '<';
         end loop;
      end Horizontal;
      procedure Vertical is
      begin
         for Row in Start.Row + 1 .. Finish.Row loop
            Str := @ & 'v';
         end loop;
         for Row in Finish.Row + 1 .. Start.Row loop
            Str := @ & '^';
         end loop;
      end Vertical;
   begin
      if Vertical_First_Goes_Through_Gap or else
         (Prefer_Horizontal_First and then not Horizontal_First_Goes_Through_Gap)
      then
         Horizontal;
         Vertical;
      else
         Vertical;
         Horizontal;
      end if;
      return Str;
   end Dist_Str;

   function Char_To_Num_Keypad (Ch : Character) return Num_Keypad_Range is
   begin
      case Ch is
         when '0' .. '9' => return Character'Pos (Ch) - 48;
         when 'A' => return 10;
         when others => raise Bad_Document;
      end case;
   end Char_To_Num_Keypad;

   procedure Fill_Shortest_Num_Keypad_Paths is
   begin
      for Start in Shortest_Num_Keypad_Paths'Range (1) loop
         for Dest in Shortest_Num_Keypad_Paths'Range (2) loop
            Shortest_Num_Keypad_Paths (Start, Dest) :=
               Dist_Str (Num_Keypad_Pos (Start), Num_Keypad_Pos (Dest), Num_Keypad_Gap, False);
         end loop;
      end loop;
   end Fill_Shortest_Num_Keypad_Paths;

   function Get_Shortest_Num_Keypad_Path (Str : Unbounded_String) return Unbounded_String is
      Cur_Pos : Num_Keypad_Range := 10;
      Next_Pos : Num_Keypad_Range;
      Path : Unbounded_String;
   begin
      for Row in 1 .. Length (Str) loop
         Next_Pos := Char_To_Num_Keypad (Element (Str, Row));
         Path := @ & Shortest_Num_Keypad_Paths (Cur_Pos, Next_Pos) & 'A';
         Cur_Pos := Next_Pos;
      end loop;
      return Path;
   end Get_Shortest_Num_Keypad_Path;

   procedure Fill_Shortest_Dir_Keypad_Paths is
      function Horizontal_Heuristic (Start, Finish : Coordinate) return Boolean is
         (Finish.Col > Start.Col);
   begin
      for Start in Shortest_Dir_Keypad_Paths'Range (1) loop
         for Dest in Shortest_Dir_Keypad_Paths'Range (2) loop
            Shortest_Dir_Keypad_Paths (Start, Dest) :=
               Dist_Str (Dir_Keypad_Pos (Start), Dir_Keypad_Pos (Dest), Dir_Keypad_Gap,
                         Horizontal_Heuristic (Dir_Keypad_Pos (Start), Dir_Keypad_Pos (Dest)));
         end loop;
      end loop;
   end Fill_Shortest_Dir_Keypad_Paths;

   function Char_To_Dir_Keypad (Ch : Character) return Dir_Keypad_Type is
   begin
      case Ch is
         when '<' => return Left;
         when '^' => return Up;
         when '>' => return Right;
         when 'v' => return Down;
         when 'A' => return Action;
         when others => raise Bad_Document;
      end case;
   end Char_To_Dir_Keypad;

   function Get_Shortest_Dir_Keypad_Path (Str : Unbounded_String) return Unbounded_String is
      Cur_Pos : Dir_Keypad_Type := Action;
      Next_Pos : Dir_Keypad_Type;
      Path : Unbounded_String;
   begin
      for Row in 1 .. Length (Str) loop
         Next_Pos := Char_To_Dir_Keypad (Element (Str, Row));
         Path := @ & Shortest_Dir_Keypad_Paths (Cur_Pos, Next_Pos) & 'A';
         Cur_Pos := Next_Pos;
      end loop;
      return Path;
   end Get_Shortest_Dir_Keypad_Path;

   function AtoI (Str : Unbounded_String) return Natural is
      N : Natural := 0;
      Ch : Character;
   begin
      for Row in 1 .. Length (Str) loop
         Ch := Element (Str, Row);
         if '0' <= Ch and then Ch <= '9' then
            N := @ * 10 + Character'Pos (Ch) - 48;
         end if;
      end loop;
      return N;
   end AtoI;

   procedure Process_Document is
      Line, Path : Unbounded_String;
      Complexity : Natural;
      Complexity_Sum : Natural := 0;
   begin
      while not End_Of_File loop
         Line := Get_Line;
         Path := Get_Shortest_Dir_Keypad_Path (
                  Get_Shortest_Dir_Keypad_Path (
                   Get_Shortest_Num_Keypad_Path (Line)));
         Put_Line (Line & ':' & Length (Path)'Image & ' ' & Path);
         Complexity := Length (Path) * AtoI (Line);
         Complexity_Sum := @ + Complexity;
      end loop;
      Put_Line ("Complexity sum is" & Complexity_Sum'Image);
   end Process_Document;

begin
   Fill_Shortest_Num_Keypad_Paths;
   --  for Row in Shortest_Num_Keypad_Paths'Range (1) loop
   --     for Col in Shortest_Num_Keypad_Paths'Range (1) loop
   --        Put_Line (Row'Image & " =>" & Col'Image & ": " & Shortest_Num_Keypad_Paths (Row, Col));
   --     end loop;
   --  end loop;
   Fill_Shortest_Dir_Keypad_Paths;
   --  for Row in Shortest_Dir_Keypad_Paths'Range (1) loop
   --     for Col in Shortest_Dir_Keypad_Paths'Range (1) loop
   --        Put_Line (Row'Image & " => " & Col'Image & ": " & Shortest_Dir_Keypad_Paths (Row, Col));
   --     end loop;
   --  end loop;
   Process_Document;
end Main;
