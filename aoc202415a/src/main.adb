pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with String_Vectors;
with Ada.Strings; use Ada.Strings;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Main is

   Bad_Doc : exception;
   Map : String_Vectors.Vector;
   Width, Height : Natural := 0;
   Moves : Unbounded_String;
   X, Y : Integer := 0;

   procedure Print_Map is
   begin
      for Line of Map loop
         Put_Line (Line);
      end loop;
      Put_Line ("");
   end Print_Map;

   procedure Read_Documents is
      Line : Unbounded_String;
   begin
      Get_Line (Line);
      Width := Length (Line) - 2;
      loop
         Get_Line (Line);
         exit when Length (Line) = 0;
         Map.Append (To_Unbounded_String (Slice (Line, 2, Length (Line) - 1)));
      end loop;
      Map.Delete_Last;
      Height := Integer (Map.Length);
      while not End_Of_File loop
         Get_Line (Line);
         Moves := @ & Line;
      end loop;
   end Read_Documents;

   procedure Determine_Initial_Position is
   begin
      Y := 0;
      while Y < Height loop
         X := Index (Map (Y), "@", 1);
         if X /= 0 then
            return;
         end if;
         Y := @ + 1;
      end loop;
      raise Bad_Doc;
   end Determine_Initial_Position;

   function Is_Blocked (DY, DX : Integer) return Boolean is
      NY : Integer := Y;
      NX : Integer := X;
      Cur : Character;
   begin
      loop
         NY := @ + DY; NX := @ + DX;
         if NY < 0 or else NY >= Height or else NX < 1 or else NX > Width then
            return True;
         end if;
         Cur := Element (Map (NY), NX);
         if Cur = '.' then
            return False;
         end if;
         if Cur = '#' then
            return True;
         end if;
      end loop;
   end Is_Blocked;

   procedure Move (DY, DX : Integer) is
      NY : Integer := Y;
      NX : Integer := X;
      Cur, Prev : Character := '.';
   begin
      if Is_Blocked (DY, DX) then
         return;
      end if;
      loop
         Cur := Element (Map (NY), NX);
         Replace_Element (Map (NY), NX, Prev);
         NY := NY + DY; NX := NX + DX;
         exit when NY < 0 or else NY >= Height or else NX < 1 or else NX > Width;
         exit when Cur = '.';
         Prev := Cur;
      end loop;
      Y := Y + DY; X := X + DX;
   end Move;

   procedure Execute_Move (Move_Char : Character) is
   begin
      case Move_Char is
         when '<' => Move (0, -1);
         when '>' => Move (0, 1);
         when '^' => Move (-1, 0);
         when 'v' => Move (1, 0);
         when others => raise Bad_Doc;
      end case;
   end Execute_Move;

   procedure Execute_Moves is
   begin
      for I in 1 .. Length (Moves) loop
         Execute_Move (Element (Moves, I));
      end loop;
   end Execute_Moves;

   function Sum_Of_Box_Distances return Natural is
      Sum : Natural := 0;
      Row : Unbounded_String;
   begin
      for Y in 0 .. Height - 1 loop
         Row := Map (Y);
         for X in 1 .. Width loop
            if Element (Row, X) = 'O' then
               Sum := @ + (Y + 1) * 100 + X;
            end if;
         end loop;
      end loop;
      return Sum;
   end Sum_Of_Box_Distances;

begin
   Read_Documents;
   Determine_Initial_Position;
   Execute_Moves;
   Put_Line (Natural'Image (Sum_Of_Box_Distances));
end Main;
