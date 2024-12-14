pragma Ada_2022;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with Scanf;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Aoc202414a is

   Width : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   Height : constant Natural := Natural'Value (Ada.Command_Line.Argument (2));
   Num_Steps : constant Natural := Natural'Value (Ada.Command_Line.Argument (3));

   type Floor_Type is array (0 .. Height - 1, 0 .. Width - 1) of Natural;

   type Robot_Type is record
      X, Y, X0, Y0, DX, DY : Integer;
   end record;

   package Robot_Vectors is new Ada.Containers.Indefinite_Vectors (Natural, Robot_Type);

   Robots : Robot_Vectors.Vector;
   Floor : Floor_Type;

   procedure Read_Document is
      Robot : Robot_Type;
   begin
      while not End_Of_File loop
         Scanf.Read_Pat ("p=");
         Robot.X0 := Scanf.Read_Int;
         Scanf.Read_Pat (",");
         Robot.Y0 := Scanf.Read_Int;
         Scanf.Read_Pat (" v=");
         Robot.DX := Scanf.Read_Int;
         Scanf.Read_Pat (",");
         Robot.DY := Scanf.Read_Int;
         Robots.Append (Robot);
         exit when not Scanf.Read_Until_Newline;
      end loop;
   end Read_Document;

   procedure Calc_Robot_Positions (Num_Steps : Natural) is
   begin
      for Robot of Robots loop
         Robot.X := (Robot.X0 + Num_Steps * Robot.DX) mod Width;
         Robot.Y := (Robot.Y0 + Num_Steps * Robot.DY) mod Height;
      end loop;
   end Calc_Robot_Positions;

   procedure Count_Floor is
   begin
      Floor := [others => [others => 0]];
      for Robot of Robots loop
         Floor (Robot.Y, Robot.X) := @ + 1;
      end loop;
      for I in Floor'Range (1) loop
         for J in Floor'Range (2) loop
            if Floor (I, J) > 0 then
               Put (Character'Val (Floor (I, J) + 48));
            else
               Put (" ");
            end if;
         end loop;
         Put_Line ("");
      end loop;
   end Count_Floor;

   function Count_Quadrant (Y0, Y1, X0, X1 : Natural) return Natural is
      Sum : Natural := 0;
   begin
      for I in Y0 .. Y1 loop
         for J in X0 .. X1 loop
            Sum := @ + Floor (I, J);
         end loop;
      end loop;
      return Sum;
   end Count_Quadrant;

   type Quadrant_Counts_Type is array (1 .. 4) of Natural;

   function Quadrant_Counts return Quadrant_Counts_Type is
      [1 => Count_Quadrant (0, Height / 2 - 1, 0, Width / 2 - 1),
       2 => Count_Quadrant (0, Height / 2 - 1, Width / 2 + 1, Width - 1),
       3 => Count_Quadrant (Height / 2 + 1, Height - 1, 0, Width / 2 - 1),
       4 => Count_Quadrant (Height / 2 + 1, Height - 1, Width / 2 + 1, Width - 1)
      ];

begin
   Read_Document;
   Put_Line ("Num Robots " & Natural'Image (Natural (Robots.Length)));
   Calc_Robot_Positions (Num_Steps);
   Count_Floor;
   Put (Quadrant_Counts'Reduce ("*", 1));
end Aoc202414a;
