pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with String_Vectors;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Doubly_Linked_Lists;

procedure Main is

   Bad_Input : exception;
   Document : String_Vectors.Vector;
   Width : Natural := 0;
   Height : Natural := 0;

   procedure Read_Document is
      Line : Unbounded_String;
      Line_Nr : Natural := 0;
   begin
      while not End_Of_File loop
         Line := Get_Line;
         if Line_Nr > 0 then
            Document.Append (To_Unbounded_String (Slice (Line, 2, Length (Line) - 1)));
         end if;
         Line_Nr := @ + 1;
      end loop;
      Width := Length (Document (0));
      Height := Natural (Document.Length) - 1;
      Document.Delete (Height);
   end Read_Document;

   type Coordinate is record
      I, J : Natural;
   end record;

   package Coordinate_Lists is new Ada.Containers.Doubly_Linked_Lists (Coordinate);

   function Find_Position (C : Character) return Coordinate is
      Row : Unbounded_String;
   begin
      for I in 0 .. Height - 1 loop
         Row := Document (I);
         for J in 1 .. Width loop
            if Element (Row, J) = C then
               return (I, J - 1);
            end if;
         end loop;
      end loop;
      raise Bad_Input;
   end Find_Position;

   type Bool_Mat is array (Natural range <>, Natural range <>) of Boolean;
   type Nat_AMat is array (Natural range <>, Natural range <>) of Natural
      with Default_Component_Value => 0;

   function Convert return Bool_Mat is
      subtype Converted_Array is Bool_Mat (0 .. Height - 1, 0 .. Width - 1);
      M : Converted_Array;
   begin
      for I in M'Range (1) loop
         for J in M'Range (2) loop
            M (I, J) := Element (Document (I), J + 1) = '#';
         end loop;
      end loop;
      return M;
   end Convert;

   --  procedure Print (M : Nat_AMat) is
   --  begin
   --     for I in M'Range (1) loop
   --        for J in M'Range (2) loop
   --           Put (M (I, J), 3);
   --        end loop;
   --        Put_Line ("");
   --     end loop;
   --  end Print;

   function Shortest_Path_Length_From (Block : Bool_Mat; Start : Coordinate; Length : out Nat_AMat) return Natural is
      type Edges_Index is new Natural range 0 .. 1;
      subtype Bool_CMat is Bool_Mat (0 .. Height - 1, 0 .. Width - 1);
      Edges : array (Edges_Index) of Coordinate_Lists.List;
      Cur_Edge : Edges_Index := 0;
      Visited : Bool_CMat := [others => [others => False]];
      Cost : Natural := 0;
      procedure Add (Pos : Coordinate) is
      begin
         if not Block (Pos.I, Pos.J) and then not Visited (Pos.I, Pos.J) then
            Edges (Cur_Edge).Append (Pos);
            Visited (Pos.I, Pos.J) := True;
            Length (Pos.I, Pos.J) := Cost;
         end if;
      end Add;
   begin
      Edges (Cur_Edge).Append ((Start.I, Start.J));
      Visited (Start.I, Start.J) := True;
      while not Edges (Cur_Edge).Is_Empty loop
         Cur_Edge := 1 - Cur_Edge;
         Edges (Cur_Edge) := Coordinate_Lists.Empty_List;
         Cost := @ + 1;
         for Pos of Edges (1 - Cur_Edge) loop
            if Pos.I < Height - 1 then
               Add ((Pos.I + 1, Pos.J));
            end if;
            if Pos.I > 0 then
               Add ((Pos.I - 1, Pos.J));
            end if;
            if Pos.J < Width - 1 then
               Add ((Pos.I, Pos.J + 1));
            end if;
            if Pos.J > 0 then
               Add ((Pos.I, Pos.J - 1));
            end if;
         end loop;
      end loop;
      return Cost - 1;
   end Shortest_Path_Length_From;

   procedure Solve (Block : Bool_Mat; Start, Finish : Coordinate) is
      subtype Nat_Mat is Nat_AMat (0 .. Height - 1, 0 .. Width - 1);
      Start_Cost, Finish_Cost : Nat_Mat;
      Max_Cost : Natural;
      Nr_Savings_Over_100 : Natural := 0;

      procedure Save (I : Integer; J : Integer; DI, DJ : Integer) is
         New_Cost : constant Natural := Start_Cost (I + DI, J + DJ) + Finish_Cost (I - DI, J - DJ) + 2;
      begin
         if New_Cost + 100 <= Max_Cost then
            Nr_Savings_Over_100 := @ + 1;
         end if;
      end Save;

      procedure Print_All_Shortcuts is
      begin
         for I in Start_Cost'Range (1) loop
            for J in Start_Cost'Range (2) loop
               if Block (I, J) then
                  if 0 < I and then I < Height - 1 then
                     if not Block (I - 1, J) and then not Block (I + 1, J) then
                        Save (I, J, -1, 0);
                        Save (I, J, 1, 0);
                     end if;
                  end if;
                  if 0 < J and then J < Width - 1 then
                     if not Block (I, J - 1) and then not Block (I, J + 1) then
                        Save (I, J, 0, -1);
                        Save (I, J, 0, 1);
                     end if;
                  end if;
               end if;
            end loop;
         end loop;
      end Print_All_Shortcuts;

   begin
      Max_Cost := Shortest_Path_Length_From (Block, Start, Start_Cost);
      Max_Cost := Shortest_Path_Length_From (Block, Finish, Finish_Cost);
      Print_All_Shortcuts;
      Put_Line (Nr_Savings_Over_100'Image);
   end Solve;

begin
   Read_Document;
   Solve (Convert, Find_Position ('S'), Find_Position ('E'));
end Main;
