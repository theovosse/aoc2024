pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with String_Vectors;
with Coords; use Coords;
with Int_Sets;

procedure Main is

   Bad_Input : exception;
   Document : String_Vectors.Vector;
   Width : Integer := 0;
   Height : Integer := 0;

   procedure Read_Document is
   begin
      while not End_Of_File loop
         Document.Append (Get_Line);
      end loop;
      Width := Length (Document (0));
      Height := Integer (Document.Length);
   end Read_Document;

   function Find_Position (C : Character) return Coordinate is
      Row : Unbounded_String;
      Empty : Int_Sets.Set;
   begin
      for I in 0 .. Height - 1 loop
         Row := Document (I);
         for J in 1 .. Width loop
            if Element (Row, J) = C then
               return (I, J, 0, 0, 0, Empty);
            end if;
         end loop;
      end loop;
      raise Bad_Input;
   end Find_Position;

   function Coord_Hash (Coord : Coordinate) return Integer is (Coord.I * Width + Coord.J - 1);

   procedure Coord_Unhash (Hash : Integer; I, J : out Integer) is
   begin
      I := Hash / Width;
      J := Hash mod Width + 1;
   end Coord_Unhash;

   function Is_Legal_Position (Coord : Coordinate; Visited : Int_Sets.Set) return Boolean is
      (Element (Document (Coord.I), Coord.J) /= '#' and then not Visited.Contains (Coord_Hash (Coord)));

   procedure Shortest_Path (Start, Finish : Coordinate) is
      type Score_Matrix is array (0 .. Height - 1, 1 .. Width) of Integer;
      Position_Queue : Coordinate_Queues.Queue;
      Cur, Next : Coordinate;
      Best_Score : Score_Matrix := [others => [others => 10000 * Width * Height]];
      Best_Coords : Score_Matrix := [others => [others => 0]];
      Best_Solution : Integer;

      procedure Mark_Best_Solution (Coord : Coordinate) is
         I, J : Integer;
      begin
         for Hash of Coord.Visited loop
            Coord_Unhash (Hash, I, J);
            Best_Coords (I, J) := @ + 1;
         end loop;
      end Mark_Best_Solution;

      procedure Print_Best_Coords_Count is
         Count : Integer := 1; --  for the start coordinate
      begin
         for I in Best_Coords'Range (1) loop
            for J in Best_Coords'Range (2) loop
               if Best_Coords (I, J) /= 0 then
                  Count := @ + 1;
               end if;
            end loop;
         end loop;
         Put_Line ("Nr tiles on best paths =" & Count'Image);
      end Print_Best_Coords_Count;

   begin
      Position_Queue.Enqueue ((Start.I, Start.J,  0,  1, Start.Distance, Start.Visited));
      while Position_Queue.Current_Use > 0 loop
         Position_Queue.Dequeue (Cur);
         if Cur.I = Finish.I and then Cur.J = Finish.J then
            Best_Solution := Cur.Distance;
            Put_Line ("Best solution =" & Best_Solution'Image);
            Mark_Best_Solution (Cur);
            while Position_Queue.Current_Use > 0 loop
               Position_Queue.Dequeue (Cur);
               exit when Cur.Distance /= Best_Solution;
               Mark_Best_Solution (Cur);
            end loop;
            Print_Best_Coords_Count;
            return;
         end if;
         --  Continue in same direction
         Next := (Cur.I + Cur.DI, Cur.J + Cur.DJ, Cur.DI, Cur.DJ, Cur.Distance + 1, Cur.Visited);
         if Is_Legal_Position (Next, Cur.Visited) and then Next.Distance <= Best_Score (Next.I, Next.J) + 1000 then
            Next.Visited.Insert (Coord_Hash (Next));
            Best_Score (Next.I, Next.J) := Next.Distance;
            Position_Queue.Enqueue (Next);
         end if;
         --  Rotate right
         Next := (Cur.I + Cur.DJ, Cur.J - Cur.DI, Cur.DJ, -Cur.DI, Cur.Distance + 1001, Cur.Visited);
         if Is_Legal_Position (Next, Cur.Visited) and then Next.Distance <= Best_Score (Next.I, Next.J) + 1000 then
            Next.Visited.Insert (Coord_Hash (Next));
            Best_Score (Next.I, Next.J) := Next.Distance;
            Position_Queue.Enqueue (Next);
         end if;
         --  Rotate left
         Next := (Cur.I - Cur.DJ, Cur.J + Cur.DI, -Cur.DJ, Cur.DI, Cur.Distance + 1001, Cur.Visited);
         if Is_Legal_Position (Next, Cur.Visited) and then Next.Distance <= Best_Score (Next.I, Next.J) + 1000 then
            Next.Visited.Insert (Coord_Hash (Next));
            Best_Score (Next.I, Next.J) := Next.Distance;
            Position_Queue.Enqueue (Next);
         end if;
      end loop;
      raise Bad_Input;
   end Shortest_Path;

begin
   Read_Document;
   Shortest_Path (Find_Position ('S'), Find_Position ('E'));
end Main;
