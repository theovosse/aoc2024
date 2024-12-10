with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Aoc202410b is

   ESC : constant Character := Character'Val (27);

   package Str_Vec is new Ada.Containers.Indefinite_Vectors
      (Natural, Unbounded_String);

   type Int_Mat is array (Natural range <>, Natural range <>) of Integer;

   Document : Str_Vec.Vector;

   procedure Read_Document is
   begin
      while not End_Of_File loop
         Document.Append (Get_Line);
      end loop;
   end Read_Document;

   function Sum_Paths (N, M : Natural) return Natural is
      Trails : array (0 .. N - 1, 1 .. M) of Integer :=
         (others => (others => 0));
      Trails_Condition : Character := ' ';

      function Trail_Pos (I, J : Integer) return Integer is
         (if 0 <= I and then I < N and then 1 <= J and then
             J <= M and then Element (Document (I), J) = Trails_Condition
          then Trails (I, J)
          else 0);

      Sum : Natural := 0;
   begin
      for I in 0 .. N - 1 loop
         for J in 1 .. M loop
            if Element (Document (I), J) = '0' then
               Trails (I, J) := 1;
            end if;
         end loop;
      end loop;
      for Height in Character range '1' .. '9' loop
         Trails_Condition := Character'Pred (Height);
         for I in 0 .. N - 1 loop
            for J in 1 .. M loop
               if Element (Document (I), J) = Height then
                  Trails (I, J) := @ +
                     Trail_Pos (I - 1, J) + Trail_Pos (I, J - 1) +
                     Trail_Pos (I + 1, J) + Trail_Pos (I, J + 1);
               end if;
            end loop;
         end loop;
      end loop;
      for I in 0 .. N - 1 loop
         for J in 1 .. M loop
            if Element (Document (I), J) = '9' then
               Sum := @ + Trails (I, J);
            end if;
         end loop;
      end loop;
      return Sum;
   end Sum_Paths;

begin
   Read_Document;
   Put_Line (Natural'Image (
      Sum_Paths (Integer (Document.Length), Length (Document (0)))));
end Aoc202410b;
