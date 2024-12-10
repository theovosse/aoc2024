with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;

procedure Aoc202410a is

   ESC : constant Character := Character'Val(27);

   package Str_Vec is new Ada.Containers.Indefinite_Vectors
      (Natural, Unbounded_String);

   package Int_Set is new Ada.Containers.Ordered_Sets (Integer);
   use Int_Set;

   Document : Str_Vec.Vector;

   procedure Read_Document is
   begin
      while not End_Of_File loop
         Document.Append (Get_Line);
      end loop;
   end Read_Document;

   type Int_Set_Mat is array (Natural range <>, Natural range <>)
      of Int_Set.Set;

   procedure Print_Trails (Trails : Int_Set_Mat; Highlight : Character) is
      Ch : Character;
   begin
      for I in Trails'Range (1) loop
         for J in Trails'Range (2) loop
            Ch := Element (Document (I), J);
            if Ch = Highlight then
               Put (ESC); Put ("[1m");
            else
               Put (ESC); Put ("[2m");
            end if;
            Put (Ch);
            Put (ESC); Put ("[0m");
         end loop;
         Put ("    ");
         for J in Trails'Range (2) loop
            if Element (Document (I), J) = Highlight then
               Put (Integer'Image (Integer (Trails (I, J).Length)));
            else
               Put (" .");
            end if;
         end loop;
         Put_Line ("");
      end loop;
      Put_Line ("");
   end Print_Trails;

   function Sum_Paths (N, M : Natural) return Natural is
      Trails : array (0 .. N - 1, 1 .. M) of Int_Set.Set;
      Trails_Condition : Character := ' ';

      function Pos_Idx (I, J : Integer) return Integer is
         (I * M + J);

      function Matching_Pos (I, J : Integer) return Boolean is
         (0 <= I and then I < N and then 1 <= J and then
          J <= M and then Element (Document (I), J) = Trails_Condition);

      Sum : Natural := 0;
   begin
      --  This bit starts trails only at the edge of the map. How stupid of me.
      --  for I in 0 .. N - 1 loop
      --     if Element (Document (I), 1) = '0' then
      --        Trails (I, 1).Insert (Pos_Idx (I, 1));
      --     end if;
      --     if Element (Document (I), M) = '0' then
      --        Trails (I, M).Insert (Pos_Idx (I, M));
      --     end if;
      --  end loop;
      --  for J in 2 .. M - 1 loop
      --     if Element (Document (0), J) = '0' then
      --        Trails (0, J).Insert (Pos_Idx (0, J));
      --     end if;
      --     if Element (Document (N - 1), J) = '0' then
      --        Trails (N - 1, J).Insert (Pos_Idx (N - 1, J));
      --     end if;
      --  end loop;
      --  Instead, trails can also start inside the map. How? No idea. Magic, I suppose.
      for I in 0 .. N - 1 loop
         for J in 1 .. M loop
            if Element (Document (I), J) = '0' then
               Trails (I, J).Insert (Pos_Idx (I, J));
            end if;
         end loop;
      end loop;
      Print_Trails (Int_Set_Mat (Trails), '0');
      for Height in Character range '1' .. '9' loop
         Trails_Condition := Character'Pred (Height);
         for I in 0 .. N - 1 loop
            for J in 1 .. M loop
               if Element (Document (I), J) = Height then
                  if Matching_Pos (I - 1, J) then
                     Trails (I, J) := @ or Trails (I - 1, J);
                  end if;
                  if Matching_Pos (I, J - 1) then
                     Trails (I, J) := @ or Trails (I, J - 1);
                  end if;
                  if Matching_Pos (I + 1, J) then
                     Trails (I, J) := @ or Trails (I + 1, J);
                  end if;
                  if Matching_Pos (I, J + 1) then
                     Trails (I, J) := @ or Trails (I, J + 1);
                  end if;
               end if;
            end loop;
         end loop;
         Print_Trails (Int_Set_Mat (Trails), Height);
      end loop;
      for I in 0 .. N - 1 loop
         for J in 1 .. M loop
            if Element (Document (I), J) = '9' then
               Sum := @ + Natural (Trails (I, J).Length);
            end if;
         end loop;
      end loop;
      return Sum;
   end Sum_Paths;

begin
   Read_Document;
   Put_Line (Natural'Image (
      Sum_Paths (Integer (Document.Length), Length (Document (0)))));
end Aoc202410a;
