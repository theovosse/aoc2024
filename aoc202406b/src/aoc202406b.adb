with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with String_Vectors;

procedure Aoc202406b is

   Document : String_Vectors.Vector;
   Pos_I, Pos_J : Integer := 0;
   Delta_I, Delta_J : Integer := 0;

   procedure Read_Document is
   begin
      while not End_Of_File loop
         declare
            Line : constant Unbounded_String :=
                Ada.Text_IO.Unbounded_IO.Get_Line;
         begin
            Document.Append (Line);
         end;
      end loop;
   end Read_Document;

   procedure Find_Direction is
   begin
      case Element (Document (Pos_I), Pos_J) is
         when '^' => Delta_I := -1;
         when 'v' => Delta_I := 1;
         when '>' => Delta_J := 1;
         when '<' => Delta_J := -1;
         when others => null;
      end case;
   end Find_Direction;

   procedure Find_Initial_Position is
      Bad_Input : exception;
   begin
      for I in 0 .. Natural (Document.Length) - 1 loop
         declare
            Line_I : constant Unbounded_String := Document (I);
         begin
            for J in 1 .. Length (Line_I) loop
               case Element (Line_I, J) is
                  when '^' | '<' | '>' | 'v' =>
                     Pos_I := I;
                     Pos_J := J;
                     Find_Direction;
                     return;
                  when others => null;
               end case;
            end loop;
         end;
      end loop;
      raise Bad_Input with "no initial position";
   end Find_Initial_Position;

   --  Placing an obstacle at (Next_I, Next_J) coming from (Pos_I, Pos_J)
   --  creates a loop in the current path iff
   --  - it is within bounds
   --  - it has not been visited and does not contain an obstacle already
   --  - turning leads to a point with an obstacle
   --  - 
   function Creates_Loop (I, J : Integer) return Boolean is
   begin

   end Creates_Loop;

   procedure Walk_Until_Exit is
      Next_I, Next_J, Tmp : Integer;
      Assumption_Error : exception;
   begin
      while 0 <= Pos_I and then Pos_I < Integer (Document.Length) and then
            1 <= Pos_J and then Pos_J <= Length (Document (Pos_I))
      loop
         if Element (Document (Pos_I), Pos_J) /= 'X' then
            Replace_Element (Document (Pos_I), Pos_J, 'X');
         end if;
         Next_I := Pos_I + Delta_I;
         Next_J := Pos_J + Delta_J;
         exit when Next_I < 0 or else Next_I >= Integer (Document.Length) or else
                   Next_J < 1 or else Next_J > Length (Document (Next_I));
         if Element (Document (Next_I), Next_J) = '#' then
            --  90 deg rotation; assume there is no obstacle in the next step
            Tmp := Delta_J;
            Delta_J := -Delta_I;
            Delta_I := Tmp;
            Next_I := Pos_I + Delta_I;
            Next_J := Pos_J + Delta_J;
            if 0 <= Next_I and then Next_I < Integer (Document.Length) and then
               1 <= Next_J and then Next_J <= Length (Document (Next_I)) and then
               Element (Document (Pos_I), Pos_J) = '#'
            then
               raise Assumption_Error with "bad assumption";
            end if;
         end if;
         Pos_I := Next_I;
         Pos_J := Next_J;
      end loop;
   end Walk_Until_Exit;

begin
   Read_Document;
   Find_Initial_Position;
   Walk_Until_Exit;
end Aoc202406b;
