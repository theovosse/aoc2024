with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
--  with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Command_Line;

procedure Main is

   Max : constant Positive := Positive'Value (Ada.Command_Line.Argument (1));
   Max_Nr_Lines : constant Natural := Natural'Value (Ada.Command_Line.Argument (2));

   type Address_Range is new Natural range 0 .. Max - 1;
   Max_Addr : constant Address_Range := Address_Range (Max - 1);

   type Coordinate is record
      I, J : Address_Range;
   end record;

   type Memory_Type is array (Address_Range, Address_Range) of Boolean;

   --  package Addr_Vectors is new Ada.Containers.Indefinite_Vectors (Natural, Coordinate);

   package Position_List is new Ada.Containers.Doubly_Linked_Lists (Coordinate);

   Bad_Document : exception;
   Corrupt : Memory_Type := [others => [others => False]];
   --  Address_List : Addr_Vectors.Vector;

   procedure Read_Document is
      I, J : Integer;
      Ch : Character;
      Nr_Lines_Read : Natural := 0;
   begin
      while not End_Of_File loop
         Get (I);
         --  Address_List.Append (Address_Range (N));
         Get_Immediate (Ch);
         if Ch /= ',' then
            raise Bad_Document with "no comma";
         end if;
         Get (J);
         --  Address_List.Append (Address_Range (N));
         Corrupt (Address_Range (I), Address_Range (J)) := True;
         Nr_Lines_Read := @ + 1;
         exit when End_Of_File or Nr_Lines_Read = Max_Nr_Lines;
         Get_Immediate (Ch);
      end loop;
   end Read_Document;

   procedure Solve is
      Cur_Pos_List, New_Pos_List : Position_List.List;
      Visited : Memory_Type := [others => [others => False]];
      Nr_Steps : Natural := 0;
      procedure Add (Pos : Coordinate) is
      begin
         if not Corrupt (Pos.I, Pos.J) and then not Visited (Pos.I, Pos.J) then
            New_Pos_List.Append (Pos);
            Visited (Pos.I, Pos.J) := True;
         end if;
      end Add;
      procedure Print_State is
      begin
         for I in Address_Range loop
            for J in Address_Range loop
               Put (if Corrupt (I, J) then "# " elsif Visited (I, J) then "O " else ". ");
            end loop;
            Put_Line ("");
         end loop;
      end Print_State;
   begin
      New_Pos_List.Append ((0, 0));
      Visited (0, 0) := True;
      while not New_Pos_List.Is_Empty loop
         Cur_Pos_List := New_Pos_List;
         New_Pos_List := Position_List.Empty_List;
         --  Put_Line (Cur_Pos_List.Length'Image);
         for Pos of Cur_Pos_List loop
            if Pos.I = Max_Addr and then Pos.J = Max_Addr then
               Print_State;
               Put_Line (Nr_Steps'Image);
               return;
            end if;
            if Pos.I < Max_Addr then
               Add ((Pos.I + 1, Pos.J));
            end if;
            if Pos.I > 0 then
               Add ((Pos.I - 1, Pos.J));
            end if;
            if Pos.J < Max_Addr then
               Add ((Pos.I, Pos.J + 1));
            end if;
            if Pos.J > 0 then
               Add ((Pos.I, Pos.J - 1));
            end if;
         end loop;
         Nr_Steps := @ + 1;
      end loop;
      Print_State;
      raise Bad_Document with "no solution";
   end Solve;

begin
   Read_Document;
   Solve;
end Main;
