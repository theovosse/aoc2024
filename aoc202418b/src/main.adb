with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Command_Line;

procedure Main is

   Max : constant Positive := Positive'Value (Ada.Command_Line.Argument (1));

   type Address_Range is new Natural range 0 .. Max - 1;
   Max_Addr : constant Address_Range := Address_Range (Max - 1);

   type Coordinate is record
      I, J : Address_Range;
   end record;

   type Memory_Type is array (Address_Range, Address_Range) of Boolean;

   package Addr_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Coordinate);

   package Position_List is new Ada.Containers.Doubly_Linked_Lists (Coordinate);

   Bad_Document : exception;
   Address_List : Addr_Vectors.Vector;

   procedure Read_Document is
      X, Y : Integer;
      Ch : Character;
   begin
      while not End_Of_File loop
         Get (X);
         Get_Immediate (Ch);
         if Ch /= ',' then
            raise Bad_Document with "no comma";
         end if;
         Get (Y);
         Address_List.Append (Coordinate'(Address_Range (Y), Address_Range (X)));
         exit when End_Of_File;
         Get_Immediate (Ch);
      end loop;
   end Read_Document;

   function Solvable (Max_Nr_Corrupt : Natural) return Boolean is
      Cur_Pos_List, New_Pos_List : Position_List.List;
      Corrupt : Memory_Type := [others => [others => False]];
      Visited : Memory_Type := [others => [others => False]];
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
      for I in 1 .. Max_Nr_Corrupt loop
         Corrupt (Address_List (I).I, Address_List (I).J) := True;
      end loop;
      New_Pos_List.Append ((0, 0));
      Visited (0, 0) := True;
      while not New_Pos_List.Is_Empty loop
         Cur_Pos_List := New_Pos_List;
         New_Pos_List := Position_List.Empty_List;
         for Pos of Cur_Pos_List loop
            if Pos.I = Max_Addr and then Pos.J = Max_Addr then
               return True;
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
      end loop;
      return False;
   end Solvable;

   procedure Bin_Split is
      N : constant Natural := Natural (Address_List.Length);
      Low : Natural := 0;
      High : Natural := N;
      Pos : Natural;
   begin
      while Low < High - 1 loop
         Pos := (Low + High) / 2;
         if Solvable (Pos) then
            Low := Pos;
         else
            High := Pos;
         end if;
      end loop;
      Put_Line (Address_List (High).J'Image & "," & Address_List (High).I'Image);
   end Bin_Split;

begin
   Read_Document;
   Bin_Split;
end Main;
