pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;

procedure Aoc202412a is

   package Str_Vec is new Ada.Containers.Indefinite_Vectors (Natural, Unbounded_String);

   package Nat_Vec is new Ada.Containers.Indefinite_Vectors (Natural, Natural);

   function "=" (A, B : Nat_Vec.Vector) return Boolean renames Nat_Vec."=";

   package Nat_Mat is new Ada.Containers.Indefinite_Vectors (Natural, Nat_Vec.Vector);

   Document : Str_Vec.Vector;
   Width : Integer := 0;
   Height : Integer := 0;
   Area : Nat_Mat.Vector;

   procedure Read_Document is
   begin
      while not End_Of_File loop
         Document.Append (Get_Line);
      end loop;
      Width := Length (Document (0));
      Height := Integer (Document.Length);
   end Read_Document;

   --  function Position_To_Int (I, J : Integer) return Integer is (I * Width + J - 1);

   --  procedure Int_To_Position (Pos : Integer; I, J : out Integer) is
   --  begin
   --     I := Pos / Width;
   --     J := Pos mod Width + 1;
   --  end Int_To_Position;

   function Is_In_Bounds (I, J : Integer) return Boolean is
      (0 <= I and then I < Height and then 1 <= J and then J <= Width);

   --  Return the letter at position I, J, or a space when it is out of bounds.
   --  A space cannot match a crop, so Perimiter will see it as different.
   function Crop_At (I, J : Integer) return Character is
      (if Is_In_Bounds (I, J) then Element (Document (I), J) else ' ');

   --  function Area_At (I0, J0 : Integer) return Natural is
   --     Crop : constant Character := Element (Document (I0), J0);
   --     Crop_Positions : Int_Set.Set;
   --     Position_Queue : Queue;
   --     Next_Position : Integer;
   --     I, J : Integer;
   --  begin
   --     Enqueue (Position_Queue, Position_To_Int (I0, J0));
   --     while Integer (Current_Use (Position_Queue)) /= 0 loop
   --        Dequeue (Position_Queue, Next_Position);
   --        Int_To_Position (Next_Position, I, J);
   --        if Is_In_Bounds (I, J) and then not Crop_Positions.Contains (Next_Position) then
   --           Crop_Positions.Insert (Next_Position);
   --           Enqueue (Position_Queue, Position_To_Int (I - 1, J));
   --           Enqueue (Position_Queue, Position_To_Int (I + 1, J));
   --           Enqueue (Position_Queue, Position_To_Int (I, J - 1));
   --           Enqueue (Position_Queue, Position_To_Int (I, J + 1));
   --        end if;
   --     end loop;
   --     return Natural (Crop_Positions.Length);
   --  end Area_At;

   --  procedure Determine_Area is
   --     Row : Nat_Vec.Vector;
   --  begin
   --     for I in 0 .. Height - 1 loop
   --        for J in 1 .. Width loop
   --           Row.Append (Area_At (I, J));
   --        end loop;
   --        Area.Append (Row);
   --     end loop;
   --  end Determine_Area;

   procedure Determine_Area is
      type Nat_Mat2 is array (0 .. Height - 1, 1 .. Width) of Natural;
      Area_Index : Nat_Mat2;
      Index_Count : array (0 .. Height * Width) of Natural := [others => 0];
      Idx : Natural := 0;
      Crop : Character;
      Changed : Boolean;
      procedure Replace_Area_Index (To : Natural; From : Natural) is
      begin
         for I in 0 .. Height - 1 loop
            for J in 1 .. Width loop
               if Area_Index (I, J) = From then
                  Area_Index (I, J) := To;
               end if;
            end loop;
         end loop;
      end Replace_Area_Index;
      procedure Print_Mat (Text : String; M : Nat_Mat2) is
      begin
         Put_Line (Text);
         for I in 0 .. Height - 1 loop
            for J in 1 .. Width loop
               Ada.Integer_Text_IO.Put (M (I, J), 3);
            end loop;
            Put_Line ("");
         end loop;
      end Print_Mat;
   begin
      --  Give each cell its own index
      for I in 0 .. Height - 1 loop
         for J in 1 .. Width loop
            Area_Index (I, J) := Idx;
            Idx := Idx + 1;
         end loop;
      end loop;
      --  Assign each cell the index of the cell it touches 1 left or 1 up
      --  if it has the same 'crop'. In case of ambiguity, replace the area
      --  touching left and all with the same value.
      for I in 0 .. Height - 1 loop
         for J in 1 .. Width loop
            Crop := Crop_At (I, J);
            if I > 0 and then Crop_At (I - 1, J) = Crop then
               Area_Index (I, J) := Area_Index (I - 1, J);
               Changed := True;
            else
               Changed := False;
            end if;
            if J > 1 and then Crop_At (I, J - 1) = Crop then
               if Area_Index (I, J) /= Area_Index (I, J - 1) then
                  if Changed then
                     Replace_Area_Index (Area_Index (I, J), Area_Index (I, J - 1));
                  else
                     Area_Index (I, J) := Area_Index (I, J - 1);
                  end if;
               end if;
            end if;
         end loop;
      end loop;
      Print_Mat ("AREAS", Area_Index);
      --  Count the frequency of the indices
      for I in 0 .. Height - 1 loop
         for J in 1 .. Width loop
            Index_Count (Area_Index (I, J)) := @ + 1;
         end loop;
      end loop;
      --  Copy the areal of each index to `Area`
      for I in 0 .. Height - 1 loop
         declare
            Row : Nat_Vec.Vector;
         begin
            for J in 1 .. Width loop
               Row.Append (Index_Count (Area_Index (I, J)));
            end loop;
            Area.Append (Row);
         end;
      end loop;
   end Determine_Area;

   function Perimiter_Price_At (I, J : Integer) return Natural is
      Crop : constant Character := Crop_At (I, J);
      N : Natural := 0;
   begin
      if Crop_At (I - 1, J) /= Crop then
         N := N + 1;
      end if;
      if Crop_At (I + 1, J) /= Crop then
         N := N + 1;
      end if;
      if Crop_At (I, J - 1) /= Crop then
         N := N + 1;
      end if;
      if Crop_At (I, J + 1) /= Crop then
         N := N + 1;
      end if;
      return N;
   end Perimiter_Price_At;

   function Area_Price_At (I, J : Integer) return Natural is
   begin
      return Area (I) (J - 1);
   end Area_Price_At;

   function Perimiter_Price return Natural is
      N : Natural := 0;
   begin
      for I in 0 .. Height - 1 loop
         for J in 1 .. Width loop
            N := N + Perimiter_Price_At (I, J) * Area_Price_At (I, J);
         end loop;
      end loop;
      return N;
   end Perimiter_Price;

begin
   Read_Document;
   Determine_Area;
   Put_Line (Natural'Image (Perimiter_Price));
end Aoc202412a;
