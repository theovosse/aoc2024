with Ada.Text_IO; use Ada.Text_IO;

procedure Aoc202409b is

   type Digit_Type is range 0 .. 9;
   type Size_Type is range 1 .. 9;
   type File_Id_Type is new Short_Integer;

   Disk_Map : array (0 .. 65535) of Digit_Type;
   Disk_Map_Len : Natural := 0;
   Disk_Content : array (0 .. 65535 * 9) of File_Id_Type;
   Disk_Content_Len : Natural := 0;

   procedure Read_Document is
      Ch : Character;
   begin
      while not End_Of_File loop
         Get (Ch);
         Disk_Map (Disk_Map_Len) := Character'Pos (Ch) - 48;
         Disk_Map_Len := Disk_Map_Len + 1;
      end loop;
   end Read_Document;

   function Is_Disk_Content_Empty (I : Natural) return Boolean is
      (Disk_Content (I) < 0);

   procedure Fill_Disk_Content is
      I : Natural := 0;
      File_Len : Digit_Type;
      File_Id : File_Id_Type := 0;
   begin
      while I < Disk_Map_Len loop
         File_Len := Disk_Map (I);
         for J in 1 .. File_Len loop
            Disk_Content (Disk_Content_Len) := File_Id;
            Disk_Content_Len := Disk_Content_Len + 1;
         end loop;
         I := I + 1;
         File_Id := File_Id + 1;
         if I < Disk_Map_Len then
            File_Len := Disk_Map (I);
            for J in 1 .. File_Len loop
               Disk_Content (Disk_Content_Len) := -File_Id_Type (File_Len);
               Disk_Content_Len := Disk_Content_Len + 1;
            end loop;
            I := I + 1;
         end if;
      end loop;
   end Fill_Disk_Content;

   No_Such_Position : constant Natural := 0;  --  note: 0 can never be an empty position

   function Get_File_Size (End_Pos : Natural) return Size_Type is
      File_Id : constant File_Id_Type := Disk_Content (End_Pos);
      Size : Size_Type := 1;
      Pos : Natural := End_Pos;
   begin
      while Pos > 0 and then Disk_Content (Pos - 1) = File_Id loop
         Pos := Pos - 1;
         Size := Size + 1;
      end loop;
      return Size;
   end Get_File_Size;

   --  This function can be made more efficient by first scanning Disk_Content and
   --  chaining all empty slots, and updating it and all that, but this is easier to write.
   function Find_First_Empty_Pos (Size : Size_Type) return Natural is
      File_Id_Empty_Size : constant File_Id_Type := -File_Id_Type (Size);
   begin
      for I in 0 .. Disk_Content_Len - 1 loop
         if Disk_Content (I) <= File_Id_Empty_Size then
            return I;
         end if;
      end loop;
      return No_Such_Position;
   end Find_First_Empty_Pos;

   procedure Move (End_Src, Start_Dest : Natural; Size : Size_Type) is
      Orig_Empty : constant File_Id_Type := -Disk_Content (Start_Dest);
      New_Empty : constant File_Id_Type := -(Orig_Empty - File_Id_Type (Size));
   begin
      for I in 0 .. Natural (Size) - 1 loop
         Disk_Content (Start_Dest + I) := Disk_Content (End_Src - I);
         Disk_Content (End_Src - I) := -File_Id_Type (Size); --  There's no need to combine empty chunks, since we're never going back towards the end
      end loop;
      for I in Natural (Size) .. Natural (Orig_Empty) - 1 loop
         Disk_Content (Start_Dest + I) := New_Empty;
      end loop;
   end Move;

   procedure Compact_Disk is
      Right : Natural := Disk_Content_Len - 1;
      --  While Shrink_Limit is No_Such_Position, Right is the last position with
      --  content. When Right moves back over content that cannot be moved,
      --  Shrink_Limit is set to the boundary.
      Shrink_Limit : Natural := No_Such_Position;
      Empty_Pos : Natural;
      Size : Size_Type;
      RV : File_Id_Type;
   begin
      while Right > 0 loop
         RV := Disk_Content (Right);
         if not Is_Disk_Content_Empty (Right) then
            Size := Get_File_Size (Right);
            Empty_Pos := Find_First_Empty_Pos (Size);
            if Empty_Pos /= No_Such_Position and then Empty_Pos < Right then
               Move (Right, Empty_Pos, Size);
            else
               if Shrink_Limit = No_Such_Position then
                  Shrink_Limit := Right + 1; --  can't shrink further
               end if;
            end if;
         end if;
         Right := Right - 1;
         while Right > 0 and then Disk_Content (Right) = RV loop
            Right := Right - 1;
         end loop;
      end loop;
      Disk_Content_Len :=
         (if Shrink_Limit = No_Such_Position then Right + 1 else Shrink_Limit);
   end Compact_Disk;

   function Check_Sum return Long_Integer is
      Sum : Long_Integer := 0;
   begin
      for I in 0 .. Disk_Content_Len - 1 loop
         if not Is_Disk_Content_Empty (I) then
            Sum := Sum + Long_Integer (I) * Long_Integer (Disk_Content (I));
         end if;
      end loop;
      return Sum;
   end Check_Sum;

begin
   Read_Document;
   Fill_Disk_Content;
   Compact_Disk;
   Put_Line (Long_Integer'Image (Check_Sum));
end Aoc202409b;
