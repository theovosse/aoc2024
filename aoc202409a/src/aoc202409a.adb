with Ada.Text_IO; use Ada.Text_IO;

procedure Aoc202409a is

   Disk_Map : array (0 .. 65535) of Short_Short_Integer;
   Disk_Map_Len : Natural := 0;
   Empty : constant Short_Integer := -1;
   Disk_Content : array (0 .. 65535 * 9) of Short_Integer;
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

   procedure Fill_Disk_Content is
      I : Natural := 0;
      File_Id : Short_Integer := 0;
   begin
      while I < Disk_Map_Len loop
         for J in 1 .. Disk_Map (I) loop
            Disk_Content (Disk_Content_Len) := File_Id;
            Disk_Content_Len := Disk_Content_Len + 1;
         end loop;
         File_Id := File_Id + 1;
         I := I + 1;
         if I < Disk_Map_Len then
            for J in 1 .. Disk_Map (I) loop
               Disk_Content (Disk_Content_Len) := Empty;
               Disk_Content_Len := Disk_Content_Len + 1;
            end loop;
            I := I + 1;
         end if;
      end loop;
   end Fill_Disk_Content;

   procedure Compact_Disk is
      Left : Natural := 0;
      Right : Natural := Disk_Content_Len - 1;
   begin
      while Left < Right loop
         if Disk_Content (Right) = Empty then
            Right := Right - 1;
         elsif Disk_Content (Left) = Empty then
            Disk_Content (Left) := Disk_Content (Right);
            Left := Left + 1;
            Right := Right - 1;
         else
            Left := Left + 1;
         end if;
      end loop;
      Disk_Content_Len := Right + 1;
   end Compact_Disk;

   function Check_Sum return Long_Integer is
      Sum : Long_Integer := 0;
   begin
      for I in 0 .. Disk_Content_Len - 1 loop
         Sum := Sum + Long_Integer (I) * Long_Integer (Disk_Content (I));
      end loop;
      return Sum;
   end Check_Sum;

begin
   Read_Document;
   Fill_Disk_Content;
   Compact_Disk;
   Put_Line (Long_Integer'Image (Check_Sum));
end Aoc202409a;
