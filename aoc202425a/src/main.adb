pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Int_Vectors;
with Pieces; use Pieces;

procedure Main is

   Keys, Locks : Piece_Vectors.Vector;

   procedure Read_Document is
      Line : Unbounded_String;
      procedure Read_Lock is
         Width : constant Integer := Length (Line);
         Lock : Int_Vectors.Vector := Int_Vectors.To_Vector (0, Count_Type (Width));
         Height : Natural := 0;
      begin
         while not End_Of_File loop
            Line := Get_Line;
            exit when Length (Line) = 0;
            Height := Height + 1;
            for I in 1 .. Width loop
               if Element (Line, I) = '#' then
                  Lock.Replace_Element (I, Height);
               end if;
            end loop;
         end loop;
         Locks.Append (Piece'(Height, Lock));
      end Read_Lock;
      procedure Read_Key is
         Width : constant Integer := Length (Line);
         Key : Int_Vectors.Vector := Int_Vectors.To_Vector (0, Count_Type (Width));
         Height : Natural := 0;
      begin
         while not End_Of_File loop
            Line := Get_Line;
            exit when Length (Line) = 0;
            Height := Height + 1;
            for I in 1 .. Width loop
               if Element (Line, I) = '#' then
                  Key.Replace_Element (I, Key (I) + 1);
               end if;
            end loop;
         end loop;
         for I in 1 .. Width loop
            Key.Replace_Element (I, Key (I) - 1);
         end loop;
         Keys.Append (Piece'(Height, Key));
      end Read_Key;
   begin
      while not End_Of_File loop
         Line := Get_Line;
         if Element (Line, 1) = '#' then
            Read_Lock;
         else
            Read_Key;
         end if;
      end loop;
   end Read_Document;

   function Fit (Key, Lock : Piece) return Boolean is
   begin
      for I in 1 .. Natural (Key.Slots.Length) loop
         if Key.Slots (I) + Lock.Slots (I) >= Key.Height then
            return False;
         end if;
      end loop;
      return True;
   end Fit;

   procedure Check_Fit is
      Count : Natural := 0;
   begin
      for L of Locks loop
         for K of Keys loop
            if Fit (L, K) then
               Count := @ + 1;
            end if;
         end loop;
      end loop;
      Put_Line (Count'Image);
   end Check_Fit;

begin
   Read_Document;
   Check_Fit;
end Main;
