with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;

procedure Main is

   package Strings2 is new Generic_Bounded_Length (2);
   type Computer_Name_Type is new Strings2.Bounded_String;

   --  type Computer_Name_Pair_Type is record
   --     A, B : Computer_Name_Type;
   --  end record;

   package Computer_Name_Sets is new Ada.Containers.Ordered_Sets (Computer_Name_Type);
   function "=" (A, B : Computer_Name_Sets.Set) return Boolean renames Computer_Name_Sets."=";
   package Connection_Maps is new Ada.Containers.Ordered_Maps (Computer_Name_Type, Computer_Name_Sets.Set);
   use Connection_Maps;

   function NS (N : Computer_Name_Type) return String is (To_String (N));

   function NS (N : Connection_Maps.Cursor) return String is (To_String (Key (N)));

   Bad_Document : exception;
   Connections : Map;

   procedure Add_Unidirectional (First, Second : Computer_Name_Type) is
   begin
      if Connections.Contains (First) then
         Connections (First).Insert (Second);
      else
         declare
            New_Name_Set : Computer_Name_Sets.Set;
         begin
            New_Name_Set.Insert (Second);
            Connections.Insert (First, New_Name_Set);
         end;
      end if;
   end Add_Unidirectional;

   procedure Add_Bidirectional (Pair : Unbounded_String) is
      Name1 : constant Computer_Name_Type := To_Bounded_String (Element (Pair, 1) & Element (Pair, 2));
      Name2 : constant Computer_Name_Type := To_Bounded_String (Element (Pair, 4) & Element (Pair, 5));
   begin
      if Name1 = Name2 then
         raise Bad_Document;
      end if;
      Add_Unidirectional (Name1, Name2);
      Add_Unidirectional (Name2, Name1);
   end Add_Bidirectional;

   procedure Read_Document is
   begin
      while not End_Of_File loop
         Add_Bidirectional (Get_Line);
      end loop;
   end Read_Document;

   procedure Traverse2 is
      Count : Natural := 0;
      C1 : Computer_Name_Type;
   begin
      for C1_Ptr in Connections.Iterate loop
         C1 := Key (C1_Ptr);
         for C2 of Element (C1_Ptr) loop
            if C1 < C2 then
               declare
                  C2M : constant Computer_Name_Sets.Set := Connections (C2);
               begin
                  for C3 of C2M loop
                     if C1 < C3 and then C2 < C3 and then Element (C1_Ptr).Contains (C3) and then
                        (Element (C1, 1) = 't' or else Element (C2, 1) = 't' or else Element (C3, 1) = 't') then
                        Put_Line (NS (C1) & ' ' & NS (C2) & ' ' & NS (C3));
                        Count := @ + 1;
                     end if;
                  end loop;
               end;
            end if;
         end loop;
      end loop;
      Put_Line (Count'Image);
   end Traverse2;

begin
   Read_Document;
   Traverse2;
end Main;
