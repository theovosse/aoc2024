with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;

procedure Main is

   package Strings2 is new Generic_Bounded_Length (2);
   type Computer_Name_Type is new Strings2.Bounded_String;

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

   function Completely_Connected (C : Computer_Name_Type; Reachable : Computer_Name_Sets.Set) return Boolean is
   begin
      return (for all R of Reachable => Connections (R).Contains (C));
   end Completely_Connected;

   --  Another flooding algorithm
   function Get_Clique (C : Computer_Name_Type) return Computer_Name_Sets.Set is
      Reachable : Computer_Name_Sets.Set;
      type Edge_Range is new Integer range 0 .. 1;
      Edge : array (Edge_Range) of Computer_Name_Sets.Set;
      Edge_Idx : Edge_Range := 0;
   begin
      Edge (Edge_Idx).Insert (C);
      Reachable.Insert (C);
      while not Edge (Edge_Idx).Is_Empty loop
         Edge_Idx := 1 - Edge_Idx;
         Edge (Edge_Idx).Clear;
         for C_Edge of Edge (1 - Edge_Idx) loop
            for C_Edge_Next of Connections (C_Edge) loop
               if not Reachable.Contains (C_Edge_Next) and then Completely_Connected (C_Edge_Next, Reachable) then
                  Edge (Edge_Idx).Insert (C_Edge_Next);
                  Reachable.Insert (C_Edge_Next);
               end if;
            end loop;
         end loop;
      end loop;
      return Reachable;
   end Get_Clique;

   procedure Traverse is
      Clique, Max_Clique : Computer_Name_Sets.Set;
   begin
      for C1_Cursor in Connections.Iterate loop
         Clique := Get_Clique (Key (C1_Cursor));
         if Clique.Length > Max_Clique.Length then
            Max_Clique := Clique;
         end if;
      end loop;
      for C of Max_Clique loop
         Put (NS (C)); Put (',');
      end loop;
      Put_Line ("");
   end Traverse;

begin
   Read_Document;
   Traverse;
end Main;
