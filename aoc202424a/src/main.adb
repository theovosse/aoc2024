pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with String_Vectors; use String_Vectors;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Containers; use Ada.Containers;
with String_To_Bool_Maps; use String_To_Bool_Maps;
with String_To_Int_Maps; use String_To_Int_Maps;
with Gates; use Gates;

procedure Main is

   Bad_Document : exception;
   Space_Separator : constant Character_Set := To_Set (' ');
   Inputs : String_To_Bool_Maps.Map;
   Gate_List : Gate_Vectors.Vector;
   Gate_Order : String_To_Int_Maps.Map;
   Operation_List : Gate_Vector_Vectors.Vector;

   function Split_Line (Line : String) return String_Vectors.Vector is
      List : String_Vectors.Vector;
      Pos : Natural := Line'First;
      Start, Finish : Natural;
   begin
      while Pos <= Line'Length loop
         Find_Token (Line, Space_Separator, Pos, Outside, Start, Finish);
         if Finish = 0 then
            List.Append (To_Unbounded_String (Line (Start .. Line'Last)));
            exit;
         end if;
         List.Append (To_Unbounded_String (Line (Start .. Finish)));
         Pos := Finish + 1;
      end loop;
      return List;
   end Split_Line;

   function Add_Input (Line : Unbounded_String) return Boolean is
      Pos : constant Natural := Index (Line, ": ");
   begin
      if Line = "" then
         return False;
      end if;
      if Pos < 2 or else Pos /= Length (Line) - 2 then
         raise Bad_Document;
      end if;
      Inputs.Insert (To_Unbounded_String (Slice (Line, 1, Pos - 1)),
                     Element (Line, Length (Line)) = '1');
      return True;
   end Add_Input;

   procedure Add_Gate (Descr : String_Vectors.Vector) is
   begin
      if Descr.Length /= 5 then
         raise Bad_Document;
      end if;
      Gate_List.Append (Gate'(Input1 => Descr (0), Input2 => Descr (2), Operator => Descr (1), Output => Descr (4)));
   end Add_Gate;

   function Max (A, B : Natural) return Natural is
      (if A > B then A else B);

   --  The order of a gate is the highest order of its inputs plus 1.
   --  Then evaluation can take place in order of gate.
   --  As it turns out, this is overkill for problem 2.
   procedure Order_Gates is
      Change : Boolean;
      Order : Natural;
   begin
      --  Inputs have order 0
      for Input in Inputs.Iterate loop
         Gate_Order.Insert (Key (Input), 0);
      end loop;
      --  Add operators when their inputs have a known order until all has been resolved (there are no loops)
      loop
         Change := False;
         for Gate of Gate_List loop
            if not Gate_Order.Contains (Gate.Output) and then
               Gate_Order.Contains (Gate.Input1) and then
               Gate_Order.Contains (Gate.Input2)
            then
               Order := Max (Gate_Order (Gate.Input1), Gate_Order (Gate.Input2)) + 1;
               Gate_Order.Insert (Gate.Output, Order);
               Change := True;
               while Integer (Operation_List.Length) < Order + 1 loop
                  Operation_List.Append (Gate_Vectors.Empty);
               end loop;
               Operation_List (Order).Append (Gate);
            end if;
         end loop;
         exit when not Change;
      end loop;
   end Order_Gates;

   procedure Propagate_Gates is
      Output : Boolean;
   begin
      for Operations of Operation_List loop
         for Gate of Operations loop
            Output := Gate_Output (Gate.Operator, Inputs (Gate.Input1), Inputs (Gate.Input2));
            Inputs.Insert (Gate.Output, Output);
         end loop;
      end loop;
   end Propagate_Gates;

   procedure Read_Document is
   begin
      while Add_Input (Get_Line) loop
         null;
      end loop;
      while not End_Of_File loop
         Add_Gate (Split_Line (Get_Line));
      end loop;
   end Read_Document;

   function Solution return Long_Integer is
      Value : Long_Integer := 0;
      Power2 : Long_Integer := 1;
   begin
      for Input in Inputs.Iterate loop
         if Element (Key (Input), 1) = 'z' then
            if Element (Input) then
               Value := @ + Power2;
            end if;
            Power2 := @ * 2;
         end if;
      end loop;
      return Value;
   end Solution;

begin
   Read_Document;
   Order_Gates;
   Propagate_Gates;
   Put_Line (Solution'Image);
end Main;
