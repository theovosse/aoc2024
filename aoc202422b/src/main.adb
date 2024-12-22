pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is

   type Rnd_Type is mod 2**64;

   type Diff_Range is new Integer range -18 .. 18;

   type Sequence_Count_Type is
      array (Diff_Range, Diff_Range, Diff_Range, Diff_Range) of Short_Integer
      with Default_Component_Value => 0;
   type Sequence_Check_Type is
      array (Diff_Range, Diff_Range, Diff_Range, Diff_Range) of Boolean;
   pragma Pack (Sequence_Check_Type);

   Sequence_Count : Sequence_Count_Type;
   Sequence_Check : Sequence_Check_Type;

   procedure Clear_Sequence_Check is
   begin
      for I in Diff_Range'Range loop
         for J in Diff_Range'Range loop
            for K in Diff_Range'Range loop
               for L in Diff_Range'Range loop
                  Sequence_Check (I, J, K, L) := False;
               end loop;
            end loop;
         end loop;
      end loop;
   end Clear_Sequence_Check;

   function Next_Secret (N : Rnd_Type) return Rnd_Type is
      A : constant Rnd_Type := N * 64;
      N1 : constant Rnd_Type := N xor A;
      N2 : constant Rnd_Type := N1 and 16#ffffff#;
      D : constant Rnd_Type := N2 / 32;
      N3 : constant Rnd_Type := D xor N2;
      N4 : constant Rnd_Type := N3 and 16#ffffff#;
      G : constant Rnd_Type := N4 * 2048;
      N5 : constant Rnd_Type := N4 xor G;
      N6 : constant Rnd_Type := N5 and 16#ffffff#;
   begin
      return N6;
   end Next_Secret;

   procedure Count_Next_Secret_Sequences (Secret : Integer; Nr_Reps : Short_Integer) is
      Result : Rnd_Type := Rnd_Type (Secret);
      Outcome : Integer;
      Prev_Outcome : Integer := Secret mod 10;
      I, J, K, L : Diff_Range := 0;
   begin
      Clear_Sequence_Check;
      for Rep in 1 .. Nr_Reps loop
         Result := Next_Secret (Result);
         Outcome := Integer (Result) mod 10;
         I := J; J := K; K := L; L := Diff_Range (Outcome - Prev_Outcome);
         Prev_Outcome := Outcome;
         if Rep >= 4 and then not Sequence_Check (I, J, K, L) then
            Sequence_Check (I, J, K, L) := True;
            Sequence_Count (I, J, K, L) := @ + Short_Integer (Outcome);
         end if;
      end loop;
   end Count_Next_Secret_Sequences;

   function Max_Sequence_Count return Short_Integer is
      Max, V : Short_Integer := 0;
   begin
      for I in Diff_Range'Range loop
         for J in Diff_Range'Range loop
            for K in Diff_Range'Range loop
               for L in Diff_Range'Range loop
                  V := Sequence_Count (I, J, K, L);
                  if V > Max then
                     Max := V;
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;
      return Max;
   end Max_Sequence_Count;

   procedure Process_Document is
      N : Integer;
   begin
      while not End_Of_File loop
         Get (N);
         Count_Next_Secret_Sequences (N, 2000);
      end loop;
      Put_Line (Max_Sequence_Count'Image);
   end Process_Document;

begin
   Process_Document;
end Main;
