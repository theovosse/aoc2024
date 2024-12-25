with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Int_Vectors;

package Pieces is

   type Piece is record
      Height : Natural;
      Slots : Int_Vectors.Vector;
   end record;

   package Piece_Vectors is new Ada.Containers.Vectors (Positive, Piece);

end Pieces;