with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Vectors;

package Position is

   type Position_Type is record
      I, J : Integer;
   end record;

   package Position_Vectors is new Ada.Containers.Vectors (Natural, Position_Type);

   function "=" (A, B : Position_Vectors.Vector) return Boolean renames Position_Vectors."=";

   package Node_To_Position_Lists is new Ada.Containers.Indefinite_Ordered_Maps
      (Character, Position_Vectors.Vector);

   Node_To_Position_List : Node_To_Position_Lists.Map;

   package Integer_Sets is new Ada.Containers.Indefinite_Ordered_Sets (Integer);

   function "=" (A, B : Integer_Sets.Set) return Boolean renames Integer_Sets."=";

   package Sparse_Maps is new Ada.Containers.Indefinite_Ordered_Maps
      (Integer, Integer_Sets.Set);

   Nr_Anti_Node_Positions : Integer := 0;

   Anti_Node_Positions : Sparse_Maps.Map;

end Position;