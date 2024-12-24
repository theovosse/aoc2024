with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Gates is

   pragma Preelaborate;

   Bad_Operator : exception;

   type Gate is record
      Input1, Input2, Operator, Output : Unbounded_String;
   end record;

   package Gate_Vectors is new Ada.Containers.Vectors (Natural, Gate);

   function "=" (A, B : Gate_Vectors.Vector) return Boolean renames Gate_Vectors."=";

   package Gate_Vector_Vectors is new Ada.Containers.Vectors (Natural, Gate_Vectors.Vector);

   function Gate_Output (Operator : Unbounded_String; Input1, Input2 : Boolean) return Boolean;

end Gates;