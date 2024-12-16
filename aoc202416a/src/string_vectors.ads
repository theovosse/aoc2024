with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package String_Vectors is new
   Ada.Containers.Vectors
      (Index_Type  => Natural,
       Element_Type => Unbounded_String);