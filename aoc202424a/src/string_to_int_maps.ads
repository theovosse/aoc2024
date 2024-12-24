with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package String_To_Int_Maps is new Ada.Containers.Ordered_Maps (Unbounded_String, Integer);