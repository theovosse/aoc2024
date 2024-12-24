with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package String_To_Bool_Maps is new Ada.Containers.Ordered_Maps (Unbounded_String, Boolean);