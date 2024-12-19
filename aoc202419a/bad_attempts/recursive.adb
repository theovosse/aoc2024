--  Naive implementation that checks all prefixes, and after a match
--  continues to check the rest of the string. Takes forever on
--  highly ambiguous matches.

   function Check_Possible (Line : Unbounded_String) return Boolean is
      function Check_Combi (Str : Unbounded_String; Pos : Positive) return Boolean is
      begin
         if Pos = Length (Str) then
            return True;
         end if;
         for Element of Elements loop
            declare
               LE : constant Natural := Element'Length;
            begin
               if LE <= Length (Str) - Pos + 1 and then
                  Slice (Str, Pos, Pos + LE - 1) = Element and then
                  Check_Combi (Str, Pos + LE)
               then
                  return True;
               end if;
            end;
         end loop;
         return False;
      end Check_Combi;
   begin
      return Check_Combi (Line, 1);
   end Check_Possible;
