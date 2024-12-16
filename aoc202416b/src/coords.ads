with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;
with Int_Sets;

package Coords is
   use Ada.Containers;

   type Coordinate is record
      --  current position
      I, J : Integer;
      --  current direction
      DI, DJ : Integer;
      --  total distance
      Distance : Integer;
      --  visited positions (hashed)
      Visited : Int_Sets.Set;
   end record;

   function Get_Priority (Coord : Coordinate) return Integer is (Coord.Distance);

   function Before (Left, Right : Integer) return Boolean is (Left < Right);

   package Coordinate_Queues_Interfaces is new Synchronized_Queue_Interfaces
     (Element_Type => Coordinate);
   package Coordinate_Queues is new Unbounded_Priority_Queues
     (Queue_Interfaces => Coordinate_Queues_Interfaces,
      Queue_Priority => Integer);

end Coords;