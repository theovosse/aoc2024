package body Parse_Mul is

   type State_Type is (
      Start, M, MU, MUL, MULp, MULpD, MULpDc, MULpDcD
   );

   State : State_Type := Start;
   Factor1, Factor2, Product, Sum : Natural := 0;

   procedure Initialize is
   begin
      State := Start;
      Sum := 0;
   end Initialize;

   procedure Process_Char (Ch : Character) is
   begin
      case State is
         when Start =>
            case Ch is
               when 'm' =>
                  State := M;
               when others =>
                  State := Start;
            end case;
         when M =>
            case Ch is
               when 'u' =>
                  State := MU;
               when others =>
                  State := Start;
            end case;
         when MU =>
            case Ch is
               when 'l' =>
                  State := MUL;
               when others =>
                  State := Start;
            end case;
         when MUL =>
            case Ch is
               when '(' =>
                  State := MULp;
               when others =>
                  State := Start;
            end case;
         when MULp =>
            case Ch is
               when '0' .. '9' =>
                  Factor1 := Character'Pos (Ch) - 48;
                  State := MULpD;
               when others =>
                  State := Start;
            end case;
         when MULpD =>
            case Ch is
               when '0' .. '9' =>
                  Factor1 := 10 * Factor1 + Character'Pos (Ch) - 48;
                  State := MULpD;
               when ',' =>
                  State := MULpDc;
               when others =>
                  State := Start;
            end case;
         when MULpDc =>
            case Ch is
               when '0' .. '9' =>
                  Factor2 := Character'Pos (Ch) - 48;
                  State := MULpDcD;
               when others =>
                  State := Start;
            end case;
         when MULpDcD =>
            case Ch is
               when '0' .. '9' =>
                  Factor2 := 10 * Factor2 + Character'Pos (Ch) - 48;
                  State := MULpDcD;
               when ')' =>
                  Product := Factor1 * Factor2;
                  Sum := Sum + Product;
                  State := Start;
               when others =>
                  State := Start;
            end case;
      end case;
   end Process_Char;

   function Get_Result return Integer is (Sum);

end Parse_Mul;