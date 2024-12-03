package body Parse_Mul is

   type State_Type is (
      Start, M, MU, MUL, MULp, MULpD, MULpDc, MULpDcD,
             D, xDO, DOp, DON, DONa, DONaT, DONaTp
   );

   Enabled : Boolean := True;
   State : State_Type := Start;
   Factor1, Factor2, Product, Sum : Natural := 0;

   procedure Initialize is
   begin
      State := Start;
      Sum := 0;
      Enabled := True;
   end Initialize;

   procedure Process_Char (Ch : Character) is
   begin
      case State is
         when Start =>
            case Ch is
               when 'd' => State := D;
               when 'm' => State := M;
               when others => State := Start;
            end case;
         when D =>
            case Ch is
               when 'd' => State := D;
               when 'm' => State := M;
               when 'o' => State := xDO;
               when others => State := Start;
            end case;
         when xDO =>
            case Ch is
               when 'd' => State := D;
               when 'm' => State := M;
               when '(' => State := DOp;
               when 'n' => State := DON;
               when others => State := Start;
            end case;
         when DOp =>
            case Ch is
               when 'd' => State := D;
               when 'm' => State := M;
               when ')' =>
                  Enabled := True;
                  State := Start;
               when 'n' => State := DON;
               when others => State := Start;
            end case;
         when DON =>
            case Ch is
               when 'd' => State := D;
               when 'm' => State := M;
               when ''' => State := DONa;
               when others => State := Start;
            end case;
         when DONa =>
            case Ch is
               when 'd' => State := D;
               when 'm' => State := M;
               when 't' => State := DONaT;
               when others => State := Start;
            end case;
         when DONaT =>
            case Ch is
               when 'd' => State := D;
               when 'm' => State := M;
               when '(' => State := DONaTp;
               when others => State := Start;
            end case;
         when DONaTp =>
            case Ch is
               when 'd' => State := D;
               when 'm' => State := M;
               when ')' =>
                  Enabled := False;
                  State := Start;
               when others => State := Start;
            end case;
         when M =>
            case Ch is
               when 'd' => State := D;
               when 'm' => State := M;
               when 'u' => State := MU;
               when others => State := Start;
            end case;
         when MU =>
            case Ch is
               when 'd' => State := D;
               when 'm' => State := M;
               when 'l' => State := MUL;
               when others => State := Start;
            end case;
         when MUL =>
            case Ch is
               when 'd' => State := D;
               when 'm' => State := M;
               when '(' => State := MULp;
               when others => State := Start;
            end case;
         when MULp =>
            case Ch is
               when 'd' => State := D;
               when 'm' => State := M;
               when '0' .. '9' =>
                  Factor1 := Character'Pos (Ch) - 48;
                  State := MULpD;
               when others => State := Start;
            end case;
         when MULpD =>
            case Ch is
               when 'd' => State := D;
               when 'm' => State := M;
               when '0' .. '9' =>
                  Factor1 := 10 * Factor1 + Character'Pos (Ch) - 48;
                  State := MULpD;
               when ',' => State := MULpDc;
               when others => State := Start;
            end case;
         when MULpDc =>
            case Ch is
               when 'd' => State := D;
               when 'm' => State := M;
               when '0' .. '9' =>
                  Factor2 := Character'Pos (Ch) - 48;
                  State := MULpDcD;
               when others => State := Start;
            end case;
         when MULpDcD =>
            case Ch is
               when 'd' => State := D;
               when 'm' => State := M;
               when '0' .. '9' =>
                  Factor2 := 10 * Factor2 + Character'Pos (Ch) - 48;
                  State := MULpDcD;
               when ')' =>
                  if Enabled then
                     Product := Factor1 * Factor2;
                     Sum := Sum + Product;
                  end if;
                  State := Start;
               when others => State := Start;
            end case;
      end case;
   end Process_Char;

   function Get_Result return Integer is (Sum);

end Parse_Mul;