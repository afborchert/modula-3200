DEFINITION MODULE MCP4ExpressionSys; (* AFB 9/83 *)

   FROM MCP4AttributSys IMPORT Attribut, ArithmeticType;

   (*
   EXPORT QUALIFIED Expression, Mult, Div, Mod;
   *)

   PROCEDURE Expression(VAR fat: Attribut);

   PROCEDURE Mult(VAR fat, lat: Attribut; AType: ArithmeticType);

   PROCEDURE Div(VAR fat, lat: Attribut; AType: ArithmeticType);

   PROCEDURE Mod(VAR fat, lat: Attribut; AType: ArithmeticType);

END MCP4ExpressionSys.
