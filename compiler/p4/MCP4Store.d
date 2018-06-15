DEFINITION MODULE MCP4Store; (* AFB 9/83 *)

   FROM MCP4AttributSys IMPORT Attribut;
   FROM MCP4Register IMPORT Reg, FloatReg;

   (*
   EXPORT QUALIFIED Store, StoreFloat;
   *)

   PROCEDURE Store(r: Reg; VAR fat: Attribut);
   PROCEDURE StoreFloat(fr: FloatReg; VAR fat: Attribut);

END MCP4Store.
