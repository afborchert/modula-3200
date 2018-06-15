DEFINITION MODULE MCP4Address;

   FROM MCP4AttributSys IMPORT Attribut;
   FROM MCP4Register IMPORT Reg;
   FROM MCP4Labels IMPORT Label;

   (*
   EXPORT QUALIFIED Address, Cleanup;
   *)

   (* these procedures generate no code *)

   PROCEDURE Address(at: Attribut;
                     VAR FX, SX: Reg;
                     VAR LabelStr: Label;
                     VAR Opr: CHAR;
                     VAR Operand: CARDINAL;
                     VAR done: BOOLEAN);

   PROCEDURE Cleanup(VAR at: Attribut);

END MCP4Address.
