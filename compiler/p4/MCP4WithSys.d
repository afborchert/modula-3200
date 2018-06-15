DEFINITION MODULE MCP4WithSys; (* AFB 9/83 *)

   FROM MCP4AttributSys IMPORT Attribut;

   (*
   EXPORT QUALIFIED
      WithStatement, UseWith;
   *)
   
   PROCEDURE WithStatement;

   PROCEDURE UseWith(i: INTEGER; VAR fat: Attribut);

END MCP4WithSys.
