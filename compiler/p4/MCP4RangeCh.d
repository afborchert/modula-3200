DEFINITION MODULE MCP4RangeChecks;

   FROM MCBase IMPORT Stptr;
   FROM MCP4AttributSys IMPORT Attribut;

   (*
   EXPORT QUALIFIED RangeCheckForConstant, RangeCheck, CheckStack;
   *)

   PROCEDURE RangeCheckForConstant(dest: Stptr; VAR fat: Attribut);

   PROCEDURE RangeCheck(dest: Stptr; VAR fat: Attribut; check: BOOLEAN);

   PROCEDURE CheckStack;

END MCP4RangeChecks.
