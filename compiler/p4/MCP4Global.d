DEFINITION MODULE MCP4Global; (* AFB 8/83 *)

   (*
   EXPORT QUALIFIED
      Assert, Error, CompilerError;
   *)

   PROCEDURE Assert(expr: BOOLEAN);

   PROCEDURE Error(nr: CARDINAL);

   PROCEDURE CompilerError;

END MCP4Global.
