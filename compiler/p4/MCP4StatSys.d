DEFINITION MODULE MCP4StatSys; (* AFB 9/83 *)

   FROM MCBase IMPORT Symbol;

   (*
   EXPORT QUALIFIED
      Statement, StatSequ1, StatSequ3;
   *)

   PROCEDURE Statement;

   PROCEDURE StatSequ1(s: Symbol);

   PROCEDURE StatSequ3(s1, s2, s3: Symbol);

END MCP4StatSys.
