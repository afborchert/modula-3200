DEFINITION MODULE MCP4Out;

   (*
   EXPORT QUALIFIED
      WriteString, Write, WriteLn, TermOut;
   *)

   PROCEDURE WriteString(str: ARRAY OF CHAR);

   PROCEDURE Write(ch: CHAR);

   PROCEDURE WriteLn;

   PROCEDURE TermOut;

END MCP4Out.
