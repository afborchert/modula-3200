DEFINITION MODULE MCP4In;               (* AFB 8/83 *)

   FROM SYSTEM IMPORT WORD;

   (*
   EXPORT QUALIFIED ReadInputWord, ReadInputHalfword, CloseIO;
   *)

   PROCEDURE ReadInputWord(VAR w: WORD);

   PROCEDURE ReadInputHalfword(VAR w: WORD);

   PROCEDURE CloseIO;

END MCP4In. 
