IMPLEMENTATION MODULE MCP4LoopSys; (* AFB 9/83 *)

   FROM Storage IMPORT DEALLOCATE;
   FROM MCBase IMPORT Symbol;
   FROM MCMnemonics IMPORT B;
   FROM MCP4Scanner IMPORT
      GetSymbol;
   FROM MCP4StatSys IMPORT
      StatSequ1;
   FROM MCP4Global IMPORT
      Error;
   FROM MCP4Labels IMPORT
      LabelType, LabelPtr, GetLabel, PushLabel, PopLabel, TopLabel;
   FROM MCP4CodeSys IMPORT
      EmitBranch, EmitLabel, AppendComment;
   FROM MCP4Register IMPORT
      Reg;
   
   VAR
      exitLabel : LabelPtr;
      level : CARDINAL;

   PROCEDURE LoopStatement;   
      VAR loopLabel: LabelPtr;
   BEGIN
      INC(level);
      AppendComment("loop-statement");
      GetLabel(loopl, loopLabel);
      EmitLabel(loopLabel);
      GetLabel(loopl, exitLabel);
      PushLabel(loopl, exitLabel);
      StatSequ1(endsy);
      EmitBranch(B, r0, r0, loopLabel^);
      GetSymbol; 
      exitLabel := PopLabel(loopl);
      AppendComment("end-loop");
      EmitLabel(exitLabel);
      DISPOSE(exitLabel);
      DISPOSE(loopLabel);
      DEC(level);
   END LoopStatement;  
   
   PROCEDURE ExitStatement;   
   BEGIN  
      IF level = 0 THEN
	 Error(151);
      ELSE
	 exitLabel := TopLabel(loopl);
	 EmitBranch(B, r0, r0, exitLabel^);
      END
   END ExitStatement;  

BEGIN
   level := 0;
END MCP4LoopSys.
