IMPLEMENTATION MODULE MCP4StatSys; (* AFB 9/83 *)

    FROM MCBase IMPORT Symbol;

    FROM MCP4Scanner IMPORT
       GetSymbol, sy;

    FROM MCP4AttributSys IMPORT Attribut;

    FROM MCP4IfSys IMPORT IfStatement;
    FROM MCP4CaseSys IMPORT CaseStatement;
    FROM MCP4LoopSys IMPORT LoopStatement, ExitStatement;
    FROM MCP4WhileSys IMPORT WhileStatement;
    FROM MCP4RepeatSys IMPORT RepeatStatement;
    FROM MCP4WithSys IMPORT WithStatement;
    FROM MCP4ReturnSys IMPORT ReturnStatement;
    FROM MCP4ForSys IMPORT ForStatement;
    FROM MCP4Assign IMPORT Assignment;
    FROM MCP4Designator IMPORT Designator;
    FROM MCP4CallSys IMPORT ProcFuncCall;

   PROCEDURE Statement;   
      VAR lat: Attribut;
   BEGIN (* Statement *)  
      IF sy = becomes THEN GetSymbol; Assignment  
      ELSIF sy = call THEN GetSymbol;   
         Designator(lat); GetSymbol(*lparent*); ProcFuncCall(lat)  
      ELSIF sy = ifsy THEN GetSymbol; IfStatement  
      ELSIF sy = casesy THEN GetSymbol; CaseStatement  
      ELSIF sy = loopsy THEN GetSymbol; LoopStatement  
      ELSIF sy = whilesy THEN GetSymbol; WhileStatement  
      ELSIF sy = repeatsy THEN GetSymbol; RepeatStatement  
      ELSIF sy = withsy THEN GetSymbol; WithStatement  
      ELSIF sy = exitsy THEN GetSymbol; ExitStatement;  
      ELSIF sy = returnsy THEN GetSymbol; ReturnStatement  
      ELSIF sy = forsy THEN GetSymbol; ForStatement  
      (* ELSE empty statement without GetSymbol*)  
      END;
   END Statement;  

   PROCEDURE StatSequ1(s: Symbol);    
   BEGIN  
      REPEAT 
         Statement   
      UNTIL sy = s  
   END StatSequ1;  

   PROCEDURE StatSequ3(s1, s2, s3: Symbol);   
   BEGIN  
      REPEAT
         Statement 
      UNTIL (sy = s1) OR (sy = s2) OR (sy = s3)   
   END StatSequ3;  

END MCP4StatSys.
