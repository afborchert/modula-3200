IMPLEMENTATION MODULE MCP4IfSys;        (* AFB 9/83 *)

   FROM Storage IMPORT DEALLOCATE;
   FROM MCBase IMPORT Symbol;
   FROM MCP4AttributSys IMPORT Attribut, AtMode;
   FROM MCP4Scanner IMPORT GetSymbol, Skip, sy;
   FROM MCP4StatSys IMPORT StatSequ1, StatSequ3;
   FROM MCP4Labels IMPORT LabelPtr, LabelType, GetLabel;
   FROM MCP4CodeSys IMPORT AppendComment, EmitRR, EmitBranch, EmitLabel;
   FROM MCMnemonics IMPORT B;
   FROM MCP4Load IMPORT Load, LoadCond;
   FROM MCP4ExpressionSys IMPORT Expression;
   FROM MCP4Register IMPORT Reg, FreeReg;
   FROM MCP4Test IMPORT Test, Invert;

   PROCEDURE IfStatement;
      VAR 
         lat: Attribut;
         endifLabel, elsifLabel : LabelPtr;

   BEGIN 
      AppendComment("if-statement");
      GetLabel(ifl,endifLabel);
      LOOP 
         Expression(lat);
         IF lat.mode = constantMod THEN 
            IF BOOLEAN(lat.value) THEN 
               StatSequ3(endsy, elsifsy, elsesy);
               IF sy <> endsy THEN 
                  Skip(endsy, endsy)
               END;
               EXIT 
            ELSE 
               Skip(elsesy, elsifsy);
               IF sy <> elsifsy THEN 
                  EXIT 
               END;
               GetSymbol 
            END 
         ELSE 
	    WITH lat DO
	       IF mode <> conditionMod THEN 
		  LoadCond(lat);
	       END;
	       IF flabel = NIL THEN
		  GetLabel(ifl, elsifLabel);
	       ELSE
		  elsifLabel := flabel;
	       END;
	       test := Invert(test);
	       Test(test, atype, elsifLabel);
	       IF tlabel <> NIL THEN
		  EmitLabel(tlabel);
		  DISPOSE(tlabel);
	       END;
	    END;
            StatSequ3(endsy, elsifsy, elsesy);
            EmitBranch(B, r0, r0, endifLabel^);
            EmitLabel(elsifLabel);
            DISPOSE(elsifLabel);
            IF sy <> elsifsy THEN 
               EXIT 
            END;
            GetSymbol;
         END;
      END;
      IF sy = elsesy THEN 
         GetSymbol;
         StatSequ1(endsy);
      END;
      AppendComment( "endif");
      EmitLabel(endifLabel);
      DISPOSE(endifLabel);
      GetSymbol;                        (* skip endsy *)
   END IfStatement;

END MCP4IfSys. 
