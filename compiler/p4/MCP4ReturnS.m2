IMPLEMENTATION MODULE MCP4ReturnSys; (* AFB 9/83 *)

   FROM MCBase IMPORT lparent, realptr;
   FROM MCP4CodeSys IMPORT EmitComment, EmitBranch;
   FROM MCP4AttributSys IMPORT Attribut;
   FROM MCP4Register IMPORT Reg, FloatReg;
   FROM MCP4Load IMPORT LoadFloatReg, LoadReg;
   FROM MCP4Labels IMPORT LabelPtr, TopLabel, blockl;
   FROM MCP4Scanner IMPORT sy;
   FROM MCP4ExpressionSys IMPORT Expression;
   FROM MCP4Types IMPORT BaseType;
   FROM MCMnemonics IMPORT Mnemonic;

   PROCEDURE ReturnStatement;
      VAR lat: Attribut;
          returnLabel: LabelPtr;
   BEGIN
      IF sy = lparent THEN
         Expression(lat);
         IF BaseType(lat.typtr) = realptr THEN
            LoadFloatReg(lat, fr0);
         ELSE
            LoadReg(lat, r0);
         END;
      END;
      returnLabel := TopLabel(blockl);
      EmitBranch(B, r0, r0, returnLabel^);
   END ReturnStatement;

END MCP4ReturnSys.
