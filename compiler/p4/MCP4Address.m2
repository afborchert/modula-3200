IMPLEMENTATION MODULE MCP4Address;

   FROM Storage IMPORT DEALLOCATE;
   FROM MCP4AttributSys IMPORT Attribut, AtMode;
   FROM MCP4Register IMPORT Reg, FreeReg, FreeFloatReg;
   FROM MCP4Labels IMPORT Label, LabelPtr, GetLabel, stringl;
   FROM MCP4CodeSys IMPORT noOpr;
   FROM MCP4Stack IMPORT FreeStackAt;
   IMPORT MCP4Block;

   CONST
      global = "G_AREA";

   PROCEDURE Address(at: Attribut;
                     VAR FX, SX: Reg;
                     VAR LabelStr: Label;
                     VAR Opr: CHAR;
                     VAR Operand: CARDINAL;
                     VAR done: BOOLEAN);
      VAR
	 slabel: LabelPtr;
   BEGIN
      FX := r0; SX := r0;
      LabelStr[0] := 0C;
      Opr := noOpr;
      Operand := 0;
      WITH at DO
         CASE mode OF
	   globalMod:
	       LabelStr := global;
	       FX := addrReg; SX := addr2Reg;
	       Operand := addr;
	       Opr := "+";
         | localMod:
	       FX := base;
	       SX := addrReg;
               Operand := addr-MCP4Block.offset;
	 | addrLoadedMod:
	       FX := addrReg;
	       Operand := addr;
	 | stringConstMod:
	       WITH strgPtr^ DO
		  IF label = 0 THEN
		     GetLabel(stringl, slabel);
		     label := CARDINAL(slabel);
		  ELSE
		     slabel := LabelPtr(label);
		  END;
		  LabelStr := slabel^;
	       END;
         | setConstMod:
               WITH setPtr^ DO
                  IF LabelPtr(label) = NIL THEN
                     GetLabel(stringl, slabel);
                     label := slabel;
                  ELSE
                     slabel := label;
                  END;
                  LabelStr := slabel^;
               END;
	 | indexMod, byteIndexMod:
	       FX := addrReg;
	       SX := addr2Reg;
	       Operand := addr;
	       Opr := "+";
	 | absolutMod:
	       Operand := addr;
         | stackMod:
               FX := base;
               Operand := offset;
	 ELSE
	    done := FALSE;
	    RETURN;
	 END; (* CASE *)
      END; (* WITH *)
      done := TRUE;
   END Address;

   PROCEDURE Cleanup(VAR at: Attribut);
   BEGIN
      WITH at DO
	 CASE mode OF
	   globalMod:
	       IF addrReg <> r0 THEN
		  FreeReg(addrReg);
		  IF addr2Reg <> r0 THEN
		     FreeReg(addr2Reg);
		  END;
	       END;
	 | localMod:
	       IF addrReg <> r0 THEN
		  FreeReg(addrReg);
	       END;
	 | addrLoadedMod:
	       IF mayRelease THEN
		  FreeReg(addrReg);
	       END;
	 | indexMod, byteIndexMod:
	       FreeReg(addrReg);
	       IF addr2Reg <> r0 THEN
		  FreeReg(addr2Reg);
	       END;
	 | loadedMod:
	       FreeReg(loadReg);
	 | floatLoadedMod:
	       FreeFloatReg(floatLoadReg);
	 | conditionMod:
	       IF tlabel <> NIL THEN DISPOSE(tlabel) END;
	       IF flabel <> NIL THEN DISPOSE(flabel) END;
         | stackMod:
               FreeStackAt(at);
	 ELSE (* stringConstMod, externalMod, constantMod,
		 absolutMod, doubleConstMod, procedureMod,
		 illegalMod *)
	    (* nothing *)
	 END; (* CASE *)
	 mode := illegalMod;
      END; (* WITH *)
   END Cleanup;

END MCP4Address.
