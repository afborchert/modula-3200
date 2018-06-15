IMPLEMENTATION MODULE MCP4Assign;       (* AFB 11/83 *)
                                        (* REV AFB 5/84 *)

   FROM Storage IMPORT DEALLOCATE;
   FROM MCBase IMPORT realptr, adc, onebyte, oneword, doubleword, Structform;
   FROM MCP4AttributSys IMPORT Attribut, AtMode;
   FROM MCP4ExpressionSys IMPORT Expression;
   FROM MCP4Designator IMPORT Designator;
   FROM MCP4Scanner IMPORT GetSymbol, arithmeticRangeCheck;
   FROM MCP4Load IMPORT Load, LoadAddr;
   FROM MCP4Types IMPORT SizeType, BaseType, SimpleType, ByteSize;
   FROM MCP4CodeSys IMPORT EmitComment, EmitRX3, EmitSF, EmitBranch,
      EmitRI, EmitLabel, EmitRR, EmitRXRXLabel;
   FROM MCP4Store IMPORT Store, StoreFloat;
   FROM MCP4RangeChecks IMPORT RangeCheck;
   FROM MCP4Register IMPORT GetReg, FreeReg, FreeFloatReg, Reg;
   FROM MCP4Labels IMPORT LabelPtr, GetLabel, LabelType, Label;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Address IMPORT Address, Cleanup;

   PROCEDURE Assign(VAR desAT, expAT: Attribut);
      VAR size: CARDINAL;
	  lReg: Reg;
          (* following for MOVE instruction: *)
          FX2, SX2: Reg; A2L: Label; Opr2: CHAR; A2C: CARDINAL;
          FX4, SX4: Reg; A4L: Label; Opr4: CHAR; A4C: CARDINAL;
          AddressDone: BOOLEAN;
   BEGIN
      size := SizeType(desAT); (* use desAt in case of variants ! *)
      IF (BaseType(expAT.typtr) = realptr)
          (* in case of type conversions *)
          OR (expAT.mode = floatLoadedMod) THEN
         Load(expAT);
	 (* expAT.mode = floatLoadedMod *)
	 StoreFloat(expAT.floatLoadReg, desAT);
	 FreeFloatReg(expAT.floatLoadReg);
      ELSIF (size > oneword) OR (expAT.mode = stringConstMod)
                             (* use desAt ! (char := "x") *)
                             AND NOT ByteSize(desAT.typtr) OR
                             (expAT.typtr^.form = bigsets) AND
			     (expAT.mode <> loadedMod) THEN
         Address(expAT, FX4, SX4, A4L, Opr4, A4C, AddressDone);
         IF NOT AddressDone THEN
            LoadAddr(expAT);
            Address(expAT, FX4, SX4, A4L, Opr4, A4C, AddressDone);
         END;
         Address(desAT, FX2, SX2, A2L, Opr2, A2C, AddressDone);
         IF NOT AddressDone THEN
            LoadAddr(desAT);
            Address(desAT, FX2, SX2, A2L, Opr2, A2C, AddressDone);
         END;
         EmitRI(LI, r1, r0, size);
         (* r1: # bytes to be moved *)
         EmitRXRXLabel(MOVE, r1, FX2, SX2, A2L, Opr2, A2C,
                             r1, FX4, SX4, A4L, Opr4, A4C);
         (* contents of r1 destroyed *)
         Cleanup(expAT); Cleanup(desAT);
      ELSE
	 Load(expAT);
	 (* expAT.mode = loadedMod *)
	 RangeCheck(desAT.typtr, expAT, arithmeticRangeCheck);
	 Store(expAT.loadReg, desAT);
	 FreeReg(expAT.loadReg);
      END;
      desAT.mode := illegalMod;
      expAT.mode := illegalMod
   END Assign;

   PROCEDURE Assignment;
      VAR 
	 lat1, lat2: Attribut;
   BEGIN 
      Designator(lat1);
      GetSymbol;
      Expression(lat2);
      Assign(lat1, lat2);
   END Assignment;

END MCP4Assign. 
