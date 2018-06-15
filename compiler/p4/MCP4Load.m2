IMPLEMENTATION MODULE MCP4Load; (* AFB 8/83 *)

   FROM Storage IMPORT DEALLOCATE;
   FROM MCBase IMPORT charptr, Stptr, realptr, Idptr, boolptr, onebyte, adc,
      procmarkspace, doubleword, Structform, xelos, oneword;
   FROM MCP4AttributSys IMPORT Attribut, AtMode, ModeSet, TestType,
      ArithmeticType;
   FROM MCP4Types IMPORT BaseType, ByteSize;
   FROM MCP4Register IMPORT Reg, FloatReg, GetReg, GetFloatReg, FreeReg,
      FreeFloatReg;
   FROM MCP4CodeSys IMPORT EmitComment, EmitRR, EmitRX3, EmitRX3Label,
      EmitRI, noOpr, EmitSF, EmitFloatRX3, EmitFloatRX3Label, EmitFloatRR,
      EmitLabel, EmitBranch, EmitRX3Int, EmitFloatRX3Int, EmitRXRXLabel,
      EmitVarExtern, EmitProcExtern;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Labels IMPORT LabelPtr, LabelType, GetLabel, Label;
   FROM MCP4Global IMPORT Assert, CompilerError;
   FROM Conversions IMPORT ConvertHex;
   FROM MCP4Block IMPORT offset, EnterReal, level, offsetstack;
   FROM MCP4Stack IMPORT GetStackAt, FreeStackAt;
   FROM MCP4Address IMPORT Address, Cleanup;
   FROM MCP4Test IMPORT Test;
   IMPORT MCP4Block;

   CONST
      global = "G_AREA";

   PROCEDURE Load(VAR fat: Attribut);
      VAR Register: Reg;
          FloatRegister: FloatReg;
   BEGIN
      WITH fat DO
         IF (mode = loadedMod) OR (mode = floatLoadedMod) THEN
            (* do nothing *)
         ELSIF BaseType(typtr) = realptr THEN
            GetFloatReg(FloatRegister);
            LoadFloatReg(fat, FloatRegister);
	 ELSE
	    IF (mode = addrLoadedMod) AND mayRelease THEN
	       Register := addrReg;
            ELSIF (mode IN ModeSet{globalMod, localMod, absolutMod,
               indexMod, byteIndexMod}) AND (addrReg <> r0) THEN
	       Register := addrReg;
	    ELSE
	       GetReg(Register);
	    END;
	    LoadReg(fat, Register);
	 END
      END
   END Load;

   PROCEDURE LoadReg(VAR fat: Attribut; Register: Reg); (* load into Reg. r *)
      TYPE CharPtr = POINTER TO ARRAY[0..0] OF CHAR;
      VAR endLabel: LabelPtr;
          LoadInst: Mnemonic;
          cp: CharPtr;
          slabel: LabelPtr;
   BEGIN
      WITH fat DO
         IF ByteSize(typtr) THEN
            LoadInst := LB;
         ELSE
            LoadInst := L;
         END;
         CASE mode OF
           loadedMod:
	       IF (Register <> loadReg) THEN
		  EmitRR(LR, Register, loadReg);
		  FreeReg(loadReg);
	       END;
         | addrLoadedMod:
	       Assert(mayRelease OR (Register <> addrReg));
               EmitRX3(LoadInst, Register, addrReg, r0, addr);
	       IF (addrReg <> Register) AND mayRelease THEN
		  FreeReg(addrReg);
	       END;
         | constantMod:
               LoadConstant(Register, iValue);
         | globalMod:
               (* BaseType(typtr) <> realptr ! *)
               EmitRX3Label(LoadInst, Register, addrReg, addr2Reg, global, '+', addr);
               IF (addrReg <> r0) AND (addrReg <> Register) THEN
                  FreeReg(addrReg);
               END;
               IF addr2Reg <> r0 THEN
                  FreeReg(addr2Reg);
               END;
         | localMod:
               (* $R- *)
               EmitRX3Int(LoadInst, Register, base, addrReg, addr-MCP4Block.offset);
               (* $R= *)
               IF (addrReg <> r0) AND (addrReg <> Register) THEN
                  FreeReg(addrReg);
               END;
         | conditionMod:
               GetLabel(ifl, endLabel);
	       IF tlabel = NIL THEN
		  GetLabel(ifl, tlabel);
	       END;
	       Test(test, atype, tlabel);
	       IF flabel <> NIL THEN
		  EmitLabel(flabel);
		  DISPOSE(flabel);
	       END;
               EmitSF(LIS, Register, CARDINAL(FALSE));
               EmitBranch(B, r0, r0, endLabel^);
               EmitLabel(tlabel);
	       DISPOSE(tlabel);
               EmitSF(LIS, Register, CARDINAL(TRUE));
               EmitLabel(endLabel);
	       DISPOSE(endLabel);
         | externalMod:
	       IF xelos THEN
		  EmitVarExtern(modPtr^.xidentifier, addr);
		  EmitRX3Label(LoadInst, Register, r0, r0,
		     modPtr^.xidentifier, 'V', addr);
	       ELSE
		  EmitRX3Label(L, r1, r0, r0, modPtr^.identifier, '-', adc);
		  EmitRX3(LoadInst, Register, r1, r0, addr);
	       END;
         | byteIndexMod:
               EmitRX3(LB, Register, addrReg, addr2Reg, addr);
	       IF Register <> addrReg THEN
		  FreeReg(addrReg);
	       END;
               IF addr2Reg <> r0 THEN
                  FreeReg(addr2Reg);
               END;
         | indexMod:
               EmitRX3(L, Register, addrReg, addr2Reg, addr);
	       IF Register <> addrReg THEN
		  FreeReg(addrReg);
	       END;
               IF addr2Reg <> r0 THEN
                  FreeReg(addr2Reg);
               END;
	 | absolutMod:
	       EmitRX3(LoadInst, Register, addrReg, addr2Reg, addr);
               IF (addrReg <> r0) AND (Register <> addrReg) THEN
                  FreeReg(addrReg);
               END;
               IF addr2Reg <> r0 THEN
                  FreeReg(addr2Reg);
               END;
         | procedureMod:
               WITH procPtr^ DO
		  IF xelos THEN
		     EmitProcExtern(globmodp^.xidentifier, procnum);
		     EmitRX3Label(LA, Register, r0, r0, globmodp^.xidentifier,
			'P', procnum);
		  ELSE
		     EmitRX3Label(L, Register, r0, r0, globmodp^.identifier,
			'+', procnum*adc);
		  END;
               END;
         | stringConstMod:
               (* assignment compatible with CHAR *)
               Assert(typtr^.ixp^.max = 0);
               cp := CharPtr(strgPtr^.valentry);
               EmitRI(LHI, Register, r0, ORD(cp^[0]));
               typtr := charptr;
         | stackMod:
               Assert(size = 1);
               EmitRX3(L, Register, base, r0, offset);
               FreeStackAt(fat);
         | setConstMod:
	       Assert(typtr^.size = oneword);
               WITH setPtr^ DO
                  IF LabelPtr(label) = NIL THEN
                     GetLabel(stringl, slabel);
                     label := slabel;
                  ELSE
                     slabel := label;
                  END;
               END;
               EmitRX3Label(L, Register, r0, r0, slabel^, noOpr, 0);
         ELSE
               CompilerError;
         END;
         mode := loadedMod;
         loadReg := Register;
      END
   END LoadReg;

   PROCEDURE LoadCond(VAR fat: Attribut);
   BEGIN
      WITH fat DO
	 IF mode <> conditionMod THEN
	    Load(fat);
	    FreeReg(loadReg);
            mode := conditionMod;
	    test := ne;
	    tlabel := NIL;
	    flabel := NIL;
	    atype := signed;
	 END;
      END;
   END LoadCond;

   PROCEDURE LoadFloatReg(VAR fat: Attribut; fr: FloatReg);
      VAR ConstLabel: LabelPtr;
   BEGIN
      WITH fat DO
         CASE mode OF
           floatLoadedMod:
               EmitFloatRR(LDR, fr, floatLoadReg);
               FreeFloatReg(floatLoadReg);
         | addrLoadedMod:
	       EmitFloatRX3(LD, fr, addrReg, r0, addr);
               IF mayRelease THEN
                  FreeReg(addrReg);
               END;
         | globalMod:
               EmitFloatRX3Label(LD, fr, addrReg, addr2Reg, global, '+', addr);
               IF addrReg <> r0 THEN FreeReg(addrReg) END;
               IF addr2Reg <> r0 THEN FreeReg(addr2Reg) END;
         | localMod:
               (* $R- *)
               EmitFloatRX3Int(LD, fr, base, addrReg, addr-MCP4Block.offset);
               (* $R= *)
               IF addrReg <> r0 THEN FreeReg(addrReg) END;
         | externalMod:
	       IF xelos THEN
		  EmitVarExtern(modPtr^.xidentifier, addr);
		  EmitFloatRX3Label(LD, fr, r0, r0,
		     modPtr^.xidentifier, 'V', addr);
	       ELSE
		  EmitRX3Label(L, r1, r0, r0, modPtr^.identifier, '-', adc);
		  EmitFloatRX3(LD, fr, r1, r0, addr);
	       END;
         | indexMod:
               EmitFloatRX3(LD, fr, addrReg, addr2Reg, addr);
               FreeReg(addrReg);
               IF addr2Reg <> r0 THEN
                  FreeReg(addr2Reg);
               END;
         | doubleConstMod:
               GetLabel(repeatl, ConstLabel);
               EnterReal(ConstLabel, Real);
               EmitFloatRX3Label(LD, fr, r0, r0, ConstLabel^, noOpr, 0);
         | stackMod:
               Assert(size = doubleword DIV adc);
               EmitFloatRX3(LD, fr, r0, r0, offset);
               FreeStackAt(fat);
         ELSE
               CompilerError;
         END;
         mode := floatLoadedMod;
         floatLoadReg := fr;
      END
   END LoadFloatReg;

   PROCEDURE LoadAddr(VAR fat: Attribut);
      VAR Register: Reg;
   BEGIN
      WITH fat DO
         IF (mode = indexMod) OR (mode = byteIndexMod) THEN
            Register := addrReg;
         ELSIF mode = addrLoadedMod THEN
	    IF addr = 0 THEN
	       RETURN
	    ELSIF NOT mayRelease THEN
	       GetReg(Register);
	    ELSE
	       Register := addrReg;
	    END;
         ELSE
            GetReg(Register);
         END;
         LoadAddrReg(fat, Register);
	 mayRelease := TRUE;
      END
   END LoadAddr;

   (* addr always zero after call of LoadAddrReg *)

   PROCEDURE LoadAddrReg(VAR fat: Attribut; Register: Reg);
      VAR rg: Reg;
          slabel: LabelPtr;
	  mayRel: BOOLEAN;
   BEGIN
      mayRel := TRUE;
      WITH fat DO
         CASE mode OF
           globalMod:
               EmitRX3Label(LA, Register, addrReg, addr2Reg, global, '+', addr);
               IF (addrReg <> r0) AND (addrReg <> Register) THEN
                  FreeReg(addrReg);
               END;
               IF addr2Reg <> r0 THEN
                  FreeReg(addr2Reg);
               END;
         | localMod:
               (* $R- *)
               EmitRX3Int(LA, Register, base, addrReg, addr-MCP4Block.offset);
               (* $R= *)
               IF (addrReg <> r0) AND (addrReg <> Register) THEN
                  FreeReg(addrReg);
               END;
	 | stackMod:
	       EmitRX3(LA, Register, base, r0, offset);
	       (* not here: FreeStackAt(fat); *)
         | externalMod:
	       IF xelos THEN
		  EmitVarExtern(modPtr^.xidentifier, addr);
		  EmitRX3Label(LA, Register, r0, r0,
		     modPtr^.xidentifier, 'V', addr);
	       ELSE
		  EmitRX3Label(L, r1, r0, r0, modPtr^.identifier, '-', adc);
		  EmitRX3(LA, Register, r1, r0, addr);
	       END;
         | indexMod, byteIndexMod:
               EmitRX3(LA, Register, addrReg, addr2Reg, addr);
               IF addrReg <> Register THEN
                  FreeReg(addrReg);
               END;
               IF addr2Reg <> r0 THEN
                  FreeReg(addr2Reg);
               END;
         | addrLoadedMod:
               IF addrReg <> Register THEN
                  EmitRR(LR, Register, addrReg);
		  IF mayRelease THEN
		     FreeReg(addrReg);
		  END;
                  mayRelease := TRUE;
                  addrReg := Register;
               END;
               IF addr > 0 THEN
                  EmitRI(AI, Register, r0, addr);
               END;
               mayRel := mayRelease;
         | stringConstMod:
               WITH strgPtr^ DO
                  IF label = 0 THEN
                     GetLabel(stringl, slabel);
                     label := CARDINAL(slabel);
                  ELSE
                     slabel := LabelPtr(label);
                  END;
               END;
               EmitRX3Label(LA, Register, r0, r0, slabel^, noOpr, 0);
         | setConstMod:
               WITH setPtr^ DO
                  IF LabelPtr(label) = NIL THEN
                     GetLabel(stringl, slabel);
                     label := slabel;
                  ELSE
                     slabel := label;
                  END;
               END;
               EmitRX3Label(LA, Register, r0, r0, slabel^, noOpr, 0);
	 | absolutMod:
	       LoadConstant(Register, INTEGER(addr));
         ELSE
            CompilerError;
         END;
         mode := addrLoadedMod;
         addrReg := Register;
	 addr := 0;
	 mayRelease := mayRel;
      END
   END LoadAddrReg;

   PROCEDURE LoadDynHigh(VAR fat: Attribut);
      VAR Index: CARDINAL;
          Register: Reg;
   BEGIN
      WITH fat DO
         IF (mode = addrLoadedMod) AND mayRelease THEN
            FreeReg(addrReg);
         END;
         GetReg(Register);
         Assert((mode = localMod) OR (mode = addrLoadedMod));
         IF dynArrLevelDiff > 0 THEN
            EmitRX3Int(L, Register, base, r0, -procmarkspace);
            FOR Index := dynArrLevelDiff TO 2 BY -1 DO
               EmitRX3Int(L, Register, Register, r0, -procmarkspace);
            END;
            EmitRX3Int(L, Register, Register, r0, INTEGER(dynArrOffset)-
               INTEGER(offsetstack[level-dynArrLevelDiff]));
         ELSE
            (* $R- *)
            EmitRX3Int(L, Register, base, r0, dynArrOffset-MCP4Block.offset);
            (* $R= *)
         END;
         mode := loadedMod;
         loadReg := Register;
      END
   END LoadDynHigh;

   PROCEDURE LoadConstant(Register: Reg; value: INTEGER);
      VAR cardValue: CARDINAL;
   BEGIN
      IF value >= 0 THEN
         EmitRI(LI, Register, r0, value);
      ELSIF value > -16 THEN
         EmitSF(LCS, Register, ABS(value));
      ELSE
         cardValue := CARDINAL(value);
         EmitRI(LI, Register, r0, cardValue);
      END;
   END LoadConstant;

   PROCEDURE LoadBigSet(VAR fat: Attribut);
      VAR
	 helpat: Attribut;
	 (* from Address *)
	 FX, SX: Reg;
	 LabelStr: Label;
	 Opr: CHAR;
	 Operand: CARDINAL;
	 done: BOOLEAN;
   BEGIN
      WITH fat DO
	 Assert((typtr <> NIL) AND (typtr^.form = bigsets));
	 IF mode = stackMod THEN RETURN END;
	 Address(fat, FX, SX, LabelStr, Opr, Operand, done);
	 IF NOT done THEN
	    LoadAddr(fat);
	    Address(fat, FX, SX, LabelStr, Opr, Operand, done);
	 END;
	 helpat := fat;
	 GetStackAt(fat);
	 LoadConstant(r1, INTEGER(size*adc));
	 EmitRXRXLabel(MOVE, r1, base, r0, "",       noOpr, fat.offset,
			     r1, FX,   SX, LabelStr, Opr,   Operand);
	 (* contents of r1 destroyed *)
	 Cleanup(helpat);
      END;
   END LoadBigSet;

END MCP4Load.
