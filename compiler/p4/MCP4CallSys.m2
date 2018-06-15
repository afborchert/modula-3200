IMPLEMENTATION MODULE MCP4CallSys; (* AFB 9/83 *)

   FROM Storage IMPORT DEALLOCATE;
   FROM MCBase IMPORT Idptr, Stptr, noprio, Idclass, Structform, Varkind,
      Stpures, Stfuncs, boolptr, intptr, cardptr, wordptr, byteptr,
      realptr, addrptr, charptr, mainmodp, Symbol, adc, oneword,
      doubleword, procmarkspace, BitsPerWord, longintptr, longcardptr,
      longrealptr, xelos;
   FROM MCTypes IMPORT IsReal;
   FROM MCP4Block IMPORT level, blockNptr, EnterSVC, ParmOffset;
   FROM MCP4Stack IMPORT IncTop, DecTop, GetStackAt;
   FROM MCP4ConstArithmetic IMPORT ConstDiv, ConstMul;
   FROM MCP4Scanner IMPORT nptr, GetSymbol, sy, arithmeticRangeCheck;
   FROM MCP4Global IMPORT Error, CompilerError, Assert;
   FROM MCP4Types IMPORT IsArrayType, ByteSize, IsSetType, SizeType,
      SimpleType, Arithmetic, BaseType, intcarptr;
   FROM MCP4AttributSys IMPORT ArithmeticType, AtMode, Attribut, ModeSet;
   FROM MCP4Load IMPORT Load, LoadAddr, LoadDynHigh, LoadConstant, LoadReg,
      LoadAddrReg;
   FROM MCP4ExpressionSys IMPORT Expression;
   FROM MCP4Designator IMPORT Designator;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4RangeChecks IMPORT RangeCheck;
   FROM MCP4Register IMPORT Reg, FloatReg, GetReg, GetFloatReg, FreeReg,
      FreeFloatReg, SaveRegs, RestoreRegs, RegRequest, top;
   FROM MCP4Labels IMPORT Label, GetLabel, LabelPtr, LabelType, LabelLength;
   FROM MCP4CodeSys IMPORT EmitComment, AppendComment, EmitRR, EmitFloatRR,
      EmitRX3, EmitRX3Label, EmitFloatRX3, EmitRI, EmitBranch, EmitLabel,
      EmitSF, noOpr, EmitRX3Int, EmitExtern, EmitDC, EmitFloatRX3Int,
      EmitDCFValue, EmitRXRXLabel, EmitRILabel, EmitEQUInt, EmitProcExtern;
   FROM Conversions IMPORT ConvertInteger;
   FROM Sys IMPORT fork;
   FROM MCP4Address IMPORT Address, Cleanup;
   IMPORT Storage, MCP4Stack;
   
   CONST
      argcLabel = "_ARGC";
      argvLabel = "_ARGV";
      CHKZ      = ".chkz";
      RX3Size = oneword; (* size of RX3-format in bytes *)

   PROCEDURE ProcFuncCall(VAR fat: Attribut);
      VAR Index: CARDINAL;
	  addtoTop: CARDINAL; (* in bytes *)
          done: BOOLEAN;

      MODULE StackReservations;

	 FROM Storage IMPORT ALLOCATE, DEALLOCATE;
	 FROM MCP4Stack IMPORT FreeStack;

	 EXPORT EnterRes, ReleaseAll;

	 TYPE
	    ResList = POINTER TO Res;
	    Res =
	       RECORD
		  offset, size: CARDINAL;
		  link: ResList;
	       END;
	 VAR
	    reslist: ResList;

	 PROCEDURE EnterRes(offset, size: CARDINAL);
	    VAR new: ResList;
	 BEGIN
	    NEW(new);
	    new^.offset := offset; new^.size := size;
	    new^.link := reslist;
	    reslist := new;
	 END EnterRes;

	 PROCEDURE ReleaseAll;
	    VAR old: ResList;
	 BEGIN
	    WHILE reslist <> NIL DO
	       WITH reslist^ DO
		  FreeStack(size, offset);
	       END;
	       old := reslist;
	       reslist := reslist^.link;
	       DISPOSE(old);
	    END;
	 END ReleaseAll;

      BEGIN
	 reslist := NIL;
      END StackReservations;

      PROCEDURE StandFunc(VAR fat: Attribut; fCalling: Stfuncs);
         VAR
            lat1, lat2 : Attribut;
            lsize: CARDINAL;
            ltyp: ArithmeticType;
            lTypP: Stptr;
	    lReg: Reg;
	    elseLabel, endifLabel: LabelPtr;
	    fReg: FloatReg;
            SVCBlock: LabelPtr;
            SVCBlockSize: CARDINAL; (* in bytes *)
	    SVCParLenLabel: LabelPtr;
            SVCCall: CARDINAL; (* SVC number (= second operand of SVC) *)
            offs: CARDINAL;
            addtoTop: CARDINAL;
            i: CARDINAL;
            child, father: LabelPtr;
	    saveBase: Reg;
      BEGIN
         CASE fCalling OF
               argcf: (* ARGC *)
                  WITH fat DO
                     typtr := intcarptr;
                     mode := loadedMod;
                     GetReg(loadReg);
                     EmitExtern(argcLabel);
                     EmitRX3Label(L, loadReg, r0, r0, argcLabel, noOpr, 0);
                  END;
             | uxf: (* UNIXCALL *)
                  AppendComment("UNIXCALL");
		  Expression(lat1);
		  Assert(lat1.mode = constantMod);
		  SVCCall := lat1.value;
                  IF xelos THEN
                     GetLabel(stringl, SVCParLenLabel);
                  ELSE
		     GetLabel(stringl, SVCBlock);
		  END;
		  GetSymbol;
		  Designator(lat1); (* register 0 *)
		  GetSymbol;
		  Designator(lat2); (* register 1 *)
		  SVCBlockSize := 0;
                  WHILE sy = comma DO
                     GetSymbol;
                     Expression(fat);
                     Load(fat);
                     Assert(fat.mode = loadedMod);
		     IF xelos THEN
			EmitRX3Label(ST, fat.loadReg, top, r0,
			   SVCParLenLabel^, '+', SVCBlockSize);
		     ELSE
			EmitRX3Label(ST, fat.loadReg, r0, r0, SVCBlock^, '+',
			   SVCBlockSize+RX3Size);
		     END;
                     FreeReg(fat.loadReg);
                     INC(SVCBlockSize, oneword);
                  END;
		  LoadAddr(lat1); LoadAddr(lat2);
		  IF xelos THEN
		     WITH fat DO
			typtr := boolptr;
			mode := loadedMod;
                        GetReg(loadReg);
			GetReg(saveBase);
			EmitRR(LR, saveBase, base);
			EmitRX3Label(LA, base, top, r0, SVCParLenLabel^,
			   noOpr, 0);
			EmitRX3(SVC, r0, r0, r0, SVCCall);
			GetLabel(ifl, elseLabel);
			GetLabel(ifl, endifLabel);
			EmitBranch(BNC, r0, r0, elseLabel^);
			EmitSF(LIS, loadReg, ORD(FALSE));
			EmitBranch(B, r0, r0, endifLabel^);
			EmitLabel(elseLabel);
			EmitSF(LIS, loadReg, ORD(TRUE));
			EmitLabel(endifLabel);
			EmitRR(LR, base, saveBase); FreeReg(saveBase);
			EmitRX3(ST, r0, lat1.addrReg, r0, 0);
			EmitRX3(ST, r1, lat2.addrReg, r0, 0);
			EmitRR(LR, loadReg, loadReg); (* set condition codes *)
		     END;
		     EmitEQUInt(SVCParLenLabel^, -INTEGER(SVCBlockSize));
		     DISPOSE(SVCParLenLabel);
		     DISPOSE(elseLabel); DISPOSE(endifLabel);
		     Cleanup(lat1); Cleanup(lat2);
		  ELSE
		     EmitRX3(L, r0, lat1.addrReg, r0, 0);
		     EmitRX3(L, r1, lat2.addrReg, r0, 0);
		     AppendComment("indir");
		     EmitRX3(SVC, r0, r0, r0, 0); (* indir svc *)
		     EmitDC(SVCBlock^);
		     EnterSVC(SVCBlock, SVCCall, SVCBlockSize);
		     (* store return code of SVC *)
		     EmitRX3(ST, r0, lat1.addrReg, r0, 0);
		     EmitRX3(ST, r1, lat2.addrReg, r0, 0);
		     Cleanup(lat1); Cleanup(lat2);
		     WITH fat DO
			typtr := boolptr;
			mode := loadedMod;
			GetReg(loadReg);
			GetLabel(ifl, elseLabel);
			GetLabel(ifl, endifLabel);
			EmitBranch(BC, r0, r0, elseLabel^);
			EmitSF(LIS, loadReg, CARDINAL(TRUE));
			EmitBranch(B, r0, r0, endifLabel^);
			EmitLabel(elseLabel);
			EmitSF(LIS, loadReg, CARDINAL(FALSE));
			EmitLabel(endifLabel);
			DISPOSE(elseLabel); DISPOSE(endifLabel);
		     END;
		  END;
             | uxff: (* PROCEDURE UNIXFORK(VAR pid: CARDINAL) : BOOLEAN; *)
		  (* XELOS: PROCEDURE UNIXFORK() : INTEGER; *)
                  AppendComment("UNIXFORK");
                  WITH fat DO
		     typtr := boolptr;
                     mode := loadedMod;
                     GetReg(loadReg);
                  END;
		  IF xelos THEN
		     Designator(lat1); LoadAddr(lat1);
		     GetLabel(ifl, elseLabel);
		     GetLabel(ifl, endifLabel);
		     EmitRX3(SVC, r0, r0, r0, fork);
		     EmitBranch(BC, r0, r0, elseLabel^);
		     EmitSF(LIS, fat.loadReg, ORD(TRUE));
		     EmitRR(LR, r1, r1);
		     EmitBranch(BZ, r0, r0, endifLabel^);
		     EmitSF(LIS, r0, 0);
		     EmitBranch(B, r0, r0, endifLabel^);
		     EmitLabel(elseLabel);
		     EmitSF(LIS, fat.loadReg, ORD(FALSE));
		     EmitLabel(endifLabel);
		     EmitRX3(ST, r0, lat1.addrReg, r0, 0);
		     EmitRR(LR, fat.loadReg, fat.loadReg);
		     DISPOSE(elseLabel); DISPOSE(endifLabel);
		     Cleanup(lat1);
		  ELSE
		     Designator(lat1);
		     LoadAddr(lat1);
		     EmitRX3(SVC, r0, r0, r0, fork);
		     GetLabel(ifl, child);
		     GetLabel(ifl, father);
		     (* will be optimized to SF-format (by as) *)
		     EmitBranch(B, r0, r0, child^);
		     (* dummy RR instruction (exact 2 bytes long) *)
		     EmitRR(LR, r0, r0);
		     (* this position must be 4 bytes after the svc *)
		     EmitRX3(ST, r0, lat1.addrReg, r0, 0);
		     GetLabel(ifl, elseLabel); GetLabel(ifl, endifLabel);
		     EmitBranch(BC, r0, r0, elseLabel^);
		     EmitBranch(B, r0, r0, father^);
		     EmitLabel(child);
		     EmitSF(LIS, r0, 0);
		     EmitRX3(ST, r0, lat1.addrReg, r0, 0);
		     EmitLabel(father);
		     EmitSF(LIS, fat.loadReg, CARDINAL(TRUE));
		     EmitBranch(B, r0, r0, endifLabel^);
		     EmitLabel(elseLabel);
		     EmitSF(LIS, fat.loadReg, CARDINAL(FALSE));
		     EmitLabel(endifLabel);
		     DISPOSE(elseLabel); DISPOSE(endifLabel);
		     Cleanup(lat1);
		     DISPOSE(child); DISPOSE(father);
		  END;
             | uxsf: (* PROCEDURE UNIXSIGNAL(sig: CARDINAL; p: PROC;
                           VAR old: PROC; VAR error: CARDINAL) : BOOLEAN *)
                  SaveRegs;
                  offs := 4 * oneword;
                  EmitRI(AHI, top, r0, offs);
		  IncTop(4 * oneword);
                  i := 0;
                  LOOP
                     INC(i);
                     CASE i OF
                       1, 2: (* sig: CARDINAL; p: PROC *)
                           Expression(lat1); Load(lat1);
                           EmitRX3Int(ST, lat1.loadReg, top, r0, -INTEGER(offs));
                           FreeReg(lat1.loadReg);
                     | 3, 4: (* VAR old: PROC; VAR error: CARDINAL *)
                           Designator(lat1); LoadAddr(lat1);
                           EmitRX3Int(ST, lat1.addrReg, top, r0, -INTEGER(offs));
                           IF lat1.mayRelease THEN
                              FreeReg(lat1.addrReg);
                           END;
                           IF i = 4 THEN
                              EXIT;
                           END;
                     END;
                     GetSymbol; (* comma *)
                     DEC(offs, adc);
                  END;
                  EmitExtern(".signal");
                  EmitRX3Label(BAL, rf, r0, r0, ".signal", noOpr, 0);
		  DecTop(4 * oneword);
                  RestoreRegs;

                  WITH fat DO
                     typtr := boolptr;
                     mode := loadedMod;
                     GetReg(loadReg);
                     EmitRR(LR, loadReg, r0);
                  END;
             | higf: (* HIGH *)
                  Designator(fat);
                  WITH fat DO
                     Assert(IsArrayType(typtr));
                     WITH typtr^ DO
                        IF dyn THEN
                           LoadDynHigh(fat);
                           Load(fat);
                           typtr := cardptr
                        ELSE
                           Assert(ixp^.form = subranges);
                           mode := constantMod;
                           value := ixp^.max;
                           typtr := ixp^.scalp
                        END
                     END
                  END

            |  sizf: (* SIZE *)
                  Designator(fat);
                  WITH fat DO
                     IF IsArrayType(typtr) AND typtr^.dyn THEN
                        LoadDynHigh(fat);
                        Load(fat);
			EmitSF(AIS, fat.loadReg, 1);
                        IF ByteSize(typtr^.elp) THEN
                        ELSE
                           ConstMul(unSigned, fat, typtr^.elp^.size)
                        END
                     ELSE
                        mode := constantMod;
                        value := SizeType(fat)
                     END;
                     typtr := intcarptr
                  END

            |  adrf: (* ADR *)
                  Designator(fat);
                  LoadAddr(fat);
                  lReg := fat.addrReg;
                  IF fat.mayRelease THEN
                     fat.mode := loadedMod;
                     fat.loadReg := lReg;
                  ELSE
                     fat.mode := loadedMod;
                     GetReg(fat.loadReg);
                     EmitRR(LR, fat.loadReg, lReg);
                  END;
                  fat.typtr := addrptr

            |  oddf: (* ODD *)
                  Expression(fat);
		  Load(fat);
		  AppendComment("ODD");
		  EmitRI(NI, fat.loadReg, r0, 1);
                  fat.typtr := boolptr;

            |  chrf: (* CHR *)
                  Expression(fat);
                  Load(fat);
		  RangeCheck(charptr, fat, arithmeticRangeCheck);
                  fat.typtr := charptr

            |  ordf: (* ORD *)
                  Expression(fat);
                  Load(fat);
                  RangeCheck(cardptr, fat, arithmeticRangeCheck);
                  fat.typtr := cardptr

            |  valf:
                  Assert((sy = namesy) AND (nptr^.klass = types));
                  lTypP := nptr^.idtyp;
		  GetSymbol;
                  GetSymbol; (*comma*)
                  Expression(fat);
                  Load(fat);
		  RangeCheck(lTypP, fat, arithmeticRangeCheck);
                  WITH fat DO
                     typtr := lTypP;
                  END;

            |  absf:  (* ABS *)
                  Expression(fat);
                  ltyp := Arithmetic(fat, fat);
                  IF ltyp = signed  THEN
		     IF fat.mode = loadedMod THEN (* set condition codes *)
			EmitRR(LR, fat.loadReg, fat.loadReg);
		     ELSE
			Load(fat);
		     END;
		     AppendComment("signed ABS");
		     GetLabel(ifl, endifLabel);
		     EmitBranch(BNM, r0, r0, endifLabel^);
		     EmitSF(LCS, r0, 1);
		     EmitRR(XR, fat.loadReg, r0);
		     EmitSF(AIS, fat.loadReg, 1);
		     EmitLabel(endifLabel);
		     DISPOSE(endifLabel);
                  ELSIF ltyp = floating THEN
		     AppendComment("floating ABS");
                     Load(fat);
		     EmitFloatRR(LPDR, fat.floatLoadReg, fat.floatLoadReg);
                  ELSE
                     Assert(ltyp=unSigned)
                  END

            |  capf:  (* CAP *)
                  Expression(fat);
		  AppendComment("CAP");
		  Load(fat);
		  EmitRI(NI, fat.loadReg, r0, 137B);

            |  fltf:  (* FLOAT *)
                  Expression(fat);
		  Load(fat);
		  GetFloatReg(fReg);
		  AppendComment("FLOAT");
                  (* fldr converts a SIGNED 32-bit value to real *)
                  (* for this reason following check is needed   *)
                  RangeCheck(intptr, fat, arithmeticRangeCheck);
		  EmitRR(FLDR, Reg(2*ORD(fReg)), fat.loadReg);
		  FreeReg(fat.loadReg);
		  fat.mode := floatLoadedMod;
		  fat.floatLoadReg := fReg;
		  fat.typtr := realptr;

            |  trcf:  (* TRUNC *)
                  Expression(fat);
		  Load(fat);
		  GetReg(lReg);
		  AppendComment("TRUNC");
		  EmitRR(FXDR, lReg, Reg(2*ORD(fat.floatLoadReg)));
		  FreeFloatReg(fat.floatLoadReg);
		  fat.mode := loadedMod;
		  fat.loadReg := lReg;
                  (* NOT standard: *)
                  (* fxdr converts floating point numbers to signed *)
                  (* 32-bit values                                  *)
		  fat.typtr := intptr;
         END
      END StandFunc;

   PROCEDURE StandProc(fCalling: Stpures);

      VAR
         lat1, lat2, lat3: Attribut;
         AType: ArithmeticType;
         LoopLabel, ExitLabel: LabelPtr;
         lReg: Reg;
         offs: INTEGER;
         i: CARDINAL;
         inst: Mnemonic;
         (* result of Address *)
         FX, SX: Reg;
         LabelStr: Label;
         Opr: CHAR; Operand: CARDINAL;

      PROCEDURE SetRX3(VAR at: Attribut);
         VAR done: BOOLEAN;
      BEGIN
         Address(at, FX, SX, LabelStr, Opr, Operand, done);
         IF NOT done THEN
            LoadAddr(at);
            Address(at, FX, SX, LabelStr, Opr, Operand, done);
         END;
      END SetRX3;

   BEGIN
      CASE fCalling OF
            argvp: (* ARGV *)
              AppendComment("ARGV");
              Designator(lat1);
              GetSymbol;
              Expression(lat2);
              Load(lat2);
              EmitSF(SLL, lat2.loadReg, 2);
              EmitExtern(argvLabel);
              EmitRX3Label(A, lat2.loadReg, r0, r0, argvLabel, noOpr, 0);
              EmitRX3(L, lat2.loadReg, lat2.loadReg, r0, 0);
              LoadAddr(lat1);
              IF NOT lat1.mayRelease THEN
                 GetReg(lReg);
                 EmitRR(LR, lReg, lat1.addrReg);
                 lat1.addrReg := lReg;
                 lat1.mayRelease := TRUE;
              END;
              WITH lat1.typtr^ DO
                 Assert(form = arrays);
                 IF dyn THEN
                    lat3 := lat1;
                    lat3.mayRelease := FALSE;
                    LoadDynHigh(lat3);
                    EmitSF(AIS, lat3.loadReg, 1);
                 ELSE
                    WITH lat3 DO
                       mode := loadedMod;
                       GetReg(loadReg);
                       LoadConstant(loadReg, ixp^.max-ixp^.min+1);
                    END;
                 END;
              END;
              (* lat1.addrReg -> in which we like to have the argument *)
              (* lat2.loadReg -> requested argument *)
              (* lat3.loadReg :  size of given character array *)
              GetLabel(whilel, LoopLabel);
              GetLabel(whilel, ExitLabel);
              EmitLabel(LoopLabel);
              (* branch to ExitLabel if (lat3.loadReg = 0)  *)
              (*                     or (lat2.loadReg^ = 0B)  *)
              (* we assume lat3.loadReg > 0 at the beginning of the loop *)
              EmitRX3(LB, r0, lat2.loadReg, r0, 0);
              EmitRX3(STB, r0, lat1.addrReg, r0, lat1.addr);
              EmitRR(LR, r0, r0); (* set condition codes *)
              EmitBranch(BZ, r0, r0, ExitLabel^);
              EmitSF(SIS, lat3.loadReg, 1);
              EmitBranch(BZ, r0, r0, ExitLabel^);
              EmitSF(AIS, lat1.addrReg, 1);
              EmitSF(AIS, lat2.loadReg, 1);
              EmitBranch(B, r0, r0, LoopLabel^);
              EmitLabel(ExitLabel);
              DISPOSE(ExitLabel);
              DISPOSE(LoopLabel);
              FreeReg(lat1.addrReg); FreeReg(lat2.loadReg); FreeReg(lat3.loadReg);
          | incp, decp: (* INC, DEC(lat1, lat2) *)
               Designator(lat1);
	       IF fCalling = incp THEN
		  AppendComment("INC");
	       ELSE
		  AppendComment("DEC");
	       END;
               IF sy = comma THEN
		  GetSymbol;
                  Expression(lat2);
		  IF fCalling = decp THEN
		     IF lat2.mode = constantMod THEN
			lat2.iValue := -lat2.iValue;
			Load(lat2);
		     ELSE
			LoadReg(lat2, r0);
			GetReg(lat2.loadReg);
			EmitSF(LIS, lat2.loadReg, 0);
			EmitRR(SR, lat2.loadReg, r0);
		     END
		  ELSE
		     Load(lat2);
		  END
               ELSE
		  lat2.mode := loadedMod;
		  lat2.typtr := lat1.typtr;
                  GetReg(lat2.loadReg);
		  IF fCalling = decp THEN
		     LoadConstant(lat2.loadReg, -1);
		  ELSE
		     LoadConstant(lat2.loadReg, 1);
		  END
               END;
               IF ByteSize(lat1.typtr) THEN (* no optimisation possible *)
                  LoadAddr(lat1);
                  lat3 := lat1;
                  lat3.mayRelease := FALSE;
                  Load(lat3);
                  EmitRR(AR, lat3.loadReg, lat2.loadReg);
                  EmitRX3(STB, lat3.loadReg, lat1.addrReg, r0, lat1.addr);
                  Cleanup(lat3);
               ELSE
                  SetRX3(lat1);
                  EmitRX3Label(AM, lat2.loadReg, FX, SX, LabelStr,
                               Opr, Operand);
               END;
               Cleanup(lat1); Cleanup(lat2);

         |  halp: (* HALT *)
               EmitExtern(".halt");
               EmitRX3Label(BAL, r1, r0, r0, ".halt", noOpr, 0);

         |  inlp, exlp: (* INCL, EXCL(lat1, lat2) *)
               Designator(lat1);
               SetRX3(lat1);
               GetSymbol;
               Expression(lat2);
	       Load(lat2);
               RangeCheck(lat1.typtr^.basep, lat2, arithmeticRangeCheck);
               WITH lat1.typtr^ DO
                  IF (form = bigsets) AND (offset > 0) THEN
                     EmitRI(SI, lat2.loadReg, r0, offset);
                  END;
               END;
               IF fCalling = inlp THEN
		  AppendComment("INCL");
                  inst := SBT;
               ELSE (*exclp*)
		  AppendComment("EXCL");
                  inst := RBT;
               END;
               EmitRX3Label(inst, lat2.loadReg,
                            FX, SX, LabelStr, Opr, Operand);
               Cleanup(lat1); Cleanup(lat2);

         |  nprp: (* NEWPROCESS(P: PROC; A: ADDRESS; n: CARDINAL;
                                VAR new: PROCESS) *)

               SaveRegs;
               offs := 4 * oneword;
               EmitRI(AHI, top, r0, offs);
	       IncTop(4 * oneword);
               i := 0;
               LOOP
                  INC(i);
                  CASE i OF
                    1, 2, 3: (* P: PROC; A: ADDRESS: n: CARDINAL *)
                        Expression(lat1); Load(lat1);
                        EmitRX3Int(ST, lat1.loadReg, top, r0, -offs);
                        FreeReg(lat1.loadReg);
                  | 4: (* VAR new: PROCESS *)
                        Designator(lat1); LoadAddr(lat1);
                        EmitRX3Int(ST, lat1.addrReg, top, r0, -offs);
                        IF lat1.mayRelease THEN
                           FreeReg(lat1.addrReg);
                        END;
                        EXIT;
                  END;
                  GetSymbol; (* comma *)
                  DEC(offs, adc);
               END;
               EmitExtern(".newproc");
               EmitRX3Label(BAL, rf, r0, r0, ".newproc", noOpr, 0);
	       DecTop(4 * oneword);
               RestoreRegs;

         |  trsp: (* TRANSFER *)
               SaveRegs;
	       IF xelos THEN
		  IncTop(2 * oneword);
		  EmitRI(AHI, top, r0, 2 * oneword);
		  Designator(lat1); LoadAddr(lat1);
		  EmitRX3Int(ST, lat1.addrReg, top, r0, - 2 * oneword);
		  Cleanup(lat1);
		  GetSymbol; (* comma *)
		  Designator(lat2); LoadAddr(lat2);
		  EmitRX3Int(ST, lat2.addrReg, top, r0, - oneword);
		  Cleanup(lat2);
		  DecTop(2 * oneword);
	       ELSE
		  (* VAR source: PROCESS *)
		  Designator(lat1); LoadAddrReg(lat1, rc);
		  GetSymbol; (* comma *)
		  (* VAR dest: PROCESS *)
		  Designator(lat2); LoadAddrReg(lat2, rd);
	       END;
	       EmitExtern(".transfe");
	       EmitRX3Label(BAL, rf, r0, r0, ".transfe", noOpr, 0);
               RestoreRegs;

         END;
      END StandProc;

      (*
       *	principles of operation for nonstandard procs/funcs :
       *
       *	ParmOffset bytes has been already reserved on stack
       *	for parameter and procmarkspace of procedures
       *	(assuming ParmOffset > procmarkspace); this avoids
       *	a lot of unneccessary increments and decrements of
       *	the stack top.
       *	This cannot be done for functions, even for
       *	not nested functions due to the fact that functions
       *	doesn't know the nesting level of their call.
       *	If there are no parameters at all the called
       *	function reserves its procmarkspace itself.
       *
       *	The parameters are stored into the activation record
       *	of the procedure to be called.
       *	In case of call by value only the address has to be be stored
       *	for arrays and records; the called procedure will
       *	copy these parameters into its activation record.
       *	Big sets must be copied in the calling sequence
       *	due to the fact that they may be temporary on stack.
       *
       *	The stack top needs not to be restored; this
       *	is done by the procedure/function itself
       *	(see MCP4Block; TerminateBlock).
       *
       *	Don't change anything without inspecting MCP4Block !
       *)

      PROCEDURE LoadParam(fsp: Stptr); (* type of procedure/function *)
         VAR
            lat: Attribut; (* attribut of one of the parameters *)
            LNP: Idptr;    (* walks through the list of parameters *)
            (* set by Address *)
            FX, SX: Reg; LabelStr: Label; Opr: CHAR;
            Operand: CARDINAL; done: BOOLEAN;

         PROCEDURE DynParam(VAR fat: Attribut; (* parameter *)
                            IsBlock: BOOLEAN; (* ARRAY OF WORD ? *)
			    IsByteBlock: BOOLEAN; (* ARRAY OF BYTE ? *)
                            vaddr: CARDINAL); (* parameter address *)
	    VAR
	       s: CARDINAL; (* byte or word size of fat *)
	       loadreg: Reg;
	       floatloadreg: FloatReg;
	 BEGIN  
	    WITH fat DO  
	       WITH typtr^ DO
		  Assert((form = arrays) OR IsBlock OR IsByteBlock);  
		  IF (form = arrays) AND dyn THEN 
		     LoadAddr(fat);
		     (* $R- *)
		     EmitRX3Int(ST, fat.addrReg, top, r0, vaddr-addtoTop);
		     (* $R= *)
		     LoadDynHigh(fat);
		     IF IsBlock THEN
			IF ByteSize(elp) THEN
			   (* for fat.loadReg mod 4 = 0 should be checked *)
			   EmitSF(SRL, fat.loadReg, 2);
			ELSE
			   (* high := (high+1)*wordsize - 1 *)
			   (* calculate wordsize *)
			   s := (elp^.size + adc - 1) DIV adc;
			   ConstMul(unSigned, fat, s);
			   IF s > 1 THEN
			      EmitRI(AI, fat.loadReg, r0, s-1);
			   END;
			END;
		     ELSIF IsByteBlock THEN
			IF NOT ByteSize(elp) THEN
			   (* high := (high+1)*elp^.size - 1 *)
			   s := elp^.size;
			   ConstMul(unSigned, fat, s);
			   IF s > 1 THEN
			      EmitRI(AI, fat.loadReg, r0, s-1);
			   END;
			END;
		     END;
		     (* $R- *)
		     EmitRX3Int(ST, fat.loadReg, top, r0, vaddr+oneword-addtoTop);
		     (* $R= *)
		     FreeReg(fat.loadReg);
		  ELSE (* (form <> arrays) OR NOT dyn *)
		     IF IsBlock OR IsByteBlock THEN
			IF (mode = loadedMod) OR (mode = constantMod) OR
			   (mode = conditionMod) THEN
			   Load(fat);
			   loadreg := loadReg;
			   GetStackAt(fat); EnterRes(fat.offset, fat.size);
			   IF fat.typtr^.size < oneword THEN
			      EmitRX3(STB, loadreg, base, r0, fat.offset);
			   ELSE
			      EmitRX3(ST, loadreg, base, r0, fat.offset);
			   END;
			   FreeReg(loadreg);
			ELSIF (mode = floatLoadedMod) OR
			      (mode = doubleConstMod) THEN
			   Load(fat);
			   floatloadreg := floatLoadReg;
			   GetStackAt(fat); EnterRes(fat.offset, fat.size);
			   EmitFloatRX3(STD, floatloadreg, base, r0, fat.offset);
			   FreeFloatReg(floatloadreg);
			END;
			IF mode IN ModeSet{globalMod, localMod, addrLoadedMod,
					   externalMod, indexMod, byteIndexMod,
                                           setConstMod, stackMod,
					   absolutMod, stringConstMod} THEN 
			   IF IsBlock AND (size MOD oneword <> 0) THEN
			      Error(209);
			   END;
			   LoadAddr(fat);
			   (* $R- *)
			   EmitRX3Int(ST, fat.addrReg, top, r0, vaddr-addtoTop);
			   (* $R= *)
			   IF IsBlock THEN
			      EmitRI(LI, r0, r0, (size+adc-1) DIV adc - 1);
			   ELSE
			      EmitRI(LI, r0, r0, size-1);
			   END;
			   (* $R- *)
			   EmitRX3Int(ST, r0, top, r0, vaddr+oneword-addtoTop);
			   (* $R= *)
			   IF fat.mayRelease THEN
			      FreeReg(fat.addrReg);
			   END;
			ELSE
			   IF IsBlock THEN
			      Error(209);
			   ELSE
			      Error(214);
			   END;
			END;
		     ELSE (* NOT IsBlock AND NOT IsByteBlock *)
			LoadAddr(fat);  
			(* $R- *)
			EmitRX3Int(ST, fat.addrReg, top, r0, vaddr-addtoTop);
			(* $R= *)
			WITH ixp^ DO  
			   IF max >= min THEN
			      EmitRI(LI, r0, r0, max-min) 
			   ELSE
			      Error(201);
			   END;  
			   (* $R- *)
			   EmitRX3Int(ST, r0, top, r0, vaddr+oneword-addtoTop);
			   (* $R= *)
			END;
			IF fat.mayRelease THEN
			   FreeReg(fat.addrReg);
			END;
		     END
		  END  
	       END
	    END  
	 END DynParam;  

      BEGIN (* LoadParam *)
         Assert((fsp <> NIL) AND (fsp^.form = proctypes));
         LNP := fsp^.fstparam;
	 IF LNP <> NIL THEN
	    AppendComment("parameters");
	 END;
         (* calculate size of parameter field *)
         addtoTop := fsp^.parlength;
         (* add the size of the parameter field to the top *)
         IF (addtoTop > procmarkspace) THEN
            IF addtoTop MOD adc <> 0 THEN
               INC(addtoTop, adc - addtoTop MOD adc);
            END;
            IF fsp^.rkind = funcs THEN
               EmitRI(AI, top, r0, addtoTop);
            ELSIF addtoTop > ParmOffset THEN
               EmitRI(AI, top, r0, addtoTop-ParmOffset);
            END;
         END;
         IncTop(addtoTop);
         (* loading parameter at local address vaddr : *)
         (* ST reg,-addtoTop+vaddr(top)                *)
         LNP := fsp^.fstparam;
         WHILE LNP <> NIL DO
            WITH LNP^ DO
               Expression(lat); (* parameter *)
               IF (vkind = copyparam) OR (vkind = varparam) THEN
                  IF (idtyp^.form = arrays) AND idtyp^.dyn THEN
		     DynParam(lat, idtyp^.elp = wordptr,
		                   idtyp^.elp = byteptr, vaddr);
                  ELSIF (lat.mode = loadedMod) OR (lat.mode = floatLoadedMod) THEN
                     Error(210); (* occurs through type converters *)
                  ELSE
                     LoadAddr(lat);
                     (* $R- *)
                     EmitRX3Int(ST, lat.addrReg, top, r0, vaddr-addtoTop);
                     (* $R= *)
		     IF lat.mayRelease THEN
			FreeReg(lat.addrReg);
		     END;
                  END
               ELSIF lat.typtr^.form = bigsets THEN
                  (* copy bigset into activation record *)
                  WITH lat DO
                     Address(lat, FX, SX, LabelStr, Opr, Operand, done);
                     IF NOT done THEN
                        LoadAddr(lat);
                        Address(lat, FX, SX, LabelStr, Opr, Operand, done);
                     END;
                     EmitRI(LI, r1, r0, typtr^.size);
                     EmitRXRXLabel(MOVE,
                        r1, top, r0, "",       noOpr, CARDINAL(vaddr-addtoTop),
                        r1, FX,  SX, LabelStr, Opr, Operand);
                     (* contents of r1 destroyed *)
                     Cleanup(lat);
                  END;
               ELSE
		  RangeCheck(idtyp, lat, arithmeticRangeCheck);
                  Load(lat);
		  IF lat.mode = loadedMod THEN
                     (* $R- *)
                     IF ByteSize(lat.typtr) THEN
                        EmitRX3Int(STB, lat.loadReg, top, r0, vaddr-addtoTop);
                     ELSE
                        EmitRX3Int(ST, lat.loadReg, top, r0, vaddr-addtoTop);
                     END;
                     (* $R= *)
		     FreeReg(lat.loadReg);
		  ELSE
                     (* $R- *)
                     EmitFloatRX3Int(STD, lat.floatLoadReg, top, r0,
                                  vaddr-addtoTop);
                     (* $R= *)
		     FreeFloatReg(lat.floatLoadReg);
		  END;
               END;
               LNP := vlink;
               IF LNP <> NIL THEN
                  GetSymbol (*comma*)
               END
            END
         END;
         GetSymbol; (*rparent*)
         DecTop(addtoTop);
      END LoadParam;

      PROCEDURE LocalProcLabel(VAR procLabel: Label; procnum: CARDINAL);
	 VAR i: CARDINAL;
             field: ARRAY[0..LabelLength-3] OF CHAR;
      BEGIN
	 ConvertInteger(procnum, 1, field);
	 procLabel := "LP.";
         FOR i := 3 TO HIGH(procLabel) DO
            procLabel[i] := field[i-3];
         END;
      END LocalProcLabel;

      VAR
         lpsptr: Stptr;
	 procLabel: Label;

   BEGIN (* ProcFuncCall *)
      WITH fat DO
         IF mode = procedureMod THEN
            IF procPtr^.isstandard THEN
               IF procPtr^.klass = funcs THEN
                  StandFunc(fat, procPtr^.fname)
               ELSE (* klass=pures*)
                  StandProc(procPtr^.pname)
               END;
               GetSymbol; (*rparent*)
            ELSIF procPtr^.klass = mods THEN
	       SaveRegs;
               (* assume ParmOffset > procmarkspace *)
	       (* static link *)
	       EmitRX3Int(ST, base, top, r0, -procmarkspace);
	       LocalProcLabel(procLabel, procPtr^.procnum);
	       EmitRX3Label(BAL, rf, r0, r0, procLabel, noOpr, 0);
	       RestoreRegs;
               GetSymbol; (*rparent*)
            ELSE
               lpsptr := procPtr^.idtyp;
               WITH lpsptr^ DO
		  SaveRegs;
                  LoadParam(lpsptr);
                  (* Call *)
                  WITH procPtr^ DO
                     IF (plev = level+1) THEN
                        (* local procedure *)
                        (* assume ParmOffset > procmarkspace *)
                        IF (klass = funcs) AND (addtoTop = procmarkspace) THEN
			   EmitRI(AI, top, r0, procmarkspace);
			END;
			(* static link *)
			EmitRX3Int(ST, base, top, r0, -procmarkspace);
			LocalProcLabel(procLabel, procnum);
			EmitRX3Label(BAL, rf, r0, r0, procLabel, noOpr, 0);
                     ELSIF plev > 1 THEN
                        (* intermediate level procedure *)
                        IF (klass = funcs) AND (addtoTop = procmarkspace) THEN
			   EmitRI(AI, top, r0, procmarkspace);
			END;
			(* static link *)
                        EmitRX3Int(L, r1, base, r0, -procmarkspace);
			FOR Index := level-plev+1 TO 2 BY -1 DO
			   EmitRX3Int(L, r1, r1, r0, -procmarkspace);
			END;
			EmitRX3Int(ST, r1, top, r0, -procmarkspace);
			LocalProcLabel(procLabel, procnum);
			EmitRX3Label(BAL, rf, r0, r0, procLabel, noOpr, 0);
                     ELSIF (plev = 1) AND (globmodp = mainmodp) THEN
                        (* global procedure *)
			(* no static link *)
			LocalProcLabel(procLabel, procnum);
			EmitRX3Label(BAL, rf, r0, r0, procLabel, noOpr, 0);
                     ELSE (* imported procedure *)
			(* external references may be used in ass. expr. ! *)
			IF xelos THEN
			   EmitProcExtern(globmodp^.xidentifier, procnum);
			   EmitRX3Label(BAL, rf, r0, r0, globmodp^.xidentifier,
			      'P', procnum);
                        ELSE
			   EmitRX3Label(L, r1, r0, r0, globmodp^.identifier,
			      '+', procnum*adc);
			   EmitRR(BALR, rf, r1);
			END;
			(* no static link *)
                     END
                  END;
		  (* the top is already correct: see MCP4Block *)
		  RestoreRegs;
                  IF rkind = funcs THEN
                     typtr := funcp;
                     IF typtr^.size <= oneword THEN
			(* result in r0 *)
			mode := loadedMod;
			GetReg(loadReg);
			EmitRR(LR, loadReg, r0);
                     ELSE
                        Assert(IsReal(typtr));
			(* result in fr0 *)
			mode := floatLoadedMod;
			GetFloatReg(floatLoadReg);
			EmitFloatRR(LDR, floatLoadReg, fr0);
                     END
                  END
               END
            END
         ELSE (* procedure variable *)
	    EmitComment("procedure variable call");
            (* must be loaded in the outer level *)
            Load(fat);
	    SaveRegs;
               (* protect register *)
               RegRequest(fat.loadReg, done);
               LoadParam(typtr);
	       (* no static link *)
               EmitRR(BALR, rf, fat.loadReg);
               (* free inner level register *)
               IF done THEN
                  FreeReg(fat.loadReg);
               END;
	    RestoreRegs;
            (* free outer level register *)
            FreeReg(fat.loadReg);
            IF typtr^.rkind = funcs THEN
	       typtr := typtr^.funcp;
	       IF typtr^.size <= oneword THEN
		  (* result in r0 *)
		  mode := loadedMod;
		  GetReg(loadReg);
		  EmitRR(LR, loadReg, r0);
	       ELSE
		  Assert(IsReal(typtr));
		  (* result in fr0 *)
		  mode := floatLoadedMod;
		  GetFloatReg(floatLoadReg);
		  EmitFloatRR(LDR, floatLoadReg, fr0);
	       END
            END
         END
      END;
      ReleaseAll; (* stack reservations *)
   END ProcFuncCall;

END MCP4CallSys.
