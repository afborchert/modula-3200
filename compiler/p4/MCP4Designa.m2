IMPLEMENTATION MODULE MCP4Designator; (* AFB 9/83 *)
                                      (* REV AFB 5/84: see MCPass2.m2 *)

   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM MCBase IMPORT Idptr, Stptr, Kindvar, intptr, Symbol, Structform,
      Idclass, addrptr, wordptr, adc, oneword, doubleword, boolptr,
      procmarkspace, realptr;
   FROM MCP4Scanner IMPORT GetSymbol, val, nptr, sy, controlRangeCheck;
   FROM MCP4AttributSys IMPORT AtMode, Attribut, ArithmeticType, ModeSet;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Block IMPORT level, offset, offsetstack;
   FROM MCP4CodeSys IMPORT EmitComment, EmitRR, EmitRX3, EmitRX3Int,
      EmitBranch, EmitLabel, noOpr, EmitExtern, EmitRX3Label, EmitDCFValue;
   FROM MCP4ConstArithmetic IMPORT ConstMulReg;
   FROM MCP4ExpressionSys IMPORT Expression;
   FROM MCP4Global IMPORT Assert, Error, CompilerError;
   FROM MCP4Load IMPORT Load, LoadReg, LoadAddr, LoadConstant, LoadDynHigh;
   FROM MCP4RangeChecks IMPORT RangeCheckForConstant, RangeCheck;
   FROM MCP4Register IMPORT Reg, GetReg, FreeReg;
   FROM MCP4Types IMPORT TestBaseType, IsArrayType, ByteSize;
   FROM MCP4WithSys IMPORT UseWith;
   FROM MCP4Labels IMPORT LabelPtr, GetLabel, LabelType;

   CONST
      CHKERR = ".chkerr";
      CHKZ   = ".chkz";

   PROCEDURE Designator(VAR fat : Attribut);   
      VAR lReg: Reg;

      PROCEDURE TypFunction(ftp: Stptr; VAR fat : Attribut);   
         (* ftp is type of function *)
         VAR oldmode: AtMode;
      BEGIN
	 GetSymbol; 
         Expression(fat);  
         IF fat.mode = stringConstMod THEN
	    LoadAddr(fat)
	 END;
         WITH fat DO
            IF (typtr^.size <> ftp^.size) 
               OR ((typtr^.form = arrays) AND typtr^.dyn) THEN Error(302) 
            ELSIF (ftp^.size > doubleword) OR
	       (ftp^.size = doubleword) AND (typtr <> realptr) THEN
	       LoadAddr(fat) 
	    ELSE
               oldmode := fat.mode;
	       Load(fat);
               IF (oldmode = loadedMod) AND (ftp = boolptr) THEN
                  (* set condition codes *)
                  (* BOOLEAN(ORD(ch)) !!! *)
                  EmitRR(LR, fat.loadReg, fat.loadReg);
               END;
            END;
            typtr := ftp;
         END
      END TypFunction;  

      PROCEDURE PreDesignator(VAR fat: Attribut);
         (* The code for loading a procedure base address will already be generated 
            if necessary, but neither the value nor the address is completely
            loaded *)
         VAR i, j: CARDINAL;
             lType: Stptr;
	     lReg: Reg;
             helpat: Attribut;
      BEGIN
         WITH fat DO
            IF sy = namesy THEN
               WITH nptr^ DO
                  typtr := idtyp;
                  IF klass = vars THEN
                     addr := vaddr;
                     CASE state OF
                        global:
			   mode := globalMod;
                           addrReg := r0;
                           addr2Reg := r0;
                     |  local:
			   i := level-vlevel;
                           IF i = 0 THEN
			      mode := localMod;
                              addrReg := r0;
                           ELSE
			      mode := addrLoadedMod; 
			      mayRelease := TRUE;
                              GetReg(addrReg);
                              EmitRX3Int(L, addrReg, base, r0, -procmarkspace);
                              FOR j := i TO 2 BY -1 DO
                                 EmitRX3Int(L, addrReg, addrReg, r0, -procmarkspace);
                              END;
                              DEC(addr, offsetstack[vlevel]);
                           END;
                           IF (typtr^.form = arrays) AND typtr^.dyn THEN
                              dynArrLevelDiff := i;
                              dynArrOffset := vaddr+oneword;
                           END;
                     |  absolute:
                           mode := absolutMod;
                           addrReg := r0;
                           addr2Reg := r0;
                     |  separate:
			   mode := externalMod; 
			   modPtr := globmodp;
                     END;
                     IF indaccess THEN
			GetReg(lReg);
                        helpat := fat;
                        helpat.typtr := intptr;
			LoadReg(helpat, lReg);
			mode := addrLoadedMod;
			mayRelease := TRUE;
			addrReg := lReg;
			addr := 0;
                     END;
                  ELSIF (klass = pures) OR (klass = funcs) OR
		     (klass = mods) THEN
                     mode := procedureMod;
                     procPtr := nptr
                  ELSE (*klass=types*)
                     Assert(klass=types);
                     GetSymbol;
                     IF sy <> lconbr THEN
                        TypFunction(idtyp, fat)
                     END;
                  END (* IF klass = .. *)  
               END (* WITH nptr^ *)
            ELSE (* sy = field *)
               Assert(sy = field); 
               UseWith(val, fat)
            END;
         END (*WITH fat*);
         IF sy <> lconbr THEN
            GetSymbol
         END;
      END PreDesignator;

      PROCEDURE SwitchAddr(VAR fat: Attribut; x: CARDINAL);
      BEGIN
         INC(fat.addr, x);
      END SwitchAddr;

      PROCEDURE IndexVar(VAR fat: Attribut);
         (* compilation of array indexing *)
         VAR iat: Attribut;
             hat: Attribut;
	     xtyp: Stptr; (* index type *)
	     elsize: CARDINAL;
             isdyn: BOOLEAN;
             newMode: AtMode;
	     label: LabelPtr;
      BEGIN
         WITH fat DO  (* attribut for array *)
            Assert(IsArrayType(typtr));
            IF NOT (mode IN ModeSet{addrLoadedMod, localMod,
                    globalMod, absolutMod, indexMod, byteIndexMod}) THEN
               LoadAddr(fat); (* -> mode := addrLoadedMod, addr := 0 *)
            END;
            WITH typtr^ DO
               xtyp := ixp;
               elsize := elp^.size; isdyn := dyn;
               IF (elsize = 1) AND ByteSize(elp) THEN
		  newMode := byteIndexMod 
               ELSE
		  newMode := indexMod
               END
            END;
            Expression(iat); 
            IF (iat.mode = constantMod) AND NOT isdyn THEN 
	       RangeCheckForConstant(xtyp, iat);
               IF TestBaseType(xtyp) = intptr THEN 
                  SwitchAddr(fat, (CARDINAL(INTEGER(iat.value)-
				  INTEGER(xtyp^.min))*elsize))
               ELSE 
                  SwitchAddr(fat, (iat.value-xtyp^.min)*elsize)
               END;
               WITH fat DO
                  IF mode = indexMod THEN
                     mode := newMode;
                  END;
               END;
            ELSE 
               Load(iat);
	       IF NOT isdyn THEN
		  RangeCheck(xtyp, iat, controlRangeCheck);
               ELSIF controlRangeCheck THEN
                  LoadAddr(fat);
                  hat := fat;
                  hat.mayRelease := FALSE;
		  LoadDynHigh(hat);
		  EmitRR(CLR, hat.loadReg, iat.loadReg);
                  FreeReg(hat.loadReg);
		  GetLabel(ifl, label);
		  EmitBranch(BNL, r0, r0, label^);
		  EmitExtern(CHKERR);
		  EmitRX3Label(BAL, r1, r0, r0, CHKERR, noOpr, 0);
		  EmitLabel(label);
		  DISPOSE(label);
	       END;
               IF newMode = indexMod THEN
                  ConstMulReg(unSigned, iat.loadReg, elsize);
               END;
               IF NOT isdyn THEN
                  IF xtyp^.min <> 0 THEN 
		     IF newMode = indexMod THEN
			LoadConstant(r0, INTEGER(xtyp^.min*elsize));
		     ELSE
			LoadConstant(r0, INTEGER(xtyp^.min));
		     END;
		     EmitRR(SR, iat.loadReg, r0);
                  END;
               END;
               IF addrReg = r0 THEN
                  addrReg := iat.loadReg;
               ELSIF (mode IN ModeSet{globalMod, absolutMod, indexMod,
                  byteIndexMod}) AND (addr2Reg = r0) THEN
                  addr2Reg := iat.loadReg;
               ELSE
                  EmitRR(AR, iat.loadReg, addrReg);
                  IF mayRelease THEN
                     FreeReg(addrReg);
                  END;
                  addrReg := iat.loadReg;
               END;
               IF mode = addrLoadedMod THEN
                  mode := newMode;                     
                  addr2Reg := r0;
               ELSIF mode = indexMod THEN
                  mode := newMode;
               END;
            END; 
            typtr := typtr^.elp;
         END
      END IndexVar;

   BEGIN (* Designator *)  
      PreDesignator(fat);  
      WHILE (sy = lbrack) OR (sy = arrow) OR (sy = period) DO  
         IF sy = lbrack THEN
	    GetSymbol;  
            IndexVar(fat);
            Assert(sy = rbrack);  
            GetSymbol (* rbrack *)  
         ELSIF sy = arrow THEN (* indirection via a pointer *)
            GetSymbol;  
            Load(fat); 
            WITH fat DO  
               IF typtr = addrptr THEN
		  typtr := wordptr
               ELSE
                  Assert((typtr <> NIL) AND (typtr^.form = pointers));  
                  typtr := typtr^.elemp  
               END;
	       lReg := loadReg;
               mode := addrLoadedMod;
	       mayRelease := TRUE;
	       addrReg := lReg;
	       addr := 0
            END  
         ELSE (* record field *)
            GetSymbol; (* period *) 
            Assert((sy = namesy) AND (nptr <> NIL) AND (nptr^.klass = fields));
            SwitchAddr(fat, nptr^.fldaddr); 
	    WITH fat DO
	       typtr := nptr^.idtyp;
	       IF (mode = indexMod) AND ByteSize(typtr) THEN
		  mode := byteIndexMod;
	       END;
	    END;
            GetSymbol;
         END  
      END  
   END Designator;  

END MCP4Designator.
