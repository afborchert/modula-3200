IMPLEMENTATION MODULE MCP4ExpressionSys; (* AFB 9/83 *)
                                         (* REV AFB 5/85: conditionMod *)

   FROM SYSTEM IMPORT ADDRESS;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM MCBase IMPORT Idptr, Stptr, realptr, boolptr, charptr, intptr,
      cardptr, wordptr, addrptr, Symbol, Structform, bitsetptr, BitsPerWord,
      maxcard, adc, Constval, SetValuePtr, oneword, Stset;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4CodeSys IMPORT EmitRR, EmitFloatRR, EmitSF, EmitRI, EmitBranch,
      EmitLabel, EmitFloatRX3, EmitFloatRX3Label, EmitRX3, EmitRX3Label,
      EmitFloatRX3Int, EmitRX3Int, EmitDCFValue, EmitExtern, noOpr,
      EmitRXRXLabel;
   FROM MCP4ConstArithmetic IMPORT ConstMul, ConstDiv, ConstMod;
   FROM MCP4Block IMPORT level, EnterEQU, offset;
   FROM MCP4Scanner IMPORT GetSymbol, sy, val, nptr, cstPtr, cString,
      arithmeticRangeCheck;
   FROM MCP4Global IMPORT Error, CompilerError, Assert;
   FROM MCP4AttributSys IMPORT AtMode, Attribut, ArithmeticType, TestType;
   FROM MCP4Types IMPORT IsSetType, ResultType, Arithmetic, SimpleType,
      ByteSize, BaseType;
   FROM MCP4Load IMPORT Load, LoadReg, LoadAddr, LoadDynHigh, LoadBigSet,
      LoadCond;
   FROM MCP4CallSys IMPORT ProcFuncCall;
   FROM MCP4Register IMPORT Reg, FloatReg, GetReg, GetFloatReg, FreeReg,
      FreeFloatReg, RegRequest, GetRegPair, GetRegTriplet, FreeRegTriplet;
   FROM MCP4Labels IMPORT LabelPtr, LabelType, GetLabel, Label;
   FROM MCP4Designator IMPORT Designator;
   FROM MCP4Address IMPORT Cleanup, Address;
   FROM MCBigSet IMPORT InitConstSet, ConstSetElement, TermConstSet;
   FROM MCP4Stack IMPORT GetStackAt, FreeStackAt;
   FROM MCP4RangeChecks IMPORT RangeCheck;
   FROM MCP4Test IMPORT Invert, Test;
   IMPORT MCP4Block;

   CONST
      CHKZ = ".chkz";

   PROCEDURE EmitOpConst(RROp, RX3Op, RIOp: Mnemonic; VAR fat, lat: Attribut);
      TYPE
         CharPtr = POINTER TO ARRAY[0..0] OF CHAR;
      VAR
         cp: CharPtr;
         const: CARDINAL;
   BEGIN
      WITH lat DO
         IF (fat.mode = loadedMod) AND
            ((mode = constantMod) OR
             (mode = stringConstMod) AND (typtr^.ixp^.max = 0)) THEN
            IF mode = stringConstMod THEN
               cp := CharPtr(strgPtr^.valentry);
               const := ORD(cp^[0]);
            ELSE
               const := value;
            END;
            EmitRI(RIOp, fat.loadReg, r0, const);
         ELSE
            EmitOp(RROp, RX3Op, fat, lat);
         END;
      END; (* WITH lat DO *)
   END EmitOpConst;

   PROCEDURE EmitOp(RROp, RX3Op: Mnemonic; VAR fat, lat: Attribut);
      CONST global = "G_AREA";
   BEGIN
      IF fat.mode = loadedMod THEN
         CASE lat.mode OF
            globalMod:
               WITH lat DO
                  EmitRX3Label(RX3Op, fat.loadReg, addrReg, addr2Reg, global, '+', addr);
                  IF addrReg <> r0 THEN
                     FreeReg(addrReg);
                  END;
                  IF addr2Reg <> r0 THEN
                     FreeReg(addr2Reg);
                  END;
               END;
         |  localMod:
               WITH lat DO
                  (* $R- *)
                  EmitRX3Int(RX3Op, fat.loadReg, base, addrReg, addr-MCP4Block.offset);
                  (* $R= *)
                  IF addrReg <> r0 THEN
                     FreeReg(addrReg);
                  END;
               END;
	 |  stackMod:
	       WITH lat DO
		  (* $R- *)
		  EmitRX3Int(RX3Op, fat.loadReg, base, r0, offset);
	       END;
	       Cleanup(lat);
         |  addrLoadedMod:
               EmitRX3(RX3Op, fat.loadReg, lat.addrReg, r0, lat.addr);
	       IF lat.mayRelease THEN
		  FreeReg(lat.addrReg);
	       END;
         |  indexMod:
               WITH lat DO
                  EmitRX3(RX3Op, fat.loadReg, addrReg, addr2Reg, addr);
                  FreeReg(addrReg);
                  IF addr2Reg <> r0 THEN
                     FreeReg(addr2Reg);
                  END;
               END;
         |  absolutMod:
               WITH lat DO
                  EmitRX3(RX3Op, fat.loadReg, addrReg, addr2Reg, addr);
                  IF addrReg <> r0 THEN
                     FreeReg(addrReg);
                  END;
                  IF addr2Reg <> r0 THEN
                     FreeReg(addr2Reg);
                  END;
               END;
         ELSE
            Load(lat);
            EmitRR(RROp, fat.loadReg, lat.loadReg);
            FreeReg(lat.loadReg);
         END;
      ELSE
         Assert(fat.mode = floatLoadedMod);
         CASE lat.mode OF
            globalMod:
               WITH lat DO
                  EmitFloatRX3Label(RX3Op, fat.floatLoadReg, addrReg, addr2Reg,
                     global, '+', addr);
                  IF addrReg <> r0 THEN FreeReg(addrReg) END;
                  IF addr2Reg <> r0 THEN FreeReg(addr2Reg) END;
               END;
         |  localMod:
               WITH lat DO
                  (* $R- *)
                  EmitFloatRX3Int(RX3Op, fat.floatLoadReg, base, addrReg,
                     addr-MCP4Block.offset);
                  (* $R= *)
                  IF addrReg <> r0 THEN
                     FreeReg(addrReg);
                  END;
               END;
	 |  stackMod:
	       WITH lat DO
		  EmitFloatRX3(RX3Op, fat.floatLoadReg, base, r0, offset);
	       END;
	       Cleanup(lat);
         |  addrLoadedMod:
               EmitFloatRX3(RX3Op, fat.floatLoadReg, lat.addrReg, r0, lat.addr);
	       IF lat.mayRelease THEN
		  FreeReg(lat.addrReg);
	       END;
         |  indexMod:
               WITH lat DO
                  EmitFloatRX3(RX3Op, fat.floatLoadReg, addrReg, addr2Reg, addr);
                  FreeReg(addrReg);
                  IF addr2Reg <> r0 THEN
                     FreeReg(addr2Reg);
                  END;
               END;
         |  absolutMod:
               WITH lat DO
                  EmitFloatRX3(RX3Op, fat.floatLoadReg, addrReg, addr2Reg, addr);
                  IF addrReg <> r0 THEN
                     FreeReg(addrReg);
                  END;
                  IF addr2Reg <> r0 THEN
                     FreeReg(addr2Reg);
                  END;
               END;
         ELSE
            Load(lat);
            EmitFloatRR(RROp, fat.floatLoadReg, lat.floatLoadReg);
            FreeFloatReg(lat.floatLoadReg);
         END;
      END;
   END EmitOp;

   PROCEDURE BigSetOp(op: Symbol; VAR fat, lat: Attribut);
      (* generate code for fat op lat and clean up lat *)
      (* precondition: lat.typtr^.size > oneword (if op <> insy) *)
      VAR
	 helpat: Attribut;
	 indexReg: Reg;
	 lataddrReg: Reg;
	 latoffset: CARDINAL;
	 label: LabelPtr; (* only if op = leq or geq *)
	 withbxle: BOOLEAN;
	 incrReg: Reg; (* only if withbxle *)
	 limitReg: Reg; (* only if withbxle *)
	 size: CARDINAL;
	 loop: LabelPtr;

      PROCEDURE InOp(VAR fat, lat: Attribut);
	 VAR
	    offset: CARDINAL;
	    (* from Address *)
	    FX, SX: Reg;
	    LabelStr: Label;
	    Opr: CHAR;
	    Operand: CARDINAL;
	    done: BOOLEAN;
      BEGIN
	 offset := lat.typtr^.offset;
	 Load(fat);
	 Address(lat, FX, SX, LabelStr, Opr, Operand, done);
	 IF NOT done THEN
	    LoadAddr(lat);
	    Address(lat, FX, SX, LabelStr, Opr, Operand, done);
	 END;
	 Assert(done);
	 IF offset > 0 THEN
	    EmitRI(SI, fat.loadReg, r0, offset);
	 END;
	 EmitRX3Label(TBT, fat.loadReg, FX, SX, LabelStr, Opr, Operand);
         FreeReg(fat.loadReg);
	 WITH fat DO
	    typtr := boolptr;
	    mode := conditionMod;
	    atype := signed;
	    test := ne;  (* BNZ *)
	    tlabel := NIL;
	    flabel := NIL;
	 END;
	 Cleanup(lat);
      END InOp;

   BEGIN
      IF op <> insy THEN
	 Assert(lat.typtr^.size > oneword); (* don't check fat (opsy = insy) *)
      END;
      CASE op OF
      | plus, times, slash, eql, neq: (* commutative operations on sets *)
	    IF (lat.mode = stackMod) AND (fat.mode <> stackMod) THEN
	       (* exchange both attributes *)
	       helpat := fat; fat := lat; lat := helpat;
	    ELSE
	       LoadBigSet(fat);
	    END;
      | minus:
	    helpat := fat; fat := lat; lat := helpat;
	    LoadBigSet(fat);
      | insy:
	    InOp(fat, lat);
	    RETURN
      | leq:
	    helpat := fat; fat := lat; lat := helpat;
	    op := geq;
	    LoadBigSet(fat);
      | geq:
	    LoadBigSet(fat);
      END;
      IF (op = geq) OR (op = neq) OR (op = eql) THEN
	 GetLabel(ifl, label);
      END;
      (* fat.mode = stackMod *)
      IF lat.mode <> stackMod THEN
	 LoadAddr(lat);
	 lataddrReg := lat.addrReg;
	 latoffset := lat.addr;
      ELSE
	 lataddrReg := base;
	 latoffset := lat.offset;
      END;
      size := fat.typtr^.size;

      (* init of loop *)
      (* for op = geq, eql, neq; the loop is #words-1 times executed *)
      (* optimisation possible for #words = 2 (no loop-construction) needed *)
      GetRegTriplet(indexReg, withbxle);
      IF NOT withbxle THEN
	 GetReg(indexReg);
      ELSE
	 incrReg := indexReg; INC(incrReg);
	 limitReg := incrReg; INC(limitReg);
	 EmitSF(LIS, incrReg, adc);
	 IF (op = geq) OR (op = eql) OR (op = neq) THEN
	    EmitRI(LI, limitReg, r0, size-2*adc);
	 ELSE
	    EmitRI(LI, limitReg, r0, size-adc);
	 END;
      END;
      EmitSF(LIS, indexReg, 0);
      IF op = minus THEN
	 EmitSF(LCS, r0, 1);
      END;

      GetLabel(forl, loop);
      EmitLabel(loop);

      (* inner loop *)
      EmitRX3(L, r1, base, indexReg, fat.offset);
      (* (op <> insy) AND (op <> leq) *)
      CASE op OF
      | plus:
	    EmitRX3(O, r1, lataddrReg, indexReg, latoffset);
      | minus:
	    EmitRR(XR, r1, r0);
	    EmitRX3(N, r1, lataddrReg, indexReg, latoffset);
      | times:
	    EmitRX3(N, r1, lataddrReg, indexReg, latoffset);
      | slash:
	    EmitRX3(X, r1, lataddrReg, indexReg, latoffset);
      | geq:
	    EmitRI(XI, r1, r0, maxcard); (* complement *)
	    EmitRX3(N, r1, lataddrReg, indexReg, latoffset);
	    EmitBranch(BNZ, r0, r0, label^);
      | eql, neq:
	    EmitRX3(C, r1, lataddrReg, indexReg, latoffset);
	    EmitBranch(BNE, r0, r0, label^);
      END;
      CASE op OF
      | plus, minus, times, slash:
            EmitRX3(ST, r1, base, indexReg, fat.offset);
      ELSE
      END;

      IF withbxle THEN
	 EmitRX3Label(BXLE, indexReg, r0, r0, loop^, noOpr, 0);
      ELSE
	 EmitSF(AIS, indexReg, adc);
	 IF (op = geq) OR (op = eql) OR (op = neq) THEN
	    EmitRI(CI, indexReg, r0, size-2*adc);
	 ELSE
	    EmitRI(CI, indexReg, r0, size-adc);
	 END;
	 EmitBranch(BNP, r0, r0, loop^);
      END;

      IF (op = geq) OR (op = eql) OR (op = neq) THEN
         FreeStackAt(fat);
	 WITH fat DO
	    EmitRX3(L, r1, base, indexReg, fat.offset);

	    typtr := boolptr;
	    mode := conditionMod;
	    tlabel := NIL;
	    flabel := NIL;
	    atype := signed;

	    (* set final condition codes *)
	    IF op = geq THEN
	       EmitRI(XI, r1, r0, maxcard); (* complement *)
	       EmitRX3(N, r1, lataddrReg, indexReg, latoffset);
	       test := eq;
	       flabel := label;
	    ELSE
	       EmitRX3(C, r1, lataddrReg, indexReg, latoffset);
	       IF op = neq THEN
		  test := ne; tlabel := label;
	       ELSE
		  test := eq; flabel := label;
	       END;
	    END;
	 END;
      END;

      IF withbxle THEN
	 FreeRegTriplet(indexReg);
      ELSE
	 FreeReg(indexReg);
      END;
      Cleanup(lat);
   END BigSetOp;

   PROCEDURE Factor(VAR fat: Attribut);   
      VAR rptr: POINTER TO (* REAL *)
                        RECORD r1, r2: CARDINAL END;
      VAR slabel: LabelPtr;

      PROCEDURE SetConstructor(VAR fat: Attribut);
         VAR at1, at2: Attribut;
             spat: BITSET;
             help: Reg;
             regused: BOOLEAN;
             dummy: Attribut;
             basetype: Stptr;
      BEGIN
         WITH fat.typtr^ DO
            Assert(form = sets);
            basetype := basep;
         END;
         regused := FALSE;
         WITH fat DO
            mode := loadedMod;
            GetReg(loadReg);
         END;
         spat := { };
         GetSymbol; (* lconbr *)
         WHILE sy <> rconbr DO
            Expression(at1);
            RangeCheck(basetype, at1, arithmeticRangeCheck);
            IF at1.mode <> constantMod THEN
               Load(at1);
            END;
            IF sy = range THEN
               GetSymbol; (* range *)
               Expression(at2);
               RangeCheck(basetype, at2, arithmeticRangeCheck);
               IF (at1.mode = constantMod) AND (at2.mode = constantMod) THEN
                  spat := spat + {at1.value..at2.value};
               ELSE
                  IF regused THEN
                     help := fat.loadReg;
                     GetReg(fat.loadReg);
                  END;
                  EmitSF(LCS, fat.loadReg, 1); (* set all bits *)
                  IF at1.mode = constantMod THEN
                     EmitRI(SLL, fat.loadReg, r0, at1.value);
                     WITH dummy DO
                        mode := loadedMod;
                        GetReg(loadReg);
                        typtr := cardptr;
                     END;
                     EmitRI(LHI, dummy.loadReg, r0, BitsPerWord-1);
                     EmitOp(SR, S, dummy, at2);
                     EmitRI(SRL, fat.loadReg, dummy.loadReg, at1.value);
                     EmitRI(SLL, fat.loadReg, dummy.loadReg, 0);
                     FreeReg(dummy.loadReg);
                  ELSE
                     (* at1.mode = loadedMod *)
                     EmitRI(SLL, fat.loadReg, at1.loadReg, 0);
                     IF at2.mode = constantMod THEN
                        EmitRI(SRL, fat.loadReg, at1.loadReg, BitsPerWord-1 -
                                                              at2.value);
                        EmitRI(SLL, fat.loadReg, r0, BitsPerWord-1 - at2.value);
                     ELSE
                        WITH dummy DO
                           mode := loadedMod;
                           GetReg(loadReg);
                           typtr := cardptr;
                        END;
                        EmitRR(LR, dummy.loadReg, at1.loadReg);
                        EmitOp(SR, S, dummy, at2);
                        EmitRI(SRL, fat.loadReg, dummy.loadReg, BitsPerWord-1);
                        EmitRR(SR, dummy.loadReg, at1.loadReg);
                        EmitRI(SLL, fat.loadReg, dummy.loadReg, BitsPerWord-1);
                        FreeReg(dummy.loadReg);
                     END;
                     FreeReg(at1.loadReg);
                  END;
                  IF regused THEN
                     EmitRR(ORR, fat.loadReg, help);
                     FreeReg(help);
                  ELSE
                     regused := TRUE;
                  END;
               END;
            ELSE
               IF at1.mode = constantMod THEN
                  INCL(spat, at1.value);
               ELSE
                  Load(at1);
                  IF regused THEN
                     help := fat.loadReg;
                     GetReg(fat.loadReg);
                  END;
                  EmitRI(LI, fat.loadReg, r0, CARDINAL({0}));
                  EmitRI(SRL, fat.loadReg, at1.loadReg, 0);
                  FreeReg(at1.loadReg);
                  IF regused THEN
                     EmitRR(ORR, fat.loadReg, help);
                     FreeReg(help);
                  ELSE
                     regused := TRUE;
                  END;
               END;
            END;
         END; (* WHILE *)
         IF spat <> {} THEN
            EmitRI(OI, fat.loadReg, r0, CARDINAL(spat));
         END;
         GetSymbol; (* rconbr *)
      END SetConstructor;

      PROCEDURE BigSetConstructor(VAR fat: Attribut);
         VAR at1, at2: Attribut;
             spat: Constval;
	     styp: Stptr;
	     loop, endloop: LabelPtr;
             setconstlabel: LabelPtr;
      BEGIN
         GetLabel(stringl, setconstlabel);
	 InitConstSet(spat, fat.typtr);
         spat.setvalue^.label := setconstlabel;
	 styp := fat.typtr^.basep;
	 GetStackAt(fat);
         (* copy setconstant at setconstlabel to set on stack *)
         EmitRI(LI, r1, r0, fat.typtr^.size);
         (* r1: # bytes to be moved *)
         EmitRXRXLabel(MOVE, r1, base, r0, "", noOpr, fat.offset,
                             r1, r0, r0, setconstlabel^, noOpr, 0);
         (* contents of r1 destroyed *)
         GetSymbol; (* lconbr *)
         WHILE sy <> rconbr DO
            Expression(at1);
	    RangeCheck(styp, at1, arithmeticRangeCheck);
            IF at1.mode <> constantMod THEN
               Load(at1);
	       IF fat.typtr^.offset > 0 THEN
		  EmitRI(SI, at1.loadReg, r0, fat.typtr^.offset);
	       END;
            END;
            IF sy = range THEN
               GetSymbol; (* range *)
               Expression(at2);
	       RangeCheck(styp, at2, arithmeticRangeCheck);
               IF (at1.mode = constantMod) AND (at2.mode = constantMod) THEN
		  ConstSetElement(spat, at1.value, at2.value);
               ELSE
		  Load(at1); Load(at2);
		  IF fat.typtr^.offset > 0 THEN
		     EmitRI(SI, at2.loadReg, r0, fat.typtr^.offset);
		  END;
		  GetLabel(forl, loop);
		  GetLabel(forl, endloop);
		  EmitRR(CR, at1.loadReg, at2.loadReg);
		  EmitBranch(BP, r0, r0, endloop^);
		  EmitLabel(loop);
		  EmitRX3(SBT, at1.loadReg, base, r0, fat.offset);
		  EmitSF(AIS, at1.loadReg, 1);
		  EmitRR(CR, at1.loadReg, at2.loadReg);
		  EmitBranch(BNP, r0, r0, loop^);
		  EmitLabel(endloop);
		  DISPOSE(loop);
		  DISPOSE(endloop);
                  Cleanup(at1); Cleanup(at2);
               END;
            ELSE
               IF at1.mode = constantMod THEN
		  ConstSetElement(spat, at1.value, at1.value);
               ELSE
		  (* at1.mode = loadedMod *)
		  EmitRX3(SBT, at1.loadReg, base, r0, fat.offset);
                  Cleanup(at1);
               END;
            END;
         END; (* WHILE *)
         GetSymbol; (* rconbr *)
	 TermConstSet(spat);
      END BigSetConstructor;

   BEGIN (* Factor *)  
      IF sy = lparent THEN
         GetSymbol;
	 Expression(fat);
	 GetSymbol; (* rparent *)  
      ELSIF sy = notsy THEN
	 (* fat.typtr = boolptr *)
	 GetSymbol;
	 Factor(fat);         
	 IF fat.mode <> constantMod THEN
	    LoadCond(fat);
	    WITH fat DO
	       test := Invert(test);
	       slabel := tlabel; tlabel := flabel; flabel := slabel;
	    END;
	 ELSE
	    fat.value := 1-fat.value (* NOT fat.value *)
	 END;
      ELSIF (sy = namesy) OR (sy = field) THEN
         Designator(fat);  
         IF sy = lparent THEN (* function call *)  
            GetSymbol;
	    ProcFuncCall(fat);
         ELSIF sy = lconbr THEN
            IF (fat.typtr <> NIL) AND (fat.typtr^.form = bigsets) THEN
               BigSetConstructor(fat);
            ELSE
               SetConstructor(fat);
            END;
         END
      ELSIF sy = lconbr THEN
         fat.typtr := bitsetptr;
         SetConstructor(fat);
      ELSE (* Constant *)   
         Assert(sy = anycon);  
         WITH fat DO 
            typtr := cstPtr;  
	    IF (typtr <> NIL) AND (typtr^.form = bigsets) THEN
	       mode := setConstMod;
	       setPtr := SetValuePtr(cString);
            ELSIF NOT SimpleType(fat) THEN                         
               mode := stringConstMod;
	       strgPtr := cString; (* see MCP4Scanner *)
            ELSIF BaseType(typtr) = realptr THEN 
               mode := doubleConstMod; 
               rptr := ADDRESS(cString);
               Real1 := rptr^.r1;  Real2 := rptr^.r2;
               Load(fat);
            ELSE
	       mode := constantMod;
               value := CARDINAL(val);
            END;
         END;
         GetSymbol;
      END;
   END Factor;   

   PROCEDURE Adapt(VAR l1, l2: LabelPtr);
   BEGIN
      IF l2 <> NIL THEN
	 IF l1 = NIL THEN
	    l1 := l2;
	 ELSE
	    EnterEQU(l2, l1);
	    DISPOSE(l2);
	 END;
      END;
   END Adapt;

   PROCEDURE Term(VAR fat: Attribut);   
      VAR 
         lat: Attribut;
	 opsy: Symbol;  
         AType: ArithmeticType;

      PROCEDURE Multiplication(VAR fat, lat: Attribut);
         VAR cat: Attribut;
	     opt: BOOLEAN;                     
             tmpat: Attribut; (* used for exchange of fat and lat *)
      BEGIN
	 opt := FALSE;
         GetSymbol;
         Factor(lat);
         IF (fat.mode <> loadedMod) AND (lat.mode = loadedMod) THEN
            tmpat := fat; fat := lat; lat := tmpat;
         END;
         CASE Arithmetic(fat, lat) OF
            bitwise: 
		IF (fat.typtr^.form = bigsets) AND
		   (fat.typtr^.size > oneword) THEN
		   BigSetOp(times, fat, lat);
		ELSE
		   Load(fat);
		   EmitOpConst(NR, N, NI, fat, lat);
		END;

         |  floating:
                Load(fat);
                EmitOp(MDR, MD, fat, lat);

         |  signed, unSigned:
                IF fat.mode = constantMod THEN   
                   Load(lat);
                   ConstMul(Arithmetic(fat, lat), lat, fat.value);
		   fat.mode := loadedMod;
                   IF lat.mode <> loadedMod THEN
                      Load(lat);
                   END;
		   fat.loadReg := lat.loadReg;
                ELSIF lat.mode = constantMod THEN
                   Load(fat);
                   ConstMul(Arithmetic(fat, lat), fat, lat.value);
                ELSE
                   Mult(fat, lat, Arithmetic(fat, lat));
                END;
         END;
      END Multiplication;

   BEGIN (* Term *)  
      Factor(fat);  
      WHILE (sy >= andsy) AND (sy <= modsy) DO
	 opsy := sy;
         IF sy = times THEN
            Multiplication(fat, lat)
         ELSE
            (* special case: short circuit AND *)
            IF opsy = andsy THEN
	       LoadCond(fat);
	       WITH fat DO
		  IF flabel = NIL THEN
		     GetLabel(ifl, flabel);
		  END;
		  test := Invert(test); (* now branch on false condition *)
		  Test(test, atype, flabel);
		  IF tlabel <> NIL THEN
		     EmitLabel(tlabel);
		     DISPOSE(tlabel);
		  END;
	       END;
            END;
            GetSymbol;  
            Factor(lat); 
            AType := Arithmetic(fat, lat);
            CASE opsy OF
               andsy:
                  Assert(AType = logical);
		  LoadCond(lat);
		  fat.test := lat.test;
		  Adapt(fat.tlabel, lat.tlabel);
		  Adapt(fat.flabel, lat.flabel);
                  fat.atype := lat.atype;

            |  divsy, modsy:
		  IF lat.mode = constantMod THEN
		     IF opsy = divsy THEN
			ConstDiv(AType, fat, lat.value)
		     ELSE
			ConstMod(AType, fat, lat.value)
		     END;
		  ELSE 
                     DivMod(fat, lat, opsy, AType);
		  END;
          
            |  slash:
                  CASE AType OF
                        bitwise:
			   IF (fat.typtr^.form = bigsets) AND
			      (fat.typtr^.size > oneword) THEN
			      BigSetOp(slash, fat, lat);
			   ELSE
			      Load(fat);
			      EmitOpConst(XR, X, XI, fat, lat);
			   END;
                     |  floating:
			   Load(fat);
                           EmitOp(DDR, DD, fat, lat);
                  END;

            END
         END;
         fat.typtr := ResultType(fat, lat)
      END;  
   END Term;  

   PROCEDURE SimpleExpression(VAR fat: Attribut);   
      (* fat describes the first operand and the result *)
      VAR 
         opsy: Symbol;  
         lat: Attribut; (* descriptor of second operand *)
         negb: BOOLEAN; (* a negation has to take place *)  
         AType: ArithmeticType;
         tmpat: Attribut; (* used for exchange of fat and lat *)
   BEGIN
      negb := sy = minus; 
      IF negb THEN
	 GetSymbol
      END;  
      Term(fat);  
      IF negb THEN  
         IF Arithmetic(fat, fat) = floating THEN
            Load(fat);
	    EmitFloatRR(LCDR, fat.floatLoadReg, fat.floatLoadReg);
         ELSIF fat.mode <> constantMod THEN
            (* negation *)
            LoadReg(fat, r0);
	    GetReg(fat.loadReg);
	    EmitRR(SR, fat.loadReg, fat.loadReg);
	    EmitRR(SR, fat.loadReg, r0)
         ELSE 
            fat.iValue := -fat.iValue  
         END  
      END;  
      WHILE (sy >= plus) AND (sy <= orsy) DO
	 opsy := sy;
         (* special case: short circuit OR *)
         IF opsy = orsy THEN
	    LoadCond(fat);
	    WITH fat DO
	       IF tlabel = NIL THEN
		  GetLabel(ifl, tlabel);
	       END;
	       Test(test, atype, tlabel);
	       IF flabel <> NIL THEN
		  EmitLabel(flabel);
		  DISPOSE(flabel);
	       END;
	    END;
         END;
         GetSymbol;
         Term(lat);
         AType := Arithmetic(fat, lat);
         CASE opsy OF   
            minus:
               CASE AType OF
                     signed, unSigned:
			Load(fat);
                        EmitOpConst(SR, S, SI, fat, lat);
                  |  bitwise:
			IF (fat.typtr^.form = bigsets) AND
			   (fat.typtr^.size > oneword) THEN
			   BigSetOp(minus, fat, lat);
			ELSE
			   Load(fat);
			   Load(lat);
			   EmitSF(LCS, r0, 1);
			   EmitRR(XR, lat.loadReg, r0);
			   EmitRR(NR, fat.loadReg, lat.loadReg);
			   FreeReg(lat.loadReg);
			END;
                  |  floating:
			Load(fat);
                        EmitOp(SDR, SD, fat, lat);
               END
         |  plus:
               IF (fat.mode <> loadedMod) AND
                  (lat.mode = loadedMod) THEN
                  tmpat := fat; fat := lat; lat := tmpat;
               END;
               CASE AType OF
                     signed, unSigned:
			Load(fat);
                        EmitOpConst(AR, A, AI, fat, lat);
                  |  bitwise:
			IF (fat.typtr^.form = bigsets) AND
			   (fat.typtr^.size > oneword) THEN
			   BigSetOp(plus, fat, lat);
			ELSE
			   Load(fat);
			   EmitOpConst(ORR, O, OI, fat, lat);
			END;
                  |  floating:
			Load(fat);
                        EmitOp(ADRR, AD, fat, lat);
               END;
         |  orsy:        
	       LoadCond(lat);
	       fat.test := lat.test;
	       Adapt(fat.tlabel, lat.tlabel);
	       Adapt(fat.flabel, lat.flabel);
               fat.atype := lat.atype;
         END;  
         fat.typtr := ResultType(fat, lat);
      END;  
   END SimpleExpression;   

   PROCEDURE Expression(VAR fat: Attribut);          
      (* fat describes the first operand and the result *)
      VAR 
         opsy : Symbol;
         lat: Attribut; (* descriptor of second operand *)  
	 AType: ArithmeticType;
	 testcond: TestType;

   BEGIN (* Expression *)
      SimpleExpression(fat);
      IF (sy >= eql) AND (sy <= insy) THEN (* relational operator ? *)
         opsy := sy;
         IF fat.mode = conditionMod THEN
            Load(fat); (* necessary for (bool_expr) op (bool_expr) *)
         END;
         GetSymbol;
         SimpleExpression(lat);
         IF (lat.typtr^.form = bigsets) AND
	    ((lat.typtr^.size > oneword) OR (opsy = insy)) THEN
	    BigSetOp(opsy, fat, lat);
         ELSIF opsy = insy THEN
            Load(fat);
	    EmitSF(LIS, r0, 1);
	    EmitRI(SLL, r0, r0, 31);
	    EmitRI(SRL, r0, fat.loadReg, 0);
	    EmitRR(LR, fat.loadReg, r0);
            EmitOpConst(NR, N, NI, fat, lat);
            FreeReg(fat.loadReg);
            WITH fat DO
               mode := conditionMod;
	       test := ne; (* BNZ *)
	       tlabel := NIL;
	       flabel := NIL;
	       atype := signed;
               typtr := boolptr;
            END;
         ELSE (* (opsy >= eql) AND (opsy <= leq) *)
            IF (fat.typtr^.form IN Stset{bigsets, sets}) AND
	       ((opsy = geq) OR (opsy = leq)) THEN
               (* set operation >=,<= *)
	       IF opsy = geq THEN
	          Load(fat);
	          EmitRI(XI, fat.loadReg, r0, maxcard); (* complement *)
	          EmitOpConst(NR, N, NI, fat, lat);
               ELSE
                  Load(lat);
	          EmitRI(XI, lat.loadReg, r0, maxcard);
	          EmitOpConst(NR, N, NI, lat, fat);
	          fat := lat;
	       END;
	       testcond := eq; (* atype becomes *) AType := signed;
            ELSE
	       IF lat.mode = conditionMod THEN
		  Load(lat); (* necessary for (bool_expr) op (bool_expr) *)
	       END;
               Load(fat);
	       AType := Arithmetic(fat, lat);
	       CASE opsy OF  
	       | neq: testcond := ne;
	       | eql: testcond := eq;
	       | leq: testcond := le;
	       | lss: testcond := lt;
	       | geq: testcond := ge;
	       | grt: testcond := gt;
	       END;
	       CASE AType OF
	          signed:
                     EmitOpConst(CR, C, CI, fat, lat);
	       |  unSigned, logical, bitwise:
                     IF ByteSize(lat.typtr) THEN
                        EmitOpConst(CLR, CLB, CLHI, fat, lat);
                     ELSE
                        EmitOpConst(CLR, CL, CLI, fat, lat);
                     END;
	       |  floating:
                     EmitOp(CDR, CD, fat, lat);
	       END;
            END;
            IF fat.mode = loadedMod THEN
               FreeReg(fat.loadReg);
	    ELSE (* fat.mode = floatLoadedMod *)
	       FreeFloatReg(fat.floatLoadReg);
	    END;
            WITH fat DO
               typtr := boolptr;
	       mode := conditionMod;
	       tlabel := NIL;
	       flabel := NIL;
	       test := testcond;
	       IF AType = signed THEN
		  atype := signed;
	       ELSE
		  atype := unSigned;
	       END;
            END  
         END  
      END
   END Expression;  

   (*
    *	restrictions for following procedures
    *
    *	they must not call any procedures from MCP4ConstArithmetic
    *	if the loadReg of fat is freed they must not reallocate this register
    *)

   (* only signed/unsigned *)

   PROCEDURE Mult(VAR fat, lat: Attribut; AType: ArithmeticType);
      VAR
         regpair: Reg; (* register pair for multiplication *)
         second: Reg; (* second register of regpair *)
         done: BOOLEAN; (* RegRequest successfull ? *)
         tmpat: Attribut;
   BEGIN
      IF (lat.mode = loadedMod) AND
         (fat.mode <> loadedMod) THEN
         tmpat := lat; lat := fat; fat := tmpat;
      END;
      IF (fat.mode = loadedMod) AND
         ODD(ORD(fat.loadReg)) THEN
         second := fat.loadReg;
         regpair := second; DEC(regpair);
         RegRequest(regpair, done);
      ELSE
         done := FALSE;
      END;
      IF NOT done THEN
         GetRegPair(regpair, done);
         IF NOT done THEN
            regpair := r0;
            Load(lat); (* MUST be done; Load may destroy r0/r1 *)
         END;
         second := regpair; INC(second);
         LoadReg(fat, second);
      END;
      fat.loadReg := regpair; (* for EmitOp *)
      EmitOp(MR, M, fat, lat);
      IF regpair = r0 THEN
         GetReg(second);
         fat.loadReg := r1;
         LoadReg(fat, second);
      ELSE
         fat.loadReg := second;
         FreeReg(regpair);
      END;
   END Mult;

   PROCEDURE Div(VAR fat, lat: Attribut; AType: ArithmeticType);
   BEGIN
      DivMod(fat, lat, divsy, AType);
   END Div;

   PROCEDURE Mod(VAR fat, lat: Attribut; AType: ArithmeticType);
   BEGIN
      DivMod(fat, lat, modsy, AType);
   END Mod;

   PROCEDURE DivMod(VAR fat, lat: Attribut; opsy: Symbol (* divsy or modsy *);
                    AType: ArithmeticType);
      (* BUG (of PE 3200): x MOD y fails if x > maxint and y = 1 !! *)
      VAR
         regpair: Reg; (* register pair for div/mod *)
         second: Reg; (* second register of regpair *)
         done: BOOLEAN; (* RegRequest successfull ? *)
   BEGIN
      IF (fat.mode = loadedMod) AND
         ODD(ORD(fat.loadReg)) THEN
         regpair := fat.loadReg; DEC(regpair);
         RegRequest(regpair, done);
         IF done THEN
            second := fat.loadReg;
         END;
      ELSE
         done := FALSE;
      END;
      IF NOT done THEN
         GetRegPair(regpair, done);
         IF NOT done THEN
            regpair := r0;
            Load(lat); (* MUST be done; Load may destroy r0/r1 *)
         END;
         second := regpair; INC(second);
         LoadReg(fat, second);
      END;
      IF AType = signed THEN (* sign extension *)
         EmitRR(LR, regpair, second);
         EmitRI(SRA, regpair, r0, 31);
      ELSE
         EmitSF(LIS, regpair, 0);
      END;
      fat.loadReg := regpair; (* for EmitOp *)
      EmitOp(DR, D, fat, lat);
      IF opsy = divsy THEN
         fat.loadReg := second;
         IF regpair <> r0 THEN
            FreeReg(regpair);
         END;
      ELSE
         (* fat.loadReg := regpair *)
         IF regpair <> r0 THEN
            FreeReg(second);
         END;
      END;
      IF regpair = r0 THEN
         GetReg(second);
         LoadReg(fat, second);
      END;
   END DivMod;

END MCP4ExpressionSys.  
