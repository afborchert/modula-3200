IMPLEMENTATION MODULE MCP4ForSys; (* AFB 9/83 *)

   FROM Storage IMPORT DEALLOCATE;
   FROM MCBase IMPORT Symbol, adc, maxint, Stptr, charptr, maxcard,
      Structform;
   FROM MCP4AttributSys IMPORT Attribut, ArithmeticType, AtMode;
   FROM MCP4Load IMPORT Load, LoadReg, LoadAddr, LoadConstant;
   FROM MCP4Scanner IMPORT GetSymbol, sy, val;
   FROM MCP4Labels IMPORT LabelPtr, GetLabel, forl, Label;
   FROM MCP4CodeSys IMPORT EmitSF, AppendComment, EmitRX3, EmitBranch,
      EmitLabel, EmitRX3Int, EmitRR, EmitRI, EmitRX3Label;
   FROM MCP4ExpressionSys IMPORT Expression;
   FROM MCP4Designator IMPORT Designator;
   FROM MCP4StatSys IMPORT StatSequ1;
   FROM MCP4Register IMPORT Reg, GetReg, FreeReg;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Global IMPORT Error, CompilerError;
   FROM MCP4Types IMPORT ByteSize, Arithmetic, TestBaseType, BaseType,
      intcarptr;
   FROM MCP4Stack IMPORT GetStack, FreeStack;
   FROM MCP4Address IMPORT Address, Cleanup;

   (*
    *	FOR index := start TO end BY step DO
    *	   .
    *      .
    *   END;
    *
    *	design of for-loop:
    *
    *	(strategy = withOverflow)
    *
    *		l	r0,start
    *		b	compare
    *	begin	equ *
    *		st	r0,index
    *		.
    *		.
    *		l	r0,index
    *		ais	r0,step
    *	compare	equ *
    *		c	r0,end
    *		b<=	begin
    *
    *	(strategy = noOverflow / ABS(step) = 1)
    *
    *		l	r0,start
    *		c	r0,end
    *		b>	endfor
    *		b	assign
    *	begin	equ	*
    *		ais	r0,step
    *	assign	equ	*
    *		st	r0,index
    *		.
    *		.
    *		l	r0,index
    *		c	r0,end
    *		b<	begin
    *
    *   (strategy = noOverflow / ABS(step) > 1)
    *
    *		l	r0,start
    *		c	r0,end
    *		b>	endfor
    *		b	assign
    *	begin	equ	*
    *		ais	r0,1
    *	assign	equ	*
    *		st	r0,index
    *		.
    *		.
    *		l	r0,index
    *		ai	r0,step-1
    *		c	r0,end
    *		b<	begin
    *)

   PROCEDURE ForStatement;   
      TYPE
         TestType = (lt, le, eq, ne, ge, gt);
      VAR 
         cntat: Attribut; (* attribut of counter/index *)
         stat: Attribut;  (* attribut of start value *)
         lat: Attribut;   (* attribut of limit and step value *)
         step: INTEGER;  
	 compLabel : LabelPtr;
	 beginforLabel : LabelPtr;
         endforLabel: LabelPtr;
         assignLabel: LabelPtr;
         byteSize: BOOLEAN; (* = ByteSize(cntat.typtr) *)
         tmpReg: Reg; (* only used if byteSize *)
	 (* if index is addressable in RX3-format: *)
	 FX, SX: Reg;
	 LabelStr: Label;
	 Opr: CHAR;
	 Operand: CARDINAL;
	 RX3: BOOLEAN;
         (* offsets relative to base+offset-adc *)
         (* if zero: no allocation on stack *)
	 limitOffset: CARDINAL; (* only if not constant *)
	 addrOffset: CARDINAL; (* only if not RX3 *)
         offset: CARDINAL; (* relative to base *)
	 space: CARDINAL; (* on stack used *)
	 limitConst: CARDINAL;
	 store: Mnemonic; (* ST or STB *)
	 load: Mnemonic;
	 atype: ArithmeticType; (* signed or unsigned compare ? *)
	 rstype: Stptr;
	 initlow: BOOLEAN; (* if on: 0 <= init value <= maxint *)
         atleastone: BOOLEAN;
         strategy: (withOverflow, noOverflow);
         min, max: CARDINAL;

      PROCEDURE Invert(t: TestType) : TestType;
      BEGIN
         (* needed if step < 0 *)
         CASE t OF
           lt: RETURN gt;
         | le: RETURN ge;
         | eq: RETURN eq;
         | ne: RETURN ne;
         | ge: RETURN le;
         | gt: RETURN lt;
         END;
      END Invert;

      PROCEDURE BranchMnem(t: TestType; atype: ArithmeticType;
                           VAR bm1, bm2: Mnemonic);
      BEGIN
         IF atype = signed THEN
            CASE t OF
              lt: bm1 := BM;
            | le: bm1 := BNP;
            | eq: bm1 := BE;
            | ne: bm1 := BNE;
            | ge: bm1 := BNM;
            | gt: bm1 := BP;
            END;
            bm2 := bm1;
         ELSE
            CASE t OF
              lt: bm1 := BL;  bm2 := BL;
            | le: bm1 := BL;  bm2 := BE;
            | eq: bm1 := BE;  bm2 := BE;
            | ne: bm1 := BNE; bm2 := BNE;
            | ge: bm1 := BNL; bm2 := BNL;
            | gt: bm1 := BE;  bm2 := BNL;
            END;
         END;
      END BranchMnem;

      PROCEDURE Test(test: TestType; atype: ArithmeticType; dest: LabelPtr);
         VAR cmp, bm1, bm2: Mnemonic;
             testLabel: LabelPtr;
      BEGIN
         (* actual counter value in r0 *)
         IF limitOffset <> 0 THEN
	    IF atype = unSigned THEN cmp := CL ELSE cmp := C END;
	    EmitRX3Int(cmp, r0, base, r0, limitOffset+offset-adc);
         ELSE
	    IF atype = unSigned THEN cmp := CLI ELSE cmp := CI END;
	    EmitRX3(cmp, r0, r0, r0, limitConst);
         END;
         IF step < 0 THEN test := Invert(test) END;
         BranchMnem(test, atype, bm1, bm2);
         IF (bm1 = BE) AND (bm2 = BNL) THEN
            GetLabel(forl, testLabel);
            EmitBranch(BE, r0, r0, testLabel^);
            EmitBranch(BNL, r0, r0, dest^);
            EmitLabel(testLabel);
            DISPOSE(testLabel);
         ELSE
            EmitBranch(bm1, r0, r0, dest^);
            IF bm1 <> bm2 THEN
               EmitBranch(bm2, r0, r0, dest^);
            END;
         END;
      END Test;

      PROCEDURE LoadIndex;
      BEGIN
         IF RX3 THEN
            EmitRX3Label(load, r0, FX, SX, LabelStr, Opr, Operand);
         ELSE
            EmitRX3Int(L, r1, base, r0, addrOffset+offset-adc);
            EmitRX3(load, r0, r1, r0, 0);
         END;
      END LoadIndex;

      PROCEDURE Increment(step: INTEGER);
      BEGIN
         (* increment/decrement counter value *)
         IF step > 0 THEN
            EmitRI(AI, r0, r0, CARDINAL(step));
         ELSE
            EmitRI(SI, r0, r0, CARDINAL(-step));
         END;
      END Increment;

      PROCEDURE SGN(step: INTEGER) : INTEGER;
      BEGIN
         IF step > 0 THEN
            RETURN 1
         ELSE
            RETURN -1
         END;
      END SGN;

      PROCEDURE MinMax(type: Stptr; VAR cmin, cmax: CARDINAL);
      BEGIN
         WITH type^ DO
            CASE form OF
              subranges:
               cmin := min; cmax := max;
            | enums:
               cmin := 0; cmax := cstnr;
            | bools:
               cmin := 0; cmax := 1;
            | cards:
               cmin := 0; cmax := maxcard;
            | ints:
               cmin := CARDINAL(-maxint-1); cmax := maxint;
            | chars:
               cmin := 0; cmax := 377B;
            | opens:
               MinMax(openstruc, cmin, cmax);
            ELSE
               CompilerError;
            END;
         END;
      END MinMax;

      PROCEDURE InRange() : BOOLEAN;
      BEGIN
         IF (rstype = charptr) OR (rstype = intcarptr) AND (step < 0) THEN
            RETURN TRUE
         END;
         IF limitOffset <> 0 THEN RETURN FALSE END;
         IF atype = signed THEN
            RETURN (step > 0) AND (INTEGER(limitConst) <= maxint-step) OR
                   (step < 0) AND (INTEGER(limitConst) >= -maxint-1-step);
         ELSE
            RETURN (step > 0) AND (limitConst <= maxcard-CARDINAL(step)) OR
                   (step < 0) AND (limitConst >= CARDINAL(ABS(step)));
         END;
      END InRange;

   BEGIN  
      AppendComment("for-statement");
      GetLabel(forl, beginforLabel);
      GetLabel(forl, compLabel);
      GetLabel(forl, assignLabel);
      GetLabel(forl, endforLabel);
      Designator(cntat); (* get attribut of counter *)
      byteSize := ByteSize(cntat.typtr);
      Address(cntat, FX, SX, LabelStr, Opr, Operand, RX3);
      (* parameters are allowed as control variable *)
      IF (FX <> r0) AND (FX <> base) OR (SX <> r0) THEN
	 (* rx3 format too expensive *)
	 RX3 := FALSE
      END;
      space := 0;
      IF NOT RX3 THEN
	 LoadAddr(cntat);  
	 INC(space, adc);
         addrOffset := space; (* relative to base+offset-adc *)
      ELSE
	 addrOffset := 0; (* address not on stack *)
      END;

      GetSymbol; (* comma *)  
      Expression(stat);
      atype := Arithmetic(cntat, stat);
      initlow := (stat.mode = constantMod) AND (atype = unSigned)
	 AND (stat.value <= maxint);

      IF byteSize THEN
	 store := STB;
	 load := LB;
      ELSE
	 store := ST;
	 load := L;
      END;

      GetSymbol; (* tosy *)  
      Expression(lat);
      IF lat.mode = constantMod THEN
	 limitConst := lat.value;
	 limitOffset := 0;
	 IF initlow AND (lat.value <= maxint) THEN
	    atype := signed; (* produces more efficient code *)
	 END;
      ELSE
	 Load(lat);  
         limitOffset := addrOffset + adc;
	 INC(space, adc);
      END;

      (* signed compare allowed ?? *)
      IF (atype = unSigned) THEN
	 rstype := TestBaseType(cntat.typtr);
	 IF (rstype = charptr) OR (rstype = intcarptr) THEN
	    atype := signed; (* produces more efficient code *)
	 END;
      END;
      IF space > 0 THEN
         GetStack(space DIV adc, offset);
      END;
      IF limitOffset <> 0 THEN
	 AppendComment("limit");
	 (* use ST even if bytesize !! *)
	 EmitRX3(ST, lat.loadReg, base, r0, limitOffset+offset-adc);
	 FreeReg(lat.loadReg);
      END;
      IF NOT RX3 THEN
	 AppendComment("index address");
	 EmitRX3Int(ST, cntat.addrReg, base, r0, addrOffset+offset-adc);
	 IF cntat.mayRelease THEN
	    FreeReg(cntat.addrReg);
	 END;
      END;

      IF sy = bysy THEN
	 GetSymbol;
	 GetSymbol;  
         step := val  
      ELSE
	 step := 1
      END;  
      IF step = 0 THEN
	 Error(202)
      END;

      (* strategy ?? *)
      IF InRange() THEN
         (* limitConst + step is in [min..max] *)
         strategy := withOverflow;
      ELSE
         strategy := noOverflow;
      END;
      MinMax(cntat.typtr, min, max);
      atleastone := (limitOffset = 0) AND
         ((step < 0) AND (limitConst = min) OR
          (step > 0) AND (limitConst = max));

      LoadReg(stat, r0);

      IF strategy = withOverflow THEN
         (* branch to loop condition *)
         EmitBranch(B, r0, r0, compLabel^);
      ELSE
         IF NOT atleastone THEN
            Test(gt, atype, endforLabel);
         END;
         EmitBranch(B, r0, r0, assignLabel^);
      END;

      (* top of the loop *)
      EmitLabel(beginforLabel);

      IF strategy = noOverflow THEN
         Increment(SGN(step));
      END;

      (* store actual index value (in r0) back *)
      EmitLabel(assignLabel);
      IF RX3 THEN
         EmitRX3Label(store, r0, FX, SX, LabelStr, Opr, Operand);
      ELSE
         (* index address on stack *)
         EmitRX3Int(L, r1, base, r0, addrOffset+offset-adc);
         EmitRX3(store, r0, r1, r0, 0);
      END;

      (* body of the loop *)
      StatSequ1(endsy);

      LoadIndex;
      IF strategy = withOverflow THEN
         Increment(step);
      ELSIF (* strategy = noOverflow AND *) ABS(step) > 1 THEN
         Increment(step - SGN(step));
      END;

      (* condition part *)
      EmitLabel(compLabel);
      IF strategy = withOverflow THEN
         Test(le, atype, beginforLabel);
      ELSE
         IF ABS(step) = 1 THEN
            Test(ne, atype, beginforLabel);
         ELSE
            Test(lt, atype, beginforLabel);
         END;
      END;

      EmitLabel(endforLabel);
      AppendComment("endfor");

      (* cleanup *)
      DISPOSE(beginforLabel);
      DISPOSE(compLabel);
      DISPOSE(assignLabel);
      DISPOSE(endforLabel);
      IF space > 0 THEN
         FreeStack(space DIV adc, offset);
      END;
      IF RX3 THEN
	 Cleanup(cntat);
      END;
      GetSymbol;  
   END ForStatement;  
      
END MCP4ForSys.
