IMPLEMENTATION MODULE MCP4RangeChecks; (* CHJ *) (* REV AFB 7/84 *)

   FROM MCBase IMPORT Stptr, oneword, maxint, maxcard, intptr, cardptr,
      charptr, Structform, addrptr;
   FROM MCP4AttributSys IMPORT AtMode, Attribut;
   FROM MCP4CodeSys IMPORT EmitBranch, EmitRR, EmitDCFValue, noOpr,
      EmitRX3Label, EmitExtern;
   FROM MCP4Global IMPORT Assert, Error;
   FROM MCP4Load IMPORT Load;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Register IMPORT Reg, top, limit;
   FROM MCP4Types IMPORT TestBaseType, SizeType, BaseType, intcarptr;
   FROM MCP4Public IMPORT sflag;
   FROM MCTypes IMPORT IsInt, IsCard;

   (*
    *	tests assignment of a constant to a typed variable;
    *	constant in an Attribut variable allows distinction
    *	of very negative integers from very big cardinals
    *)

   PROCEDURE RangeCheckForConstant(dest: Stptr; VAR fat: Attribut);
      VAR base: Stptr;
   BEGIN
      WITH fat DO
         IF mode = stringConstMod THEN
            CharCast(fat);
            (* mode = constantMod *)
         END;
	 IF mode = constantMod THEN
	    base := TestBaseType(dest);
	    WITH dest^ DO
	       IF form = subranges THEN
		  IF IsInt(base) THEN
		     IF (INTEGER(max) < iValue) OR
			(INTEGER(min) > iValue) THEN
			Error(222);
		     END;
		  ELSE
		     IF (max < value) OR (min > value) THEN
			Error(222);
		     END;
		  END;
	       END;
	       IF IsInt(base) THEN
		  IF (TestBaseType(typtr) = cardptr) AND (value > maxint) THEN
		     Error(222);
		  END;
	       ELSIF IsCard(base) THEN
		  IF (TestBaseType(typtr) = intptr) AND (value > maxint) THEN
		     Error(222);
		  END;
	       ELSIF dest = charptr THEN
		  IF value > 377B THEN
		     Error(222);
		  END;
	       ELSIF form = enums THEN
		  IF value > cstnr THEN
		     Error(222);
		  END;
	       END;
	    END; (* WITH dest DO *)
	 END; (* IF mode = constantMod *)
      END; (* WITH fat DO *)
   END RangeCheckForConstant;

   (*
    *	may load fat if not constant
    *
    *	assignment of a subrange to another subrange is tested;
    *	this checks against not initialized variables
    *)

   PROCEDURE RangeCheck(dest: Stptr; VAR fat: Attribut; check: BOOLEAN);
      TYPE
	 CheckType = (signed, unsigned);
      VAR source: Stptr;

      PROCEDURE Check(ct: CheckType; low, high: CARDINAL);
	 CONST
	    CHKZ = ".chkz";
	    CHK  = ".chk";
	    UCHK = ".uchk";
      BEGIN
	 Load(fat);
	 EmitRR(LR, r0, fat.loadReg);
         IF (low = 0) AND (ct = unsigned) THEN
	    EmitExtern(CHKZ);
	    EmitRX3Label(BAL, r1, r0, r0, CHKZ, noOpr, 0);
	    EmitDCFValue(INTEGER(high));
	 ELSE
	    IF ct = signed THEN
	       EmitExtern(CHK);
	       EmitRX3Label(BAL, r1, r0, r0, CHK, noOpr, 0);
	    ELSE
	       EmitExtern(UCHK);
	       EmitRX3Label(BAL, r1, r0, r0, UCHK, noOpr, 0);
	    END;
	    EmitDCFValue(INTEGER(low));
	    EmitDCFValue(INTEGER(high));
	 END;
      END Check;

      PROCEDURE CheckSign;
	 CONST CHKS = ".chks";
      BEGIN
	 Load(fat);
	 EmitRR(LR, r0, fat.loadReg);
	 EmitExtern(CHKS);
	 EmitRX3Label(BAL, r1, r0, r0, CHKS, noOpr, 0);
      END CheckSign;

   BEGIN
      WITH fat DO
	 IF mode = constantMod THEN
	    RangeCheckForConstant(dest, fat);
	 ELSIF check AND (SizeType(fat) <= oneword) THEN
	    source := typtr;
	    IF IsInt(TestBaseType(dest)) THEN
	       (* includes intcar subranges of INTEGER *)
	       IF IsInt(dest) THEN
		  IF IsCard(source) OR
		     (source^.form = subranges) AND
		     IsCard(BaseType(source)) THEN
		     CheckSign;
		  END;
	       ELSE
		  (*
		   *	dest <> intptr --> dest^.form = subranges, min < 0
		   *
		   *	this test may be too hard, but detects not
		   *	initialized vars
		   *)

		  (* check for range [dest^.min .. dest^.max] *)
		  Check(signed, dest^.min, dest^.max);
		  IF NOT IsInt(TestBaseType(source)) AND
		     (INTEGER(dest^.min) < 0) THEN
		     CheckSign;
		  END;
	       END;
	    ELSIF IsCard(BaseType(dest)) THEN
	       (*
		*	includes intcar subranges of CARDINAL but not
		*	intcar itself
		*)
	       
	       IF dest = addrptr THEN RETURN
               ELSIF dest^.form <> subranges THEN (* intptr, longintptr *)
                  IF IsInt(BaseType(source)) THEN
		     CheckSign;
		  END;
	       ELSE
		  (*
		   *	dest <> cardptr --> dest^.form = subranges
		   *
		   *	guarantee dest^.form = subranges; otherwise is
		   *	max meaningless
		   *)

		  (* check unsigned range [dest^.min .. dest^.max] *)
		  Check(unsigned, dest^.min, dest^.max);
		  (* intcar is only used for subranges *)
		  IF NOT IsCard(TestBaseType(source)) AND
		     (dest^.max > maxint) THEN
		     CheckSign;
		  END;
	       END;
	    ELSIF dest^.form = subranges THEN
	       (* but not of card, int, intcar *)
	       (* check range [dest^.min .. dest^.max] *)
	       Check(signed, dest^.min, dest^.max);
	    ELSIF (dest = charptr) AND (BaseType(source) <> charptr) THEN
	       (* check range [0 .. 377B] *)
	       Check(unsigned, 0, 377B);
	    ELSIF (dest^.form = enums) AND (BaseType(source) <> dest) THEN
	       (* check range [0 .. dest^.cstnr] *)
	       Check(unsigned, 0, dest^.cstnr);
	    (* ELSE dest = intcar or no range test possible *)
	    END; (* IF TestBaseType(dest) = intptr *)
	 END; (* IF mode = constantMod *)
      END; (* WITH fat DO *)
   END RangeCheck;

   (*
    *	check for top < limit
    *)

   PROCEDURE CheckStack;
      CONST StackError = ".stack";
   BEGIN
      IF NOT sflag THEN
	 EmitRR(CR, top, limit);
	 EmitExtern(StackError);
	 EmitBranch(BNM, r0, r0, StackError);
      END;
   END CheckStack;

   (*
    *	stringConstMod for character constants possible
    *)

   PROCEDURE CharCast(VAR at: Attribut);
      TYPE CharPtr = POINTER TO ARRAY[0..0] OF CHAR;
      VAR cp: CharPtr;
   BEGIN
      WITH at DO
         Assert(typtr^.ixp^.max = 0);
         cp := CharPtr(strgPtr^.valentry);
         mode := constantMod;
         value := ORD(cp^[0]);
         typtr := charptr;
      END;
   END CharCast;

END MCP4RangeChecks.
