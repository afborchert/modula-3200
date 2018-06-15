IMPLEMENTATION MODULE MCP4Register; (* AFB 8/83 *)

   FROM MCP4Global IMPORT Assert, Error;
   FROM MCP4CodeSys IMPORT EmitRR, EmitRX3, EmitSF, EmitFloatRX3, EmitRI,
      EmitRX3Int, EmitFloatRX3Int;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCBase IMPORT xelos, adc, BitsPerWord, doubleword;
   FROM MCP4Stack IMPORT GetStack, FreeStack, (* for SaveRegs/RestoreRegs *)
      IncTop, DecTop;

   (*
    *	principles of operation:
    *
    *	temporary use		r0, r1
    *	protected registers	base, top, limit (= UNIX stack pointer)
    *	free registers		r3, r4, r5, r6, r8, r9, ra, rb, rc, rd, rf
    *
    *	rf is used as link register, but saved by GenBlockEntry.
    *
    *	Normally register will be allocated with GetReg and
    *	freed with FreeReg. This registers are allocated at
    *	their specific level, i.e. they are lost after RestoreRegs
    *	and must not be used after SaveRegs.
    *	Exception: one register is allocated for all levels greater
    *	or equal to the actual level for each "with"-statement;
    *	this register should be declared with WithReg and freed
    *	with FreeWithReg.
    *
    *   changes for XELOS:
    *
    *   protected registers    base (= re), top (= rd), limit (= r2)
    *   free registers         r3, r4, r5, r6, r7, r8, r9, ra, rb, rc, rf
    *)

   CONST
      maxlevel = BitsPerWord;
      FreeRegisters = 11;
   TYPE
      RegType = (free, withreg, prot, locked);
      RegUse =
	 RECORD
            type: RegType;
	 END;
      FloatRegUse = (used, unused);
      Registers = [r0..rf];
      FloatRegisters = [fr0..fr7];
      LevelRange = [0..maxlevel-1];

   VAR
      level: LevelRange; (* incremented/decremented by SaveRegs/RestoreRegs *)
      leveloverflow: CARDINAL;      (* simulate level >= maxlevel *)
      RegTab : ARRAY LevelRange, Registers OF RegUse; (* general registers *)
      FloatRegTab : ARRAY LevelRange, FloatRegisters OF FloatRegUse;
         (* double precision floating point registers *)
      Priority : ARRAY [0..FreeRegisters-1] OF Registers;
      Index : CARDINAL;
      RegIndex : Registers;
      FloatRegIndex : FloatRegisters;

   PROCEDURE Lookup(VAR r: Reg; rl: CARDINAL; VAR found: BOOLEAN);
      VAR i, reg : Reg;
	  indx : CARDINAL;
   BEGIN
      indx := 0;
      found := TRUE;
      WHILE indx < FreeRegisters DO
	 reg := Priority[indx];
	 i := reg;
         (* special case: register pair must be located on a doubleword boundary *)
         IF (rl = 2) AND ODD(CARDINAL(reg)) THEN
            (* continue *)
         ELSE
	    WHILE (i <= rf) AND (RegTab[level, i].type = free)
	          AND (CARDINAL(i) - CARDINAL(reg) < rl) DO
	       INC(i);
	    END;
	    IF CARDINAL(i) - CARDINAL(reg) = rl THEN
	       r := reg;
	       RETURN
	    END;
         END;
	 INC(indx);
      END;
      found := FALSE;
   END Lookup;

   PROCEDURE GetFloatReg(VAR r: FloatReg);
   BEGIN
      FOR r := fr7 TO fr1 BY -1 DO
	 IF FloatRegTab[level, r] = unused THEN
	    FloatRegTab[level, r] := used;
	    RETURN
	 END
      END;
      Error(204);
      r := fr0;
   END GetFloatReg;

   PROCEDURE GetReg(VAR r: Reg);
      VAR ok : BOOLEAN;
   BEGIN
      Lookup(r, 1, ok);
      IF NOT ok THEN
	 Error(204);
	 r := r1;
      ELSE
	 LockReg(r,1);
      END;
   END GetReg;

   PROCEDURE RegRequest(wanted: Reg; VAR done: BOOLEAN);
   BEGIN
      WITH RegTab[level, wanted] DO
         IF type = free THEN
            LockReg(wanted, 1);
	    done := TRUE;
	 ELSE
	    done := FALSE;
	 END
      END
   END RegRequest;

   PROCEDURE GetRegPair(VAR r: Reg; VAR done: BOOLEAN);
   BEGIN
      Lookup(r, 2, done);
      IF done THEN
	 LockReg(r, 2);
      END;
   END GetRegPair;

   PROCEDURE GetRegTriplet(VAR r: Reg; VAR done: BOOLEAN);
   BEGIN
      Lookup(r, 3, done);
      IF done THEN
	 LockReg(r, 3);
      END
   END GetRegTriplet;

   PROCEDURE FreeReg(r: Reg);
   BEGIN
      UnlockReg(r, 1);
   END FreeReg;

   PROCEDURE FreeFloatReg(r: FloatReg);
   BEGIN
      FloatRegTab[level, r] := unused;
   END FreeFloatReg;

   PROCEDURE FreeRegPair(r: Reg);
   BEGIN
      UnlockReg(r, 2);
   END FreeRegPair;

   PROCEDURE FreeRegTriplet(r: Reg);
   BEGIN
      UnlockReg(r, 3);
   END FreeRegTriplet;

   PROCEDURE LockReg(r: Reg; rl: CARDINAL);
      VAR reg: Reg;
   BEGIN
      FOR reg := r TO Reg(CARDINAL(r)+rl-1) DO
	 WITH RegTab[level, reg] DO
            Assert(type = free);
            type := locked;
	 END
      END
   END LockReg;

   PROCEDURE UnlockReg(r: Reg; rl: CARDINAL);
      VAR reg: Reg;
   BEGIN
      IF r = r1 THEN (* occurs on register overflow *) RETURN END;
      FOR reg := r TO Reg(CARDINAL(r)+rl-1) DO
	 WITH RegTab[level, reg] DO
            Assert(type = locked);
            type := free;
	 END
      END
   END UnlockReg;

   PROCEDURE WithReg(r: Reg);
      VAR l: LevelRange;
   BEGIN
      FOR l := level TO maxlevel-1 DO
         RegTab[l, r].type := withreg;
      END;
   END WithReg;

   PROCEDURE FreeWithReg(r: Reg);
      VAR l: LevelRange;
   BEGIN
      FOR l := level TO maxlevel-1 DO
         RegTab[l, r].type := free;
      END;
   END FreeWithReg;

   PROCEDURE IsAllocated(VAR type: RegType) : BOOLEAN;
   BEGIN
      RETURN (type = withreg) OR (type = locked);
   END IsAllocated;

   VAR
      RegAlloc, FloatRegAlloc: ARRAY LevelRange OF
         RECORD
            offset: CARDINAL; (* offset to base *)
         END;

   PROCEDURE SaveRegs;
      VAR
         space: CARDINAL; (* in words *)
   BEGIN
      (* save general registers *)
      RegIndex := r3;
      WHILE (RegIndex < rf) AND NOT IsAllocated(RegTab[level, RegIndex].type) DO
         INC(RegIndex);
      END;
      (* assume: rf isn't protected *)
      IF (RegIndex < rf) OR (RegTab[level, RegIndex].type <> free) THEN
         space := ORD(rf) - ORD(RegIndex) + 1;
         IF leveloverflow > 0 THEN
            IncTop(space * adc);
            EmitRI(AHI, top, r0, space * adc);
            EmitRX3Int(STM, RegIndex, top, r0, -INTEGER(space * adc));
         ELSE
            WITH RegAlloc[level] DO
               GetStack(space, offset);
               EmitRX3(STM, RegIndex, base, r0, offset);
            END;
         END;
      END;
      (* save floating point registers *)
      FloatRegIndex := fr1;
      WHILE (FloatRegIndex < fr7) AND (FloatRegTab[level, FloatRegIndex] = unused) DO
         FloatRegIndex := FloatReg(CARDINAL(FloatRegIndex)+1)
      END;
      IF (FloatRegIndex < fr7) OR (FloatRegTab[level, FloatRegIndex] = used) THEN
         space := (ORD(fr7) - ORD(FloatRegIndex) + 1) * doubleword DIV adc;
         IF leveloverflow > 0 THEN
            IncTop(space * adc);
            EmitRI(AHI, top, r0, space * adc);
            EmitFloatRX3Int(STMD, FloatRegIndex, top, r0, -INTEGER(space * adc));
         ELSE
            WITH FloatRegAlloc[level] DO
               GetStack(space, offset);
               EmitFloatRX3(STMD, FloatRegIndex, base, r0, offset);
            END;
         END;
      END;
      IF level = maxlevel-1 THEN
         INC(leveloverflow);
      ELSE
         INC(level);
      END;
   END SaveRegs;

   PROCEDURE RestoreRegs;
      VAR
         space: CARDINAL; (* in words *)
   BEGIN
      (* inversion of SaveRegs *)
      IF leveloverflow > 0 THEN
         DEC(leveloverflow);
      ELSE
         DEC(level);
      END;
      (* load floating point registers *)
      FloatRegIndex := fr1;
      WHILE (FloatRegIndex < fr7) AND (FloatRegTab[level, FloatRegIndex] = unused) DO
         FloatRegIndex := FloatReg(CARDINAL(FloatRegIndex)+1)
      END;
      IF (FloatRegIndex < fr7) OR (FloatRegTab[level, FloatRegIndex] = used) THEN
         space := (ORD(fr7) - ORD(FloatRegIndex) + 1) * doubleword DIV adc;
         IF leveloverflow > 0 THEN
            DecTop(space * adc);
            EmitFloatRX3Int(LMD, FloatRegIndex, top, r0, -INTEGER(space * adc));
            EmitRI(SHI, top, r0, space * adc);
         ELSE
            WITH FloatRegAlloc[level] DO
               EmitFloatRX3(LMD, FloatRegIndex, base, r0, offset);
               FreeStack(space, offset);
            END;
         END;
      END;
      (* load general registers *)
      RegIndex := r3;
      WHILE (RegIndex < rf) AND NOT IsAllocated(RegTab[level, RegIndex].type) DO
         RegIndex := Reg(CARDINAL(RegIndex)+1)
      END;
      (* assume: rf isn't protected *)
      IF (RegIndex < rf) OR (RegTab[level, RegIndex].type <> free) THEN
         space := ORD(rf) - ORD(RegIndex) + 1;
         IF leveloverflow > 0 THEN
            DecTop(space * adc);
            EmitRX3Int(LM, RegIndex, top, r0, -INTEGER(space * adc));
            EmitRI(SHI, top, r0, space * adc);
         ELSE
            WITH RegAlloc[level] DO
               EmitRX3(LM, RegIndex, base, r0, offset);
               FreeStack(space, offset);
            END;
         END;
      END;
   END RestoreRegs;

   PROCEDURE ProtectReg(r: Reg);
   BEGIN
      RegTab[level, r].type := prot;
   END ProtectReg;

BEGIN
   FOR level := 0 TO maxlevel-1 DO
      FOR RegIndex := r0 TO rf DO
         RegTab[level, RegIndex].type := free;
      END;
      FOR FloatRegIndex := fr0 TO fr7 DO
         FloatRegTab[level, FloatRegIndex] := unused;
      END;
   END;
   leveloverflow := 0;
   IF xelos THEN
      top := rd;
      limit := r2;
      Priority[0] := rf;
      Priority[1] := rc;
      Priority[2] := rb;
      Priority[3] := ra;
      Priority[4] := r9;
      Priority[5] := r8;
      Priority[6] := r7;
   ELSE
      top := r2;
      limit := r7;
      Priority[0] := rf;
      Priority[1] := rd;
      Priority[2] := rc;
      Priority[3] := rb;
      Priority[4] := ra;
      Priority[5] := r9;
      Priority[6] := r8;
   END;
   Priority[7] := r6;
   Priority[8] := r5;
   Priority[9] := r4;
   Priority[10] := r3;
   FOR level := 0 TO maxlevel-1 DO
      ProtectReg(top);
      ProtectReg(limit);
      ProtectReg(base);
   END;
   level := 0;
END MCP4Register.
