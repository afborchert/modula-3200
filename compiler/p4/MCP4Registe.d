DEFINITION MODULE MCP4Register; (* AFB 8/83 *)

   (*
   EXPORT QUALIFIED
      Reg, FloatReg, GetReg, FreeReg, GetFloatReg, FreeFloatReg,
      GetRegPair, FreeRegPair, GetRegTriplet, FreeRegTriplet, SaveRegs,
      RestoreRegs, RegRequest, WithReg, FreeWithReg;
   *)

   TYPE
      Reg = (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, ra, rb,
             rc, rd, base, rf);
      FloatReg = (fr0, fr1, fr2, fr3, fr4, fr5, fr6, fr7);

   VAR
      top, limit: Reg;

   PROCEDURE GetReg(VAR r: Reg);

   PROCEDURE RegRequest(wanted: Reg; VAR done: BOOLEAN);

   PROCEDURE FreeReg(r: Reg);

   PROCEDURE GetFloatReg(VAR r: FloatReg);

   PROCEDURE FreeFloatReg(f: FloatReg);

   PROCEDURE GetRegPair(VAR r: Reg; VAR done: BOOLEAN);

   PROCEDURE FreeRegPair(r: Reg);

   PROCEDURE GetRegTriplet(VAR r: Reg; VAR done: BOOLEAN);

   PROCEDURE FreeRegTriplet(r: Reg);

   PROCEDURE SaveRegs;

   PROCEDURE RestoreRegs;

   PROCEDURE WithReg(r: Reg);

   PROCEDURE FreeWithReg(r: Reg);

END MCP4Register.
