DEFINITION MODULE MCP4Load; (* AFB 9/83 *)

   FROM MCP4AttributSys IMPORT Attribut;
   FROM MCP4Register IMPORT Reg, FloatReg;

   PROCEDURE Load(VAR fat: Attribut);

   PROCEDURE LoadReg(VAR fat: Attribut; r: Reg); (* load into Reg. r *)

   PROCEDURE LoadCond(VAR fat: Attribut); (* load condition codes *)

   PROCEDURE LoadFloatReg(VAR fat: Attribut; fr: FloatReg);

   PROCEDURE LoadAddr(VAR fat: Attribut);

   PROCEDURE LoadAddrReg(VAR fat: Attribut; r: Reg);

   PROCEDURE LoadDynHigh(VAR fat: Attribut);

   PROCEDURE LoadConstant(r: Reg; value: INTEGER);

   PROCEDURE LoadBigSet(VAR fat: Attribut);

END MCP4Load.
