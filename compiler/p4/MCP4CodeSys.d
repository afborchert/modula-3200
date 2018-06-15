DEFINITION MODULE MCP4CodeSys; (* AFB 8/83 *)

   FROM MCP4Register IMPORT Reg, FloatReg;
   FROM MCP4Labels IMPORT LabelPtr;
   FROM MCMnemonics IMPORT Mnemonic;

   CONST
      noOpr = ' ';

   VAR
      star : ARRAY [0..1] OF CHAR;

   PROCEDURE EmitLabel(l: LabelPtr);

   PROCEDURE EmitRR(OP: Mnemonic; R1, R2: Reg);

   PROCEDURE EmitFloatRR(OP: Mnemonic; R1, R2: FloatReg);

   PROCEDURE EmitSF(OP: Mnemonic; R1: Reg; N: CARDINAL);

   PROCEDURE EmitRX3(OP: Mnemonic; R1, FX2, SX2: Reg; A2: CARDINAL);

   PROCEDURE EmitRX3Int(OP: Mnemonic; R1, FX2, SX2: Reg; A2: INTEGER);

   PROCEDURE EmitFloatRX3(OP: Mnemonic; R1: FloatReg; FX2, SX2: Reg;
			  A2: CARDINAL);

   PROCEDURE EmitFloatRX3Int(OP: Mnemonic; R1: FloatReg;
			     FX2, SX2: Reg; A2: INTEGER);

   PROCEDURE EmitRX3Label(OP: Mnemonic; R1, FX2, SX2: Reg;
                          A2L: ARRAY OF CHAR; Opr: CHAR; A2C: CARDINAL);
   
   PROCEDURE EmitFloatRX3Label(OP: Mnemonic; R1: FloatReg; FX2, SX2: Reg;
                               A2L: ARRAY OF CHAR; Opr: CHAR; A2C: CARDINAL);

   PROCEDURE EmitRXRX(OP: Mnemonic; R1, FX2, SX2, R3, FX4, FX5: Reg);

   PROCEDURE EmitRXRXLabel(OP: Mnemonic; R1, FX2, SX2: Reg;
                           A2L: ARRAY OF CHAR; Opr2: CHAR; A2C: CARDINAL;
                           R3, FX4, SX4: Reg;
                           A4L: ARRAY OF CHAR; Opr4: CHAR; A4C: CARDINAL);

   PROCEDURE EmitBranch(OP: Mnemonic; FX2, SX2: Reg;
			A2: ARRAY OF CHAR);

   PROCEDURE EmitRI(OP: Mnemonic; R1, X2: Reg; I2: CARDINAL);

   PROCEDURE EmitRILabel(OP: Mnemonic; R1, X2: Reg;
                         AL: ARRAY OF CHAR; Opr: CHAR; I2: CARDINAL);

   PROCEDURE EmitEQU(str1,str2: ARRAY OF CHAR);

   PROCEDURE EmitEQUCard(str : ARRAY OF CHAR; c: CARDINAL);

   PROCEDURE EmitEQUInt(str: ARRAY OF CHAR; i: INTEGER);

   PROCEDURE EmitEND;

   PROCEDURE EmitComment(str: ARRAY OF CHAR);

   PROCEDURE EmitDCF(const: ARRAY OF CHAR);

   PROCEDURE EmitDC(const: ARRAY OF CHAR);

   PROCEDURE EmitDS(size: CARDINAL);

   PROCEDURE EmitDCFValue(value: INTEGER);

   PROCEDURE EmitBSS(l: LabelPtr; af: CARDINAL);

   PROCEDURE EmitPure;

   PROCEDURE EmitImpure;

   PROCEDURE EmitAlign;

   PROCEDURE EmitEntry(name: ARRAY OF CHAR);

   PROCEDURE EmitExtern(name: ARRAY OF CHAR);

   PROCEDURE EmitString(l: LabelPtr; str: CARDINAL); (* see Stringval ! *)

   PROCEDURE AppendComment(str: ARRAY OF CHAR);

   PROCEDURE AppendValue(str1: ARRAY OF CHAR; val: CARDINAL; str2: ARRAY OF CHAR);

   (* for XELOS only: *)

   PROCEDURE EmitFileName(filename: ARRAY OF CHAR);

   PROCEDURE EmitExportedVarLabel(offset: CARDINAL);

   PROCEDURE EmitExportedProcLabel(num: CARDINAL);

   PROCEDURE EmitProcExtern(modname: ARRAY OF CHAR; procnum: CARDINAL);

   PROCEDURE EmitLn(line: CARDINAL);

   (* EmitDef { EmitDefOptCard | EmitDefOptStr } EmitEndDef *)

   PROCEDURE EmitDef(symbol: ARRAY OF CHAR);

   PROCEDURE EmitDefOptCard(opt: ARRAY OF CHAR; val: CARDINAL);

   PROCEDURE EmitDefOptStr(opt, str: ARRAY OF CHAR);

   PROCEDURE EmitEndDef;

   PROCEDURE EmitVarExtern(modname: ARRAY OF CHAR; offset: CARDINAL);

   PROCEDURE EmitDefPV(opt: ARRAY OF CHAR; modname: ARRAY OF CHAR;
                       PV: CHAR; num: CARDINAL);

END MCP4CodeSys.
