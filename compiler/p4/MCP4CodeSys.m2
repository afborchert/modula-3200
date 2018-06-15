IMPLEMENTATION MODULE MCP4CodeSys; (* AFB 8/83 *)
   (* formatted output in CAL-32 *)

   FROM MCP4Out IMPORT Write, WriteLn, WriteString, TermOut;
   FROM MCP4Labels IMPORT Label, LabelPtr;
   FROM MCP4Register IMPORT FloatReg, Reg;
   FROM MCP4Global IMPORT Assert;
   FROM Conversions IMPORT ConvertInteger, ConvertCardinal;
   FROM MCMnemonics IMPORT Mnemonic, Mnem;
   FROM MCP4Public IMPORT Sflag;
   FROM MCBase IMPORT stringmax, mainmodp, xelos;

   CONST
      tab = 11C;
      BufSize = 40;

   TYPE
      Segment = (pure, impur, bss);

   VAR
      i: CARDINAL;
      RegStr: ARRAY [ 0..15 ] OF ARRAY [ 0..4 ] OF CHAR;
      FloatRegStr: ARRAY [ 0..7 ] OF ARRAY [ 0..2 ] OF CHAR;
      ActSeg: Segment;
      ComBuf: ARRAY [ 0..BufSize-1 ] OF CHAR;
      AppCom: BOOLEAN;
      emptyString: ARRAY [ 0..0 ] OF CHAR; (* = "\0" *)

   PROCEDURE EOL;
   BEGIN
      IF Sflag AND AppCom THEN
	 Write(tab);
	 WriteString(ComBuf);
	 AppCom := FALSE;
      END;
      WriteLn;
   END EOL;

   PROCEDURE EmitLabel(l: LabelPtr);
   BEGIN
      EmitEQU(l^,star);
   END EmitLabel;

   PROCEDURE EmitRR(OP: Mnemonic; R1, R2: Reg);
   BEGIN
      Write(tab); WriteMnem(OP); Write(tab); WriteReg(R1);
      IF OP <> BR THEN
         Write(',');
         WriteReg(R2);
      END;
      EOL;
   END EmitRR;

   PROCEDURE EmitFloatRR(OP: Mnemonic; R1, R2: FloatReg);
   BEGIN
      Write(tab); WriteMnem(OP); Write(tab); WriteFloatReg(R1); Write(',');
      WriteFloatReg(R2); EOL;
   END EmitFloatRR;

   PROCEDURE EmitSF(OP: Mnemonic; R1: Reg; N: CARDINAL);
      VAR field : ARRAY [ 0..1 ] OF CHAR;
   BEGIN
      Assert( N < 16 );
      Write(tab); WriteMnem(OP); Write(tab); WriteReg(R1); Write(',');
      ConvertCardinal(N,1,field); WriteString(field);
      EOL;
   END EmitSF;

   PROCEDURE EmitRX3(OP: Mnemonic; R1, FX2, SX2: Reg; A2: CARDINAL);
   BEGIN
      Write(tab); WriteMnem(OP); Write(tab); WriteReg(R1); Write(',');
      EmitX2(FX2, SX2, emptyString, noOpr, A2);
      EOL;
   END EmitRX3;

   PROCEDURE EmitFloatRX3(OP: Mnemonic;
			  R1: FloatReg; FX2, SX2: Reg; A2: CARDINAL);
   BEGIN
      Write(tab); WriteMnem(OP); Write(tab); WriteFloatReg(R1); Write(',');
      EmitX2(FX2, SX2, emptyString, noOpr, A2);
      EOL;
   END EmitFloatRX3;

   PROCEDURE EmitRX3Int(OP: Mnemonic; R1, FX2, SX2: Reg;
			A2: INTEGER);
      VAR field: ARRAY [0..9] OF CHAR;
   BEGIN
      ConvertInteger(A2, 1, field);
      EmitRX3Label(OP, R1, FX2, SX2, field, noOpr, 0);
   END EmitRX3Int;

   PROCEDURE EmitRX3Label(OP: Mnemonic; R1, FX2, SX2: Reg;
                          A2L: ARRAY OF CHAR; Opr: CHAR; A2C: CARDINAL);
   BEGIN
      Write(tab); WriteMnem(OP); Write(tab); WriteReg(R1); Write(',');
      EmitX2(FX2, SX2, A2L, Opr, A2C);
      EOL
   END EmitRX3Label;

   PROCEDURE EmitFloatRX3Int(OP: Mnemonic; R1: FloatReg; FX2, SX2: Reg;
			A2: INTEGER);
      VAR field: ARRAY [0..9] OF CHAR;
   BEGIN
      ConvertInteger(A2, 1, field);
      EmitFloatRX3Label(OP, R1, FX2, SX2, field, noOpr, 0);
   END EmitFloatRX3Int;

   PROCEDURE EmitFloatRX3Label(OP: Mnemonic; R1: FloatReg; FX2, SX2: Reg;
                               A2L: ARRAY OF CHAR; Opr: CHAR; A2C: CARDINAL);
   BEGIN
      Write(tab); WriteMnem(OP); Write(tab); WriteFloatReg(R1); Write(',');
      EmitX2(FX2, SX2, A2L, Opr, A2C);
      EOL
   END EmitFloatRX3Label;

   PROCEDURE EmitRXRX(OP: Mnemonic; R1, FX2, SX2, R3, FX4, SX4: Reg);
   BEGIN
      Write(tab); WriteMnem(OP); Write(tab); WriteReg(R1); Write(',');
      EmitX2(FX2, SX2, emptyString, noOpr, 0);
      Write(','); WriteReg(R3); Write(',');
      EmitX2(FX4, SX4, emptyString, noOpr, 0);
      EOL
   END EmitRXRX;

   PROCEDURE EmitRXRXLabel(OP: Mnemonic; R1, FX2, SX2: Reg;
                           A2L: ARRAY OF CHAR; Opr2: CHAR; A2C: CARDINAL;
                           R3, FX4, SX4: Reg;
                           A4L: ARRAY OF CHAR; Opr4: CHAR; A4C: CARDINAL);
   BEGIN
      Write(tab); WriteMnem(OP); Write(tab); WriteReg(R1); Write(',');
      EmitX2(FX2, SX2, A2L, Opr2, A2C);
      Write(','); WriteReg(R3); Write(',');
      EmitX2(FX4, SX4, A4L, Opr4, A4C);
      EOL
   END EmitRXRXLabel;

   PROCEDURE EmitX2(FX2, SX2: Reg; A2L: ARRAY OF CHAR; Opr: CHAR;
                    A2C: CARDINAL);
      VAR
	 field: ARRAY [0..9] OF CHAR;
	 PV: BOOLEAN;
   BEGIN
      IF A2L[0] <> 0C THEN
         WriteString(A2L);
      END;
      PV := (Opr = 'P') OR (Opr = 'V');
      IF ((A2C <> 0) OR PV) AND (Opr <> noOpr) THEN
	 IF PV THEN
	    Assert(xelos);
	    Write('_');
	 END;
         Write(Opr);
	 IF Opr = 'V' THEN
	    WriteString("0+");
	 END;
      END;
      IF (A2C <> 0) OR (A2L[0] = 0C) OR PV THEN
         ConvertCardinal(A2C, 1, field);
         WriteString(field);
      END;
      IF (FX2 <> r0) OR (SX2 <> r0) THEN
	 Write('(');
	 IF (FX2 <> r0) THEN
	    WriteReg(FX2);
	    IF (SX2 <> r0) THEN
	       Write(',');
	    END;
	 END;
	 IF (SX2 <> r0) THEN
	    WriteReg(SX2);
	 END;
	 Write(')');
      END;
   END EmitX2;

   PROCEDURE EmitBranch(OP: Mnemonic; FX2, SX2: Reg;
			A2: ARRAY OF CHAR);
   BEGIN
      Write(tab); WriteMnem(OP); Write(tab); WriteString(A2);
      IF (FX2 <> r0) OR (SX2 <> r0) THEN
	 Write('(');
	 IF (FX2 <> r0) THEN
	    WriteReg(FX2);
	    IF (SX2 <> r0) THEN
	       Write(',');
	    END;
	 END;
	 IF (SX2 <> r0) THEN
	    WriteReg(SX2);
	 END;
	 Write(')');
      END;
      EOL
   END EmitBranch;

   PROCEDURE EmitRI(OP: Mnemonic; R1, X2: Reg; I2: CARDINAL );
      VAR field: ARRAY [0..10] OF CHAR;
   BEGIN
      Write(tab); WriteMnem(OP); Write(tab); WriteReg(R1);
      Write(',');
      ConvertCardinal(I2, 1, field);
      WriteString(field);
      IF X2 <> r0 THEN
         Write('(');
         WriteReg(X2);
         Write(')');
      END;
      EOL
   END EmitRI;

   PROCEDURE EmitRILabel(OP: Mnemonic; R1, X2: Reg;
                         AL: ARRAY OF CHAR; Opr: CHAR; I2: CARDINAL);
   BEGIN
      Write(tab); WriteMnem(OP); Write(tab); WriteReg(R1); Write(',');
      EmitX2(X2, r0, AL, Opr, I2);
      EOL
   END EmitRILabel;

   PROCEDURE EmitEQU(str1,str2: ARRAY OF CHAR);
   BEGIN
      WriteString(str1); Write(tab); WriteString("equ"); Write(tab);
      WriteString(str2); EOL
   END EmitEQU;

   PROCEDURE EmitEQUCard(str: ARRAY OF CHAR; c: CARDINAL);
      VAR field : ARRAY [ 0..9 ] OF CHAR;
   BEGIN
      ConvertCardinal(c,1,field);
      EmitEQU(str,field)
   END EmitEQUCard;

   PROCEDURE EmitEQUInt(str: ARRAY OF CHAR; i: INTEGER);
      VAR field : ARRAY [ 0..10 ] OF CHAR;
   BEGIN
      ConvertInteger(i, 1, field);
      EmitEQU(str, field)
   END EmitEQUInt;

   PROCEDURE EmitEntry(name: ARRAY OF CHAR);
   BEGIN
      Write(tab); WriteString("entry"); Write(tab); WriteString(name);
      EOL;
   END EmitEntry;

   PROCEDURE EmitExtern(name: ARRAY OF CHAR);
   BEGIN
      Write(tab); WriteString("extrn"); Write(tab); WriteString(name);
      WriteLn
   END EmitExtern;

   PROCEDURE EmitEND;
   BEGIN
      Write(tab); WriteString("end"); WriteLn;
      TermOut;
   END EmitEND;

   PROCEDURE EmitComment(str: ARRAY OF CHAR);
   BEGIN
      IF NOT Sflag THEN RETURN END;
      Write('*'); Write(tab); WriteString(str); WriteLn;
   END EmitComment;

   PROCEDURE AppendComment(str: ARRAY OF CHAR);
      VAR indx : CARDINAL;
   BEGIN
      IF NOT Sflag THEN RETURN END;
      AppCom := TRUE;
      indx := 0;
      WHILE (indx < BufSize) AND (indx <= HIGH(str)) AND (str[indx] <> 0C) DO
	 ComBuf[indx] := str[indx];
	 INC(indx);
      END;
      IF (indx < BufSize) THEN
	 ComBuf[indx] := 0C;
      END;
   END AppendComment;

   PROCEDURE AppendValue(str1: ARRAY OF CHAR; val: CARDINAL;
			 str2: ARRAY OF CHAR);
      VAR field: ARRAY[1..9] OF CHAR;
	  indx, indx2 : CARDINAL;
   BEGIN
      IF NOT Sflag THEN RETURN END;
      AppCom := TRUE;
      indx := 0;
      WHILE (indx < BufSize) AND (indx <= HIGH(str1)) AND (str1[indx] <> 0C) DO
	 ComBuf[indx] := str1[indx];
	 INC(indx);
      END;
      ConvertCardinal(val,1,field); 
      indx2 := 1;
      WHILE (indx < BufSize) AND (indx2 <= 8) AND (field[indx2] <> 0C) DO
	 ComBuf[indx] := field[indx2];
	 INC(indx);
	 INC(indx2);
      END;
      indx2 := 0;
      WHILE (indx < BufSize) AND (indx2 <= HIGH(str2)) AND (str2[indx2] <> 0C) DO
	 ComBuf[indx] := str2[indx2];
	 INC(indx);
	 INC(indx2);
      END;
      IF (indx < BufSize) THEN
	 ComBuf[indx] := 0C;
      END;
   END AppendValue;

   PROCEDURE EmitBSS(l: LabelPtr; af: CARDINAL); (* af = Anzahl Fullwords *)
      VAR field : ARRAY [ 0..5 ] OF CHAR;
   BEGIN
      IF ActSeg <> bss THEN
         EmitComment("  ");
         Write(tab); WriteString("bss"); EOL;
      END;
      WriteString(l^); Write(tab); WriteString("dsf"); Write(tab);
      ConvertCardinal(af,1,field); WriteString(field);
      EOL;
      ActSeg := bss;
   END EmitBSS;
   
   PROCEDURE EmitDCF(const: ARRAY OF CHAR);
   BEGIN
      Write(tab); WriteString("dcf"); Write(tab); WriteString(const);
      EOL
   END EmitDCF;

   PROCEDURE EmitDC(const: ARRAY OF CHAR);
   BEGIN
      Write(tab); WriteString("dc"); Write(tab); WriteString(const);
      EOL
   END EmitDC;

   PROCEDURE EmitDS(size: CARDINAL);
      VAR field : ARRAY [ 0..7 ] OF CHAR;
   BEGIN
      EmitImpure;
      ConvertCardinal(size,1,field);
      Write(tab); WriteString("ds"); Write(tab); WriteString(field);
      EOL
   END EmitDS;

   PROCEDURE EmitDCFValue(value: INTEGER);
      VAR field: ARRAY [0..10] OF CHAR;
   BEGIN
      Write(tab); WriteString("dcf"); Write(tab);
      ConvertInteger(value,1,field);
      WriteString(field);
      EOL
   END EmitDCFValue;

   PROCEDURE EmitString(l: LabelPtr; str: CARDINAL);
      TYPE
	  StringPtr = POINTER TO ARRAY [ 0..stringmax ] OF CHAR;

      VAR field : ARRAY [ 0..2 ] OF CHAR;
	  ptr : StringPtr;
   BEGIN
      EmitPure;
      EmitAlign;
      EmitLabel(l);
      ptr := StringPtr(str);
      i := 0;
      REPEAT
         ConvertCardinal(ORD(ptr^[i]),1,field);
         IF i MOD 10 = 0 THEN
            IF i > 0 THEN
               EOL
            END;
            Write(tab); WriteString("db"); Write(tab);
         ELSE
            Write(',');
         END;
         INC(i);
         WriteString(field);
      UNTIL ptr^[i-1] = 0C;
      EOL;
   END EmitString;

   PROCEDURE EmitPure;
   BEGIN
      IF ActSeg <> pure THEN
         EmitComment("  ");
         Write(tab); WriteString("pure"); EOL;
         ActSeg := pure;
      END
   END EmitPure;

   PROCEDURE EmitImpure;
   BEGIN
      IF ActSeg <> impur THEN
         EmitComment("  ");
         Write(tab); WriteString("impur"); EOL;
         ActSeg := impur;
      END
   END EmitImpure;

   PROCEDURE EmitAlign;
   BEGIN
      Write(tab); WriteString("align"); Write(tab); WriteString("adc");
      EOL
   END EmitAlign;

   PROCEDURE EmitFileName(filename: ARRAY OF CHAR);
      VAR
	 i: CARDINAL;
   BEGIN
      FOR i := 0 TO HIGH(filename) DO
	 IF filename[i] = "'" THEN
	    filename[i] := " ";
	 END;
      END;
      IF xelos THEN
	 Write(tab); WriteString(".file"); Write(tab);
	 WriteString("c'");
	 WriteString(filename);
	 Write("'");
	 EOL;
      END;
      (* identification for what (sccs) and ident (rcs) *)
      Write(tab);
      WriteString("db"); Write(tab);
      WriteString("c'$Source:@(#)");
      WriteString(filename);
      WriteString("$',0"); EOL;
   END EmitFileName;

   PROCEDURE EmitExportedVarLabel(offset: CARDINAL);
      VAR field: ARRAY[0..11] OF CHAR;
   BEGIN
      WriteString(mainmodp^.xidentifier); WriteString("_V");
      ConvertCardinal(offset, 1, field);
      WriteString(field);
      Write(tab); WriteString("equ"); Write(tab);
      WriteString("G_AREA+"); WriteString(field); EOL;
      Write(tab); WriteString("entry"); Write(tab);
      WriteString(mainmodp^.xidentifier); WriteString("_V");
      WriteString(field); EOL;
   END EmitExportedVarLabel;

   PROCEDURE EmitExportedProcLabel(num: CARDINAL);
      VAR field: ARRAY[0..11] OF CHAR;
   BEGIN
      WriteString(mainmodp^.xidentifier); WriteString("_P");
      ConvertCardinal(num, 1, field);
      WriteString(field);
      Write(tab); WriteString("equ"); Write(tab); Write("*"); EOL;
      Write(tab); WriteString("entry"); Write(tab);
      WriteString(mainmodp^.xidentifier); WriteString("_P");
      WriteString(field); EOL;
   END EmitExportedProcLabel;

   PROCEDURE EmitProcExtern(modname: ARRAY OF CHAR; procnum: CARDINAL);
      VAR field: ARRAY[0..11] OF CHAR;
   BEGIN
      Write(tab); WriteString("extrn"); Write(tab); WriteString(modname);
      WriteString("_P"); ConvertCardinal(procnum, 1, field);
      WriteString(field); EOL;
   END EmitProcExtern;

   PROCEDURE EmitVarExtern(modname: ARRAY OF CHAR; offset: CARDINAL);
      (* VAR field: ARRAY[0..11] OF CHAR; *)
   BEGIN
      Write(tab); WriteString("extrn"); Write(tab); WriteString(modname);
      (*
      WriteString("_V");
      ConvertCardinal(offset, 1, field);
      WriteString(field); EOL;
      *)
      WriteString("_V0");
      EOL;
   END EmitVarExtern;

   PROCEDURE EmitLn(line: CARDINAL);
      VAR field: ARRAY[0..11] OF CHAR;
   BEGIN
      Write(tab); WriteString(".ln"); Write(tab);
      ConvertCardinal(line, 1, field);
      WriteString(field); EOL;
   END EmitLn;

   PROCEDURE EmitDef(symbol: ARRAY OF CHAR);
   BEGIN
      Write(tab); WriteString(".def"); Write(tab); WriteString(symbol);
   END EmitDef;

   PROCEDURE EmitDefPV(opt: ARRAY OF CHAR;
		       modname: ARRAY OF CHAR; PV: CHAR; num: CARDINAL);
      VAR field: ARRAY[0..11] OF CHAR;
   BEGIN
      IF NOT ((opt[1] = 'd') AND (opt[2] = 'e') AND (opt[3] = 'f')) THEN
	 Write(';');
      END;
      Write(tab); WriteString(opt); Write(tab); WriteString(modname);
      WriteString("_"); Write(PV);
      ConvertCardinal(num, 1, field);
      WriteString(field);
   END EmitDefPV;

   PROCEDURE EmitDefOptCard(opt: ARRAY OF CHAR; val: CARDINAL);
      VAR field: ARRAY[0..11] OF CHAR;
   BEGIN
      Write(';'); Write(tab); WriteString(opt); Write(tab);
      ConvertCardinal(val, 1, field);
      WriteString(field);
   END EmitDefOptCard;

   PROCEDURE EmitDefOptStr(opt, str: ARRAY OF CHAR);
   BEGIN
      Write(';'); Write(tab); WriteString(opt); Write(tab);
      WriteString(str);
   END EmitDefOptStr;

   PROCEDURE EmitEndDef;
   BEGIN
      Write(';'); Write(tab); WriteString(".endef"); EOL;
   END EmitEndDef;

   PROCEDURE WriteMnem(OP: Mnemonic);
   BEGIN
      WriteString(Mnem[CARDINAL(OP)]);
   END WriteMnem;

   PROCEDURE WriteReg(r: Reg);
   BEGIN
      WriteString(RegStr[CARDINAL(r)]);
   END WriteReg;

   PROCEDURE WriteFloatReg(r: FloatReg);
   BEGIN
      WriteString(FloatRegStr[CARDINAL(r)]);
   END WriteFloatReg;

   PROCEDURE InitRegStr;
      VAR i: CARDINAL;
   BEGIN
      IF Sflag THEN
	 RegStr[0]     := "r0";
	 RegStr[1]     := "r1";
	 IF xelos THEN
	    RegStr[2]  := "limit";
	 ELSE
	    RegStr[2]     := "top";
	 END;
	 RegStr[3]     := "r3";
	 RegStr[4]     := "r4";
	 RegStr[5]     := "r5";
	 RegStr[6]     := "r6";
	 IF xelos THEN
	    RegStr[7]  := "r7";
	 ELSE
	    RegStr[7]     := "limit";
	 END;
	 RegStr[8]     := "r8";
	 RegStr[9]     := "r9";
	 RegStr[10]    := "ra";
	 RegStr[11]    := "rb";
	 RegStr[12]    := "rc";
	 IF xelos THEN
	    RegStr[13] := "top";
	 ELSE
	    RegStr[13]    := "rd";
	 END;
	 RegStr[14]    := "base";
	 RegStr[15]    := "rf";
	 FloatRegStr[CARDINAL(fr0)] := "fr0";
	 FloatRegStr[CARDINAL(fr1)] := "fr1";
	 FloatRegStr[CARDINAL(fr2)] := "fr2";
	 FloatRegStr[CARDINAL(fr3)] := "fr3";
	 FloatRegStr[CARDINAL(fr4)] := "fr4";
	 FloatRegStr[CARDINAL(fr5)] := "fr5";
	 FloatRegStr[CARDINAL(fr6)] := "fr6";
	 FloatRegStr[CARDINAL(fr7)] := "fr7";
      ELSE
	 RegStr[0][0]  := "0";
	 RegStr[1][0]  := "1";
	 RegStr[2][0]  := "2";
	 RegStr[3][0]  := "3";
	 RegStr[4][0]  := "4";
	 RegStr[5][0]  := "5";
	 RegStr[6][0]  := "6";
	 RegStr[7][0]  := "7";
	 RegStr[8][0]  := "8";
	 RegStr[9][0]  := "9";
	 FOR i := 0 TO 9 DO RegStr[i][1] := 0C; END;
	 RegStr[10]    := "10";
	 RegStr[11]    := "11";
	 RegStr[12]    := "12";
	 RegStr[13]    := "13";
	 RegStr[14]    := "14";
	 RegStr[15]    := "15";
	 FloatRegStr[CARDINAL(fr0)][0] := "0";
	 FloatRegStr[CARDINAL(fr1)][0] := "2";
	 FloatRegStr[CARDINAL(fr2)][0] := "4";
	 FloatRegStr[CARDINAL(fr3)][0] := "6";
	 FloatRegStr[CARDINAL(fr4)][0] := "8";
	 FloatRegStr[CARDINAL(fr5)] := "10";
	 FloatRegStr[CARDINAL(fr6)] := "12";
	 FloatRegStr[CARDINAL(fr7)] := "14";
	 FOR i := 0 TO CARDINAL(fr4) DO FloatRegStr[i][1] := 0C END;
      END;
   END InitRegStr;

BEGIN
   star[0] := "*"; star[1] := 0C;
   emptyString[0] := 0C;
   ActSeg := impur;
   AppCom := FALSE;
   EmitPure;
   InitRegStr;
   IF Sflag THEN
      EmitComment("general registers");
      FOR i := 0 TO 15 DO
	 EmitEQUCard(RegStr[i],i);
      END;
      EmitComment("floating point double precision registers");
      FOR i := 0 TO 7 DO
	 EmitEQUCard(FloatRegStr[i],2*i);
      END
   END;
END MCP4CodeSys.
