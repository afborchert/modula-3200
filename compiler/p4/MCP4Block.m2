IMPLEMENTATION MODULE MCP4Block;        (* AFB 8/83 *)
                                        (* REV AFB 5/84 *)

   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM MCBase IMPORT Idptr, Idclass, Stptr, Symbol, mainmodp, globvarnext,
      root, Varkind, ismain, oneword, adc, procmarkspace, noprio, BitsPerWord,
      procnumber, xelos;
   FROM MCP4Global IMPORT CompilerError, Error, Assert;
   FROM MCP4Scanner IMPORT GetSymbol, sy, nptr, line, pline;
   FROM MCP4StatSys IMPORT StatSequ1;
   FROM MCP4CodeSys IMPORT EmitAlign, EmitComment, AppendValue, EmitLabel,
      EmitEQU, EmitDCFValue, EmitPure, EmitImpure, star, EmitBranch,
      EmitRR, EmitRX3, EmitRI, EmitSF, EmitRX3Label, noOpr, EmitEntry,
      AppendComment, EmitRXRX, EmitRX3Int, EmitExtern, EmitBSS,
      EmitEQUCard, EmitRILabel, EmitExportedProcLabel, EmitProcExtern,
      EmitDef, EmitDefOptCard, EmitDefOptStr, EmitEndDef, EmitDefPV;
   FROM Conversions IMPORT ConvertInteger;
   FROM MCP4Register IMPORT Reg, GetReg, GetRegTriplet,
      FreeReg, FreeRegTriplet, top, limit;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Labels IMPORT LabelPtr, GetLabel, PushLabel, PopLabel,
      TopLabel, LabelType;
   FROM MCP4Types IMPORT IsArrayType, ByteSize;
   FROM MCP4ConstArithmetic IMPORT ConstMulReg;
   FROM MCP4AttributSys IMPORT ArithmeticType;
   FROM MCP4Public IMPORT profile;
   FROM MCP4Stack IMPORT SetStartOffset, GetStackUse;
   IMPORT MCP4CodeSys, Storage, MCMnemonics, MCP4Register, MCP4Labels,
      MCP4Public, MCP4Stack, MCBase;

   (* $R- *)

   CONST
      global = "G_AREA";
      flag   = "_FLAG";
      startLabel = "_M2START";
      entprio = ".entprio";
      exprio = ".exprio";
      fret = ".fret"; (* function returns no value error *)
      mcount = ".mcount";
   VAR
      (* avoid undefined procedure labels (empty module bodies) *)
      ProcNumUsed: ARRAY[0..15] OF BITSET;
      index: CARDINAL;
      StackUse: LabelPtr;

   PROCEDURE Block(fnptr: Idptr);
      VAR 
	 procnptr: Idptr;
	 ReturnLabel: LabelPtr;
   BEGIN 
      (* calculate base offset *)
      IF fnptr <> NIL THEN
         WITH fnptr^ DO
            IF klass = mods THEN
               offsetstack[level] := procmarkspace;
            ELSE
               offsetstack[level] := idtyp^.parlength;
            END;
         END;
      ELSE
         offsetstack[level] := 0;
      END;
      WHILE (sy = proceduresy) DO 
         procnptr := nptr;
         GetSymbol;
         INC(level);
         Block(procnptr);
         DEC(level)
      END;
      blockNptr := fnptr;
      GenBlockEntry;
      IF sy = beginsy THEN 
         GetSymbol;
         StatSequ1(endblock)
      END;
      TerminateBlock;
      Assert(sy=endblock);
      GetSymbol                         (*endblock*)
   END Block;

   PROCEDURE GenBlockEntry;
      VAR 
	 label: ARRAY [ 0..7 ] OF CHAR;
         field: ARRAY[0..7-3] OF CHAR;
	 j: CARDINAL;
	 rx, ry: Reg;
	 rlen: Reg;
	 Userx: BOOLEAN;
	 CopyLabel, ReturnLabel: LabelPtr;
         ProfileLabel: LabelPtr;
	 lpp, lvp: Idptr;
	 done: BOOLEAN;
         ParmOffsetAllocated: BOOLEAN;

   BEGIN 
      IF blockNptr <> NIL THEN
         WITH blockNptr^ DO
            offset := offsetstack[level];
            ParmOffsetAllocated := FALSE;
            (* emit procedure label *)
            GetLabel(blockl, ReturnLabel);
            PushLabel(blockl, ReturnLabel);
            label[0] := "L";
            label[1] := "P";
            label[2] := ".";
            ConvertInteger(procnum, 1, field);
            FOR j := 3 TO 7 DO
               label[j] := field[j-3];
            END;
            INCL(ProcNumUsed[procnum DIV BitsPerWord], procnum MOD BitsPerWord);
            EmitPure;
            EmitAlign;
            EmitEQU(label,star);
	    IF xelos THEN
	       IF plev = 1 THEN
		  (* may be exported *)
		  EmitExportedProcLabel(procnum);
	       END;
	       EmitDefPV(".def", mainmodp^.xidentifier, 'P', procnum);
	       EmitDefOptStr(".val", ".");
	       EmitDefOptCard(".scl", 2); (* external *)
	       EmitDefOptStr(".type", "y'20'"); (* procedure *)
	       EmitEndDef;
	    END;
            IF ismain AND (procnum = 0) THEN
               EmitEntry(startLabel);
	       EmitEQU(startLabel, "*");
            END;

            (* set up activation record *)
            IF (level = 1) AND (klass = funcs) AND
               (idtyp^.parlength = procmarkspace) THEN
               EmitRI(AI, top, r0, procmarkspace);
            END;
            (* dynamic link *)
            EmitRX3Int(STM, base, top, r0, -procmarkspace+oneword);
   	    EmitRR(LR, base, top);

            (* StackUse: symbol for allocated stack size in bytes *)
            GetLabel(ifl, StackUse);

            (* enter priority (if necessary) *)
            IF (plev = 1) AND (priolev <> noprio) THEN
               EmitSF(LIS, r0, priolev);
               EmitExtern(entprio);
               EmitRX3Label(BAL, rf, r0, r0, entprio, noOpr, 0);
            END;

            IF blockNptr = mainmodp THEN
               EmitRX3Label(TS, r0, r0, r0, flag, noOpr, 0);
               ReturnLabel := TopLabel(blockl);
               EmitBranch(BM, r0, r0, ReturnLabel^);

               (* allocate global arrays and records *)
               lvp := globvarp;
               Userx := FALSE;
               WHILE lvp <> NIL DO
                  WITH lvp^ DO
                     (* cannot happen in this version *)
                     IF indaccess THEN
                        IF NOT Userx THEN
                           Userx := TRUE;
                           GetReg(rx);
                           EmitRX3Label(LA, rx, r0, r0, global, '+', globvarnext);
                        END;
                        EmitRX3Label(ST, rx, r0, r0, global, '+', vaddr);
                        IF vlink <> NIL THEN
                           IF idtyp^.size MOD adc <> 0 THEN
                              EmitRI(AI, rx, r0, idtyp^.size + adc -
                                                 idtyp^.size MOD adc);
                           ELSE
                              EmitRI(AI, rx, r0, idtyp^.size);
                           END;
                        END;
                     END;
                  lvp := vlink;
                  END
               END;
               IF Userx THEN
                  FreeReg(rx);
               END;
               lpp := root^.locp^.link;

               (* initialize imported modules *)
               EmitRILabel(AI, top, r0, StackUse^, "+", ParmOffset);
               ParmOffsetAllocated := TRUE;
               WHILE lpp <> NIL DO
                  IF lpp <> mainmodp THEN
		     IF xelos THEN
			EmitProcExtern(lpp^.xidentifier, 0);
			EmitRX3Label(BAL, rf, r0, r0, lpp^.xidentifier,
			   'P', 0);
		     ELSE
			EmitRX3Label(L, r1, r0, r0, lpp^.identifier, noOpr, 0);
			EmitRR(BALR, rf, r1);
		     END;
                  END;
                  lpp := lpp^.link;
               END;
               lpp := NIL;
               SetStartOffset(0); (* relative to base *)
            ELSIF klass = mods THEN
               lpp := NIL;
               lvp := NIL;
               EmitRILabel(AI, top, r0, StackUse^, "+", ParmOffset);
               ParmOffsetAllocated := TRUE;
               SetStartOffset(0); (* relative to base *)
            ELSE
               (* proc/func entry *)
               lvp := locvarp;
               lpp := idtyp^.fstparam;

	       (* check if ParmOffset can be allocated now, *)
	       (* i.e. no copy parameters and no indirect accessed *)
	       (* variables are present: *)
	       ParmOffsetAllocated := TRUE;
	       WHILE lpp <> NIL DO
		  IF lpp^.vkind = copyparam THEN
		     ParmOffsetAllocated := FALSE; lpp := NIL;
		  ELSE
		     lpp := lpp^.vlink;
		  END;
	       END;
	       WHILE ParmOffsetAllocated AND (lvp <> NIL) DO
		  IF lvp^.indaccess THEN
		     ParmOffsetAllocated := FALSE;
		  ELSE
		     lvp := lvp^.vlink;
		  END;
	       END;
	       lvp := locvarp;
	       lpp := idtyp^.fstparam;

	       IF ParmOffsetAllocated THEN
		  EmitRILabel(AI, top, r0, StackUse^, "+",
			   varlength-offset+ParmOffset);
	       ELSE
		  EmitRILabel(AI, top, r0, StackUse^, "+",
                           varlength-offset);
	       END;

               (* set start offset for stack allocation *)
               SetStartOffset(varlength-offset); (* relative to base *)
            END;

            (* allocate space for copy parameters *)
            WHILE lpp <> NIL DO
               WITH lpp^ DO
                  IF vkind = copyparam THEN

		     (* rlen: # bytes to be moved *)
		     (* copy from rx to ry        *)

		     GetReg(rlen); GetReg(rx); GetReg(ry);
		     (* calculate # bytes to be moved *)
                     IF IsArrayType(idtyp) AND idtyp^.dyn THEN
                        (* MCP4CallSys: vaddr(base) = copy address *)
                        (*      vaddr+oneword(base) = HIGH         *)
                        EmitRX3Int(L, rlen, base, r0, vaddr+oneword-offset);
                        EmitSF(AIS, rlen, 1);
                        IF ByteSize(idtyp^.elp) THEN
                        ELSE
                           ConstMulReg(unSigned, rlen, idtyp^.elp^.size)
                        END;
                     ELSE
                        EmitRI(LI, rlen, r0, idtyp^.size);
                     END;
		     EmitRX3Int(L, rx, base, r0, vaddr-offset);
		     EmitRR(LR, ry, top);
                     EmitRX3Int(ST, ry, base, r0, vaddr-offset);
		     EmitRR(AR, top, rlen);
                     (* align top-register *)
                     IF IsArrayType(idtyp) AND idtyp^.dyn AND
                        ByteSize(idtyp^.elp) THEN
                        (* other types are aligned *)
                        EmitSF(AIS, top, 3);
                        EmitRI(NHI, top, r0, 177774B);
                     ELSIF idtyp^.size MOD adc <> 0 THEN
                        EmitSF(AIS, top, adc - idtyp^.size MOD adc);
                     END;

		     EmitRXRX(MOVE, rlen, ry, r0, rlen, rx, r0);
		     FreeReg(rlen); FreeReg(rx); FreeReg(ry);
                  END
               END;
               lpp := lpp^.vlink;
            END;
            (* allocate space for local variables *)
            WHILE lvp <> NIL DO
               WITH lvp^ DO
                  IF indaccess THEN
                     EmitRX3Int(ST, top, base, r0, vaddr-offset);
                     (* keep top aligned !! *)
                     IF idtyp^.size MOD adc = 0 THEN
                        EmitRI(AI, top, r0, idtyp^.size)
                     ELSE
                        EmitRI(AI, top, r0, idtyp^.size + adc -
                                            idtyp^.size MOD adc);
                     END;
                  END;
                  lvp := vlink;
               END
            END;
            (* count procedure calls if profile is on *)
            IF profile THEN
               GetLabel(casel, ProfileLabel);
               EmitBSS(ProfileLabel, 1 (* fullword *));
               EmitPure;
               EmitRX3Label(LA, r1, r0, r0, ProfileLabel^, noOpr, 0);
               EmitRX3Label(BAL, rf, r0, r0, mcount, noOpr, 0);
               DISPOSE(ProfileLabel);
            END;
            IF NOT ParmOffsetAllocated THEN
               EmitRI(AI, top, r0, ParmOffset);
            END;
	    (* check for stack overflow *)
	    Check;
            (* generate special label for the debugger (for breakpoints) *)
	    IF xelos THEN
	       EmitDef(".bf"); (* begin of function *)
	       EmitDefOptStr(".val", ".");
	       EmitDefOptCard(".scl", 101);  (* storage class: function *)
	       EmitDefOptCard(".line", line); (* starting line number *)
	       EmitEndDef;
	       pline := 1;
	    ELSE
	       label[0] := 'B'; label[1] := 'B'; (* breakpoint at the begin *)
	       EmitEQU(label, star);
	    END;
         END;
      END;
   END GenBlockEntry;

   PROCEDURE TerminateBlock;
      VAR label: ARRAY[0..7] OF CHAR;
          field: ARRAY[0..7-3] OF CHAR;
          j: CARDINAL;
          size: CARDINAL;
   BEGIN 
      IF blockNptr <> NIL THEN
         (* a function must return a value *)
         WITH blockNptr^ DO
            IF klass = funcs THEN
               EmitExtern(fret);
               EmitRX3Label(BAL, r1, r0, r0, fret, noOpr, 0);
            END;
         END;
         (* emit label for RETURN *)
         EmitLabel(PopLabel(blockl));
	 (* generate label for stack overflow check *)
	 EmitOffsetLabel;
         (* generate label for allocated stack size in bytes *)
         GetStackUse(size); (* size in fullwords *)
         EmitEQUCard(StackUse^, size * adc);
         DISPOSE(StackUse);
         (* generate special label for the debugger *)
	 IF xelos THEN
	    EmitDef(".ef");
	    EmitDefOptStr(".val", ".");
	    EmitDefOptCard(".scl", 101);
	    EmitDefOptCard(".line", pline);
	    EmitEndDef;
	    pline := 0;
	 ELSE
	    ConvertInteger(blockNptr^.procnum, 1, field);
	    label[0] := 'B'; label[1] := 'E'; (* breakpoint at the end *)
	    label[2] := '.';
	    FOR j := 3 TO 7 DO
	       label[j] := field[j-3];
	    END;
	    EmitEQU(label, star);
	 END;
         (* exit priority (if necessary) *)
         WITH blockNptr^ DO
            IF (plev = 1) AND (priolev <> noprio) THEN
               EmitExtern(exprio);
               EmitRX3Label(BAL, rf, r0, r0, exprio, noOpr, 0);
            END;
         END;
         (* return sequence *)
         EmitRR(LR, top, base);
         EmitRX3Int(LM, base, top, r0, -procmarkspace+oneword);
         IF blockNptr^.klass = funcs THEN
            EmitRI(SI, top, r0, offset);
         ELSIF offset > ParmOffset THEN
            EmitRI(SI, top, r0, offset-ParmOffset);
         END;
         EmitRR(BR, rf, r0);
         FlushEQUs;
         FlushReals;
      END
   END TerminateBlock;

   MODULE Equates;

      IMPORT EmitEQU, LabelPtr, ALLOCATE, DEALLOCATE;
      EXPORT EnterEQU, FlushEQUs;

      TYPE
         EQURel = POINTER TO Node;
         Node = RECORD
            left, right: LabelPtr;
            next: EQURel;
         END;

      VAR list: EQURel;

      PROCEDURE EnterEQU(l, r: LabelPtr);
         VAR ptr: EQURel;
      BEGIN
         NEW(ptr);
         NEW(ptr^.left);
         NEW(ptr^.right);
         ptr^.left^ := l^; ptr^.right^ := r^;
         ptr^.next := list;
         list := ptr;
      END EnterEQU;

      PROCEDURE FlushEQUs;
         VAR ptr: EQURel;
      BEGIN
         ptr := list;
         WHILE ptr <> NIL DO
            WITH ptr^ DO
               EmitEQU(left^, right^);
               DISPOSE(left); DISPOSE(right);
               ptr := next;
            END;
            DISPOSE(list);
            list := ptr;
         END;
      END FlushEQUs;

   BEGIN
      list := NIL;
   END Equates;

   MODULE SVCs;

      FROM Storage IMPORT ALLOCATE, DEALLOCATE;
      FROM MCMnemonics IMPORT Mnemonic;
      FROM MCP4CodeSys IMPORT EmitRX3, EmitDS, EmitImpure, EmitLabel, EmitAlign;
      FROM MCP4Register IMPORT Reg;
      IMPORT LabelPtr;
      EXPORT EnterSVC, FlushSVCs;

      TYPE
         SVCPtr = POINTER TO SVCNode;
         SVCNode =
            RECORD
               label: LabelPtr;
               nr: CARDINAL;
               size: CARDINAL;
               next: SVCPtr;
            END;
      VAR
         list: SVCPtr;

      PROCEDURE EnterSVC(Label: LabelPtr; Call: CARDINAL; BlockSize: CARDINAL);
         VAR ptr: SVCPtr;
      BEGIN
         NEW(ptr);
         WITH ptr^ DO
            label := Label;
            nr := Call;
            size := BlockSize;
            next := list;
         END;
         list := ptr;
      END EnterSVC;

      PROCEDURE FlushSVCs;
         VAR ptr: SVCPtr;
      BEGIN
         EmitImpure; (* the following text may be changed by UNIX *)
         ptr := list;
         WHILE ptr <> NIL DO
            WITH ptr^ DO
               EmitAlign;
               (* the arguments of the svc must be fullword aligned *)
               EmitLabel(label);
               EmitRX3(SVC, r0, r0, r0, nr);
               EmitDS(size);
               DISPOSE(label);
            END;
            ptr := ptr^.next;
            DISPOSE(list);
            list := ptr;
         END;
      END FlushSVCs;

   BEGIN
      list := NIL;
   END SVCs;

   MODULE RealConstants;

      FROM MCP4Labels IMPORT LabelPtr;
      FROM MCP4CodeSys IMPORT EmitLabel, EmitDCFValue, EmitPure, EmitAlign;
      FROM Storage IMPORT ALLOCATE, DEALLOCATE;

      EXPORT EnterReal, FlushReals;

      TYPE
         RealConstPtr = POINTER TO RealNode;
         RealNode =
            RECORD
               label: LabelPtr;
               CASE : BOOLEAN OF
                 TRUE:
                   const: REAL;
               | FALSE:
                   const1, const2: INTEGER;
               END;
               link: RealConstPtr;
            END;

      VAR
         list: RealConstPtr;

      PROCEDURE EnterReal(l: LabelPtr; r: REAL);
         VAR p: RealConstPtr;
      BEGIN
         NEW(p);
         WITH p^ DO
            label := l;
            const := r;
            link := list;
         END;
         list := p;
      END EnterReal;

      PROCEDURE FlushReals;
         TYPE DoubleWord = ARRAY[0..1] OF INTEGER;
         VAR p: RealConstPtr;
             double: DoubleWord;
      BEGIN
         IF list = NIL THEN RETURN END;
         EmitPure;
         EmitAlign;
         p := list;
         WHILE p <> NIL DO
            WITH p^ DO
               EmitLabel(label);
               DISPOSE(label);
               double := DoubleWord(const);
               EmitDCFValue(double[0]); EmitDCFValue(double[1]);
               p := link;
            END;
            DISPOSE(list);
            list := p;
         END;
      END FlushReals;

   BEGIN (* MODULE RealConstants *)
      list := NIL;
   END RealConstants;

   PROCEDURE EmitUndefinedProcLabels;
      VAR index: CARDINAL;
          label: ARRAY[0..7] OF CHAR;
          field: ARRAY[0..7-3] OF CHAR;
          j: CARDINAL;
   BEGIN
      EmitPure;
      AppendComment("unused procedure labels");
      FOR index := 0 TO procnumber-1 DO
         IF NOT (index MOD BitsPerWord
                 IN ProcNumUsed[index DIV BitsPerWord]) THEN
            ConvertInteger(index, 1, field);
            label[0] := "L";
            label[1] := "P";
            label[2] := ".";
            FOR j := 3 TO 7 DO
               label[j] := field[j-3];
            END;
            EmitEQU(label, star);
         END;
      END;
   END EmitUndefinedProcLabels;

   MODULE Stack;

      FROM Storage IMPORT DEALLOCATE;
      FROM MCP4Stack IMPORT GetMaxIncTop;
      FROM MCP4CodeSys IMPORT EmitEQUCard, EmitRX3Label, EmitRR,
	 EmitBranch, EmitLabel, EmitExtern, noOpr;
      FROM MCP4Labels IMPORT LabelPtr, ifl, GetLabel;
      FROM MCMnemonics IMPORT Mnemonic;
      FROM MCP4Register IMPORT Reg, top, limit;
      FROM MCP4Public IMPORT sflag;
      FROM MCBase IMPORT xelos;
      EXPORT Check, EmitOffsetLabel;

      VAR
         label: LabelPtr; (* label equ max *)

      (*
       * check for max_offset(top) < limit
       *)

      PROCEDURE Check;
         CONST stackError = ".stack";
         VAR okay: LabelPtr;
      BEGIN
	 IF NOT sflag THEN
	    GetLabel(ifl, label);
	    EmitRX3Label(LA, r1, top, r0, label^, noOpr, 0);
	    EmitRR(CR, r1, limit);
	    GetLabel(ifl, okay);
	    EmitBranch(BM, r0, r0, okay^);
	    EmitExtern(stackError);
	    EmitRX3Label(BAL, r1, r0, r0, stackError, noOpr, 0);
	    EmitLabel(okay);
	    DISPOSE(okay);
	 END;
      END Check;

      PROCEDURE EmitOffsetLabel;
         VAR incr: CARDINAL;
      BEGIN
	 IF NOT sflag THEN
            GetMaxIncTop(incr);
	    EmitEQUCard(label^, incr);
	    DISPOSE(label);
	 END;
      END EmitOffsetLabel;

   END Stack;

   PROCEDURE CompilationUnit;
   BEGIN
      IF profile THEN
         EmitExtern(mcount);
      END;
      FOR index := 0 TO HIGH(ProcNumUsed) DO
         ProcNumUsed[index] := { };
      END;
      level := 0;
      GetSymbol;
      Block(NIL);
      FlushSVCs;
      EmitUndefinedProcLabels;
   END CompilationUnit;

END MCP4Block. 
