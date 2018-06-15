IMPLEMENTATION MODULE MCP4Init;         (* AFB 8/83 *)

   FROM MCP4CodeSys IMPORT EmitLabel, EmitBSS, EmitImpure, EmitPure, EmitEntry,
      EmitExtern, EmitDCF, EmitEQU, EmitFileName, EmitExportedVarLabel;
   FROM MCP4Global IMPORT Assert;
   FROM MCBase IMPORT Idptr, Stptr, mainmodp, root, globvarnext, Varkind,
      Idclass, procnumber, Stringptr, stringroot, adc, xelos;
                                        (* number of procedures in program *)
   FROM Conversions IMPORT ConvertInteger;
   FROM MCP4Public IMPORT sourcefilename;
   FROM SYSTEM IMPORT ADR;

   (*		pure
   		entry	modulename
		dcf	G_AREA		points to the global data field
	moduln. ds	0
	P_AREA	equ	*		procedure addresses
		dcf	LP.0
		dcf	LP.1
		.
		.
		.
                extrn	impmod1		imported modules
		.
		.
		.
		bss
	_FLAG	dsf	1		initflag
	G_AREA	dsf	datasize	uninitialised global area
   *)

   TYPE 
      Label = ARRAY [ 0..7 ] OF CHAR;

   VAR 
      GlobalLab  : Label;
      ProcTabLab : Label;
      Flag       : Label;
      datasize   : CARDINAL;

   PROCEDURE PutModuleHeader(modP: Idptr);
   BEGIN 
      EmitPure;
      IF xelos THEN
	 EmitEntry(modP^.xidentifier);
	 EmitEQU(modP^.xidentifier, "*");
      ELSE
	 EmitEntry(modP^.identifier);
	 EmitEQU(modP^.identifier, "*");
      END;
   END PutModuleHeader;

   PROCEDURE PutPointers;
   BEGIN 
      EmitPure;
      EmitDCF(GlobalLab);
   END PutPointers;

   PROCEDURE PutGlobal(datasize: CARDINAL); (* in Fullwords *)
   BEGIN 
      EmitBSS(ADR(Flag), 1);
      EmitBSS(ADR(GlobalLab), datasize DIV adc);
   END PutGlobal;

   PROCEDURE PutProcTab;
      VAR 
         label : ARRAY [0..7] OF CHAR;
         i, j  : CARDINAL;
         field: ARRAY[0..7-3] OF CHAR;

   BEGIN 
      EmitPure;
      EmitLabel(ADR(ProcTabLab));
      FOR i := 1 TO procnumber DO 
         ConvertInteger(i-1, 1, field);
         label[0] := "L";
         label[1] := "P";
         label[2] := ".";
         FOR j := 3 TO 7 DO
            label[j] := field[j-3];
         END;
         EmitDCF(label);
      END;
   END PutProcTab;

   PROCEDURE PutImports;
      VAR 
         lp: Idptr;
   BEGIN 
      lp := root^.locp^.link;
      WHILE lp <> NIL DO 
	 IF xelos THEN
	    EmitExtern(lp^.xidentifier);
	 ELSE
	    EmitExtern(lp^.identifier);
	 END;
         lp := lp^.link;
      END;
   END PutImports;

   PROCEDURE CalcDatasize; (* determines Size for globalMod Variables *)
      VAR 
         lp: Idptr;
   BEGIN 
      datasize := 0;
      lp := mainmodp^.globvarp;
      WHILE lp<>NIL DO 
         WITH lp^ DO 
            Assert((klass = vars) AND (vkind = noparam));
            IF indaccess THEN 
               INC(datasize, idtyp^.size);
               (* align datasize *)
               IF datasize MOD adc <> 0 THEN
                  INC(datasize, adc - datasize MOD adc);
               END;
            END;
            lp := vlink 
         END 
      END;
      INC(datasize, globvarnext);
      IF datasize MOD adc <> 0 THEN
         INC(datasize, adc - datasize MOD adc);
      END;
   END CalcDatasize;

   PROCEDURE ExportVars;
      VAR 
         lp: Idptr;
   BEGIN 
      lp := mainmodp^.globvarp;
      WHILE lp<>NIL DO 
         WITH lp^ DO 
	    (* may be exported *)
	    EmitExportedVarLabel(vaddr);
            lp := vlink;
         END 
      END;
   END ExportVars;

   PROCEDURE Init;
   BEGIN
      IF xelos THEN
	 EmitFileName(sourcefilename);
      END;
      GlobalLab := "G_AREA";
      ProcTabLab := "P_AREA";
      Flag := "_FLAG";
      PutPointers;
      PutModuleHeader(mainmodp);
      PutProcTab;
      PutImports;
      CalcDatasize;
      PutGlobal(datasize);
      IF xelos THEN
	 ExportVars;
      END;
   END Init;

END MCP4Init. 
