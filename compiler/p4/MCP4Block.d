DEFINITION MODULE MCP4Block; (* AFB 8/83 *)

   FROM MCBase IMPORT
      Idptr, BitsPerWord, doubleword, procmarkspace;
   FROM MCP4Labels IMPORT LabelPtr;

   CONST
      maxlevel = BitsPerWord; (* see MCP2Ident.m2 and MCP3Ident.m2 *)
      ParmOffset = doubleword + procmarkspace;
   VAR
      level : CARDINAL; (* current level *)
      blockNptr : Idptr;
      (* offset to base to get the address of the local *)
      (* activation record; use "addr-offset(base)"     *)
      offset: CARDINAL; (* current offset *)
      (* offsetstack[level] = offset *)
      offsetstack : ARRAY[0..maxlevel-1] OF CARDINAL;

   PROCEDURE Block(fnptr: Idptr);

   PROCEDURE EnterEQU(l, r: LabelPtr);

   PROCEDURE EnterSVC(Label: LabelPtr; Call: CARDINAL; BlockSize: CARDINAL);

   PROCEDURE EnterReal(Label: LabelPtr; r: REAL);

   PROCEDURE CompilationUnit;

END MCP4Block.
