(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for ENDLICH        *
*                                       *
*     MCPublic:                         *
*                                       * 
*     public part of the common base    *
*     of the Modula-2 compiler          *
*                                       * 
****************************************)

DEFINITION MODULE MCP1Public;             (* LG *) (* REV AFB 3/84 *)

   FROM MCBase IMPORT Spellix;

   (*
   EXPORT QUALIFIED
      ascName, srcName, il1Name, symNames, ErrorsFound, String14,
      FileNameLength, FileName, SymNamePtr, SymNameNode, SymFilesMissing,
      listing, fast;
   *)

   CONST
      FileNameLength = 128;
   TYPE
      FileName = ARRAY[0..FileNameLength-1] OF CHAR;
      String14 = ARRAY[0..13] OF CHAR;
      SymNamePtr = POINTER TO SymNameNode;
      SymNameNode =
	 RECORD
	    symName: FileName;
	    moduleName: Spellix;
	    link: SymNamePtr;
	 END;
   VAR 
      ascName : FileName;                   (* identifier table file *)
      srcName : FileName;                   (* compiler input file *)
      il1Name : FileName;		    (* interpass file *)
      symNames : SymNamePtr;
      listing : BOOLEAN;
      fast : BOOLEAN;

      ErrorsFound : BOOLEAN;
      SymFilesMissing : BOOLEAN;

END MCP1Public. 
