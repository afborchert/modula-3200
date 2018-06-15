DEFINITION MODULE MCP4AttributSys;

   FROM MCBase IMPORT Idptr, Stptr, Stringptr, SetValuePtr;
   FROM MCP4Register IMPORT Reg, FloatReg;
   FROM MCP4Labels IMPORT LabelPtr;

   TYPE
      ArithmeticType = (signed, unSigned, bitwise, floating, logical);
      TestType = (lt, le, eq, ne, ge, gt);

      AtMode  = (globalMod, localMod, floatLoadedMod, loadedMod, addrLoadedMod,
         externalMod, indexMod, byteIndexMod, constantMod, absolutMod,
         doubleConstMod, procedureMod, stringConstMod, conditionMod,
         stackMod, setConstMod, illegalMod);

      ModeSet = SET OF AtMode;

      Attribut = RECORD
         typtr: Stptr; (* type of attribut *)
         CASE mode: AtMode OF
         | globalMod, localMod, absolutMod, indexMod, byteIndexMod,
           addrLoadedMod, externalMod:
               addr: CARDINAL; (* offset *)
               CASE : AtMode OF
               | globalMod, localMod, absolutMod, indexMod, byteIndexMod,
                 addrLoadedMod:
                     addrReg: Reg; (* may be r0 for global/local/absolutMod *)
                     CASE : AtMode OF
                     | globalMod, absolutMod, indexMod, byteIndexMod:
                           addr2Reg: Reg; (* may be r0 *)
                     | localMod, addrLoadedMod:
                           (* used for dynamic array parameters *)
                           CASE : BOOLEAN OF
                           | TRUE:
                                 dynArrLevelDiff, dynArrOffset: CARDINAL;
                           END;
                           CASE : AtMode OF
                           | addrLoadedMod:
			         mayRelease: BOOLEAN;
			         (* = false in case of a with-attribut *)
                           END;
                     END;
               | externalMod: (* external variables *)
                     modPtr: Idptr;
               END;
         | conditionMod: (* condition codes set *)
               test: TestType; (* test for TRUE *)
               atype: ArithmeticType; (* signed (C) or unSigned (CL) *)
               tlabel: LabelPtr; (* may be NIL; branch on TRUE condition *)
               flabel: LabelPtr; (* may be NIL; branch on FALSE condition *)
         | constantMod:
               CASE : BOOLEAN OF
               | TRUE:  value: CARDINAL;
               | FALSE: iValue: INTEGER;
               END;
         | doubleConstMod:
               CASE : BOOLEAN OF
               | TRUE: Real1, Real2: CARDINAL
               | FALSE: Real: REAL
               END;
         | stringConstMod:
               strgPtr: Stringptr
	 | setConstMod:
	       setPtr: SetValuePtr;
         | procedureMod:
              procPtr: Idptr;
         | floatLoadedMod:
               floatLoadReg: FloatReg;
         | loadedMod:
               loadReg: Reg;
         | stackMod:
               offset: CARDINAL; (* to base *)
               size: CARDINAL;
	       (* NOT size of type; size of stack use in fullwords *)
         END;
      END (* Attribut *) ;

END MCP4AttributSys.
