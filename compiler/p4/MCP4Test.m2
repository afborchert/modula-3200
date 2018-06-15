IMPLEMENTATION MODULE MCP4Test;         (* AFB 5/85 *)

   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Labels IMPORT LabelPtr, GetLabel, ifl;
   FROM MCP4CodeSys IMPORT EmitLabel, EmitBranch;
   FROM MCP4AttributSys IMPORT ArithmeticType, TestType;
   FROM MCP4Register IMPORT Reg;
   FROM Storage IMPORT DEALLOCATE;

   PROCEDURE Invert(t: TestType) : TestType;
   BEGIN 
      CASE t OF 
        lt: 
            RETURN ge;
      | le: 
            RETURN gt;
      | eq: 
            RETURN ne;
      | ne: 
            RETURN eq;
      | ge: 
            RETURN lt;
      | gt: 
            RETURN le;
      END;
   END Invert;

   PROCEDURE BranchMnem(t: TestType; atype: ArithmeticType; VAR bm1, bm2: 
      Mnemonic);
   BEGIN 
      IF atype = signed THEN 
         CASE t OF 
           lt: 
               bm1 := BM;
         | le: 
               bm1 := BNP;
         | eq: 
               bm1 := BE;
         | ne: 
               bm1 := BNE;
         | ge: 
               bm1 := BNM;
         | gt: 
               bm1 := BP;
         END;
         bm2 := bm1;
      ELSE 
         CASE t OF 
           lt: 
               bm1 := BL;
               bm2 := BL;
         | le: 
               bm1 := BL;
               bm2 := BE;
         | eq: 
               bm1 := BE;
               bm2 := BE;
         | ne: 
               bm1 := BNE;
               bm2 := BNE;
         | ge: 
               bm1 := BNL;
               bm2 := BNL;
         | gt: 
               bm1 := BE;
               bm2 := BNL;
         END;
      END;
   END BranchMnem;

   PROCEDURE Test(test: TestType; atype: ArithmeticType; dest: LabelPtr);
      VAR 
         cmp, bm1, bm2: Mnemonic;
         testLabel    : LabelPtr;
   BEGIN 
      BranchMnem(test, atype, bm1, bm2);
      IF (bm1 = BE) AND (bm2 = BNL) THEN 
         GetLabel(ifl, testLabel);
         EmitBranch(BE, r0, r0, testLabel^);
         EmitBranch(BNL, r0, r0, dest^);
         EmitLabel(testLabel);
         DISPOSE(testLabel);
      ELSE 
         EmitBranch(bm1, r0, r0, dest^);
         IF bm1 <> bm2 THEN 
            EmitBranch(bm2, r0, r0, dest^);
         END;
      END;
   END Test;

END MCP4Test. 
