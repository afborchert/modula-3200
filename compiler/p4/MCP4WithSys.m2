IMPLEMENTATION MODULE MCP4WithSys;

   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM MCP4StatSys IMPORT StatSequ1;
   FROM MCP4Global IMPORT Assert;
   FROM MCP4Designator IMPORT Designator;
   FROM MCP4Scanner IMPORT GetSymbol;
   FROM MCP4AttributSys IMPORT Attribut, AtMode;
   FROM MCBase IMPORT Symbol;
   FROM MCP4Register IMPORT Reg, WithReg, FreeWithReg;
   FROM MCP4Load IMPORT LoadAddr;
   FROM MCP4Labels IMPORT Label;
   FROM MCP4Address IMPORT Address, Cleanup;

   TYPE
      WithPt = POINTER TO WithRecord;
      WithRecord = RECORD
            wat: Attribut;
	    readonly: BOOLEAN;
	    next: WithPt
      END;

   VAR
      firstWith : WithPt;
      withCount : INTEGER;

   PROCEDURE UseWith(i: INTEGER; VAR fat: Attribut);
         (* fat gets the description of the with element.
            May produce code for loading the address of the with variable *)
      VAR
         lWith: WithPt;
         l: INTEGER;
         rg: Reg;
   BEGIN
      lWith := firstWith;
      Assert(i <= withCount);
      FOR l := 1 TO i DO
         lWith := lWith^.next
      END;
      WITH lWith^ DO
         fat := wat;
      END;
   END UseWith;

   PROCEDURE EnterWith(VAR fat: Attribut);
         (* Store fat for further use by UseWith,
             code for entry of a with statement *)
      VAR
         lWith: WithPt;
         i: INTEGER;
         (* RX3-fields of fat *)
         FX, SX: Reg;
         LabelStr: Label; Opr: CHAR; Operand: CARDINAL;
         RX3: BOOLEAN;
   BEGIN
      INC(withCount);
      lWith := firstWith;
      FOR i := 2 TO withCount DO
         lWith := lWith^.next
      END;
      NEW(lWith^.next);
      lWith := lWith^.next;
      WITH lWith^ DO
         wat := fat;
	 readonly := (fat.mode = addrLoadedMod) AND NOT fat.mayRelease;
         Address(fat, FX, SX, LabelStr, Opr, Operand, RX3);
         IF NOT RX3 OR
            (wat.mode <> addrLoadedMod) AND (FX <> r0) AND (FX <> base) OR
            (SX <> r0) THEN
            LoadAddr(wat);
         END;
         IF wat.mode = addrLoadedMod THEN
            WithReg(wat.addrReg);
            readonly := readonly AND (fat.addrReg = wat.addrReg);
	    wat.mayRelease := FALSE;
         END;
      END;
   END EnterWith;

   PROCEDURE ExitWith;
   (* Exit the innermost with statement *)
      VAR
         lWith: WithPt;
         i:     INTEGER;
   BEGIN
      lWith := firstWith;
      FOR i := 1 TO withCount DO
         lWith := lWith^.next
      END;
      WITH lWith^ DO
         IF (wat.mode = addrLoadedMod) AND NOT readonly THEN
            FreeWithReg(wat.addrReg);
         ELSE
            Cleanup(wat);
         END;
      END;
      DISPOSE(lWith);
      Assert(withCount > 0);
      DEC(withCount);
   END ExitWith;

  PROCEDURE WithStatement;
     VAR lat: Attribut;
  BEGIN
     Designator(lat); EnterWith(lat);
     StatSequ1(endsy);
     GetSymbol; ExitWith;
  END WithStatement;

BEGIN
   withCount := 0;
   NEW(firstWith);
END MCP4WithSys.
