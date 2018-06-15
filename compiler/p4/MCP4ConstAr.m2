IMPLEMENTATION MODULE MCP4ConstArithmetic; (* AFB 11/83 *)

   FROM MCBase IMPORT cardptr, intptr, BitsPerWord;
   FROM MCP4AttributSys IMPORT ArithmeticType, Attribut, AtMode;
   FROM MCP4CodeSys IMPORT EmitComment, EmitRI, EmitSF, EmitRR;
   FROM MCP4Register IMPORT Reg, FreeReg, RegRequest;
   FROM MCP4Global IMPORT CompilerError, Error, Assert;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Load IMPORT Load, LoadReg;
   FROM MCP4ExpressionSys IMPORT Mult, Div, Mod;
   FROM MCP4Address IMPORT Cleanup;

   (* only signed/unsigned arithmetic *)

   PROCEDURE ConstMul(aT: ArithmeticType; VAR fat: Attribut; val: CARDINAL);
      VAR
         power, mask: CARDINAL;
         lat: Attribut;
         done: BOOLEAN;
         rg: Reg;
   BEGIN
      WITH fat DO
	 IF val = 1 THEN
	    (* do nothing *)
	 ELSIF val = 0 THEN
            Cleanup(fat);
            fat.mode := constantMod; fat.value := 0;
         ELSE
            GetPower(power, mask, val, done);
            IF done THEN
               Load(fat); rg := fat.loadReg;
               IF aT = signed THEN
                  EmitRI(SLA, rg, r0, power);
               ELSE
                  EmitRI(SLL, rg, r0, power);
               END;
            ELSE
               SetConstAt(lat, val, aT);
               Mult(fat, lat, aT);
            END;
	 END;
      END;
   END ConstMul;

   PROCEDURE ConstDiv(aT: ArithmeticType; VAR fat: Attribut; val: CARDINAL);
      VAR
         power, mask: CARDINAL;
         done: BOOLEAN;
         lat: Attribut;
         rg: Reg;
   BEGIN
      WITH fat DO
	 IF val = 1 THEN
	    (* do nothing *)
	 ELSIF val = 0 THEN
            Error(221);
         ELSE
            GetPower(power, mask, val, done);
            IF done THEN
               Load(fat); rg := fat.loadReg;
               IF aT = signed THEN
                  EmitRI(SRA, rg, r0, power);
               ELSE
                  EmitRI(SRL, rg, r0, power);
               END;
            ELSE
               SetConstAt(lat, val, aT);
               Div(fat, lat, aT);
            END;
	 END;
      END;
   END ConstDiv;

   PROCEDURE ConstMod(aT: ArithmeticType; VAR fat: Attribut; val: CARDINAL);
      VAR
         power, mask: CARDINAL;
         done: BOOLEAN;
         lat: Attribut;
         rg: Reg;
   BEGIN
      WITH fat DO
	 IF val = 1 THEN
            Cleanup(fat);
            mode := constantMod;
            value := 0;
	 ELSIF val = 0 THEN
            Error(221);
         ELSE
            GetPower(power, mask, val, done);
            IF done THEN
               Load(fat); rg := fat.loadReg;
               EmitRI(NI, rg, r0, mask-1);
            ELSE
               SetConstAt(lat, val, aT);
               Mod(fat, lat, aT);
            END;
	 END;
      END;
   END ConstMod;

   PROCEDURE ConstMulReg(aT: ArithmeticType; rg: Reg; val: CARDINAL);
      VAR
         fat, lat: Attribut;
         done: BOOLEAN;
         power, mask: CARDINAL;
   BEGIN
      IF val = 0 THEN
         EmitSF(LIS, rg, 0);
      ELSIF val = 1 THEN
         (* do nothing *)
      ELSE (* val >= 2 *)
         GetPower(power, mask, val, done);
         IF done THEN
            IF aT = signed THEN
               EmitRI(SLA, rg, r0, power);
            ELSE
               EmitRI(SLL, rg, r0, power);
            END;
         ELSE
            SetAttributes(fat, lat, aT, rg, val);
            Mult(fat, lat, aT);
            (* fat may be in another register *)
            IF fat.loadReg <> rg THEN
               RegRequest(rg, done);
               Assert(done);
               LoadReg(fat, rg);
            END;
         END;
      END;
   END ConstMulReg;

   PROCEDURE ConstDivReg(aT: ArithmeticType; rg: Reg; val: CARDINAL);
      VAR
         fat, lat: Attribut;
         done: BOOLEAN;
         power, mask: CARDINAL;
   BEGIN
      IF val = 0 THEN
         Error(221);
      ELSIF val = 1 THEN
         (* do nothing *)
      ELSE (* val >= 2 *)
         GetPower(power, mask, val, done);
         IF done THEN
            IF aT = signed THEN
               EmitRI(SRA, rg, r0, power);
            ELSE
               EmitRI(SRL, rg, r0, power);
            END;
         ELSE
            SetAttributes(fat, lat, aT, rg, val);
            Div(fat, lat, aT);
            (* fat may be in another register *)
            IF fat.loadReg <> rg THEN
               RegRequest(rg, done);
               Assert(done);
               LoadReg(fat, rg);
            END;
         END;
      END;
   END ConstDivReg;

   PROCEDURE ConstModReg(aT: ArithmeticType; rg: Reg; val: CARDINAL);
      VAR
         fat, lat: Attribut;
         done: BOOLEAN;
         power, mask: CARDINAL;
   BEGIN
      IF val = 0 THEN
         Error(221);
      ELSIF val = 1 THEN
         EmitSF(LIS, rg, 0);
      ELSE (* val >= 2 *)
         GetPower(power, mask, val, done);
         IF done THEN
            EmitRI(NI, rg, r0, mask-1);
         ELSE
            SetAttributes(fat, lat, aT, rg, val);
            Mod(fat, lat, aT);
            (* fat may be in another register *)
            IF fat.loadReg <> rg THEN
               RegRequest(rg, done);
               Assert(done);
               LoadReg(fat, rg);
            END;
         END;
      END;
   END ConstModReg;

   (* local routines *)

   PROCEDURE SetAttributes(VAR fat, lat: Attribut; AType: ArithmeticType;
                           rg: Reg; val: CARDINAL);
   BEGIN
      fat.mode := loadedMod; fat.loadReg := rg;
      IF AType = signed THEN
         fat.typtr := intptr;
      ELSE
         fat.typtr := cardptr;
      END;
      lat.mode := constantMod; lat.value := val;
      lat.typtr := fat.typtr;
   END SetAttributes;

   PROCEDURE SetConstAt(VAR lat: Attribut; val: CARDINAL;
                        AType: ArithmeticType);
   BEGIN
      lat.mode := constantMod; lat.value := val;
      IF AType = signed THEN
         lat.typtr := intptr;
      ELSE
         lat.typtr := cardptr;
      END;
   END SetConstAt;

   PROCEDURE GetPower(VAR power, mask: CARDINAL;
                      val: CARDINAL; VAR done: BOOLEAN);
   BEGIN
      mask := 1;
      FOR power := 1 TO BitsPerWord-1 DO
         mask := mask * 2;
         IF mask = val THEN done := TRUE; RETURN END;
      END;
      done := FALSE;
   END GetPower;

END MCP4ConstArithmetic.
