IMPLEMENTATION MODULE MCP4Store; (* AFB 11/83 *)

   FROM MCP4AttributSys IMPORT Attribut, AtMode;
   FROM MCP4CodeSys IMPORT EmitRX3, EmitComment, EmitRX3Label,
      EmitFloatRX3, EmitFloatRX3Label, EmitRX3Int, EmitFloatRX3Int,
      EmitVarExtern;
   FROM MCP4Register IMPORT FreeReg, Reg, FloatReg;
   FROM MCP4Load IMPORT LoadAddr;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Global IMPORT CompilerError;
   FROM MCP4Types IMPORT ByteSize;
   FROM MCBase IMPORT adc, xelos;
   IMPORT MCP4Block;

   CONST
      global = "G_AREA";

   PROCEDURE Store(rg: Reg; VAR fat: Attribut);
      VAR StoreInst: Mnemonic;
   BEGIN
      WITH fat DO
         IF ByteSize(typtr) THEN
            StoreInst := STB;
         ELSE
            StoreInst := ST;
         END;
	 CASE mode OF
	   globalMod:
               EmitRX3Label(StoreInst, rg, addrReg, addr2Reg, global, '+', addr);
               IF addrReg <> r0 THEN
                  FreeReg(addrReg);
               END;
               IF addr2Reg <> r0 THEN
                  FreeReg(addr2Reg);
               END;
	 | localMod:
               (* $R- *)
	       EmitRX3Int(StoreInst, rg, base, addrReg, addr-MCP4Block.offset);
               IF addrReg <> r0 THEN
                  FreeReg(addrReg);
               END;
               (* $R= *)
	 | addrLoadedMod:
	       EmitRX3(StoreInst, rg, addrReg, r0, addr);
	       IF mayRelease THEN
		  FreeReg(addrReg);
	       END;
         | indexMod, byteIndexMod:
               EmitRX3(StoreInst, rg, addrReg, addr2Reg, addr);
               FreeReg(addrReg);
               IF addr2Reg <> r0 THEN
                  FreeReg(addr2Reg);
               END;
	 | absolutMod:
	       EmitRX3(StoreInst, rg, addrReg, addr2Reg, addr);
               IF addrReg <> r0 THEN
                  FreeReg(addrReg);
               END;
               IF addr2Reg <> r0 THEN
                  FreeReg(addr2Reg);
               END;
	 | externalMod:
	       IF xelos THEN
		  EmitVarExtern(modPtr^.xidentifier, addr);
		  EmitRX3Label(StoreInst, rg, r0, r0,
		     modPtr^.xidentifier, 'V', addr);
	       ELSE
		  EmitRX3Label(L, r1, r0, r0, modPtr^.identifier, '-', adc);
		  EmitRX3(StoreInst, rg, r1, r0, addr);
	       END;
	 ELSE
	    CompilerError;
	 END
      END
   END Store;

   PROCEDURE StoreFloat(fr: FloatReg; VAR fat: Attribut);
   BEGIN
      WITH fat DO
	 CASE mode OF
	   globalMod:
               EmitFloatRX3Label(STD, fr, addrReg, addr2Reg, global, '+', addr);
               IF addrReg <> r0 THEN
                  FreeReg(addrReg);
               END;
               IF addr2Reg <> r0 THEN
                  FreeReg(addr2Reg);
               END;
	 | localMod:
               (* $R- *)
	       EmitFloatRX3Int(STD, fr, base, addrReg, addr-MCP4Block.offset);
               (* $R= *)
               IF addrReg <> r0 THEN
                  FreeReg(addrReg);
               END;
	 | addrLoadedMod:
	       EmitFloatRX3(STD, fr, addrReg, r0, addr);
	       IF mayRelease THEN
		  FreeReg(addrReg);
	       END;
         | indexMod:
               EmitFloatRX3(STD, fr, addrReg, addr2Reg, addr);
               FreeReg(addrReg);
               IF addr2Reg <> r0 THEN
                  FreeReg(addr2Reg);
               END;
	 | externalMod:
	       IF xelos THEN
		  EmitVarExtern(modPtr^.xidentifier, addr);
		  EmitFloatRX3Label(STD, fr, r0, r0,
		     modPtr^.xidentifier, 'V', addr);
	       ELSE
		  EmitRX3Label(L, r1, r0, r0, modPtr^.identifier, '-', adc);
		  EmitFloatRX3(STD, fr, r1, r0, addr);
	       END;
	 | absolutMod:
	       EmitFloatRX3(STD, fr, addrReg, addr2Reg, addr);
               IF addrReg <> r0 THEN
                  FreeReg(addrReg);
               END;
               IF addr2Reg <> r0 THEN
                  FreeReg(addr2Reg);
               END;
	 ELSE
	    CompilerError;
	 END
      END
   END StoreFloat;

END MCP4Store.
