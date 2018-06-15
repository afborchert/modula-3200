DEFINITION MODULE MCMnemonics; (* AFB 8/83 *)

   (*
   EXPORT QUALIFIED
      Mnemonic, MnemString, Mnem;
   *)

   TYPE
      Mnemonic = (A, AD, ADRR, AE, AER, AH, AHI, AHM, AI, AIS, AR, AM,
                  N, NH, NHI, NI, NR,
		  BAL, BALR, BFC, BFCR, BXH, BXLE, BTC, BTCR,
		  BC, BCR, BNC, BNCR, BE, BER, BNE, BNER,
                  BL, BLR, BNL, BNLR, BM, BMR, BNM, BNMR,
                  BP, BPR, BNP, BNPR, BO, BOR, BNO, BNOR,
                  BZ, BZR, BNZ, BNZER, B, BR,
		  C, CE, CER, CH, CHI, CI, CL, CLB, CLH, CLHI, CLI, CLR, CR,
		  CBT,
		  CD, CDR,
		  D, DD, DDR, DE, DER, DH, DHR, DR,
		  EXBR, EXHR,
		  FXDR, FLDR,
		  X, XH, XHI, XI, XR,
		  L, LA, LB, LBR, LCS, LD, LDR, LE, LM, LMD, LME, LER, LH,
		  LHI, LHL, LPDR, LCDR, LI, LIS, LR,
		  M, MD, MDR, ME, MER, MH, MHR, MR,
		  O, OH, OHI, OI, ORR, (* ORR = "OR" *)
		  RLL, RRL, SBT, RBT,
		  SLA, SLHA, SLHL, SLHLS, SLL, SLLS,
		  SRA, SRHA, SRHL, SRHLS, SRL, SRLS,
		  ST, STB, STBR, STD, STE, STM, STMD, STME, STH,
		  S, SD, SDR, SE, SER, SH, SHI, SI, SIS, SR,
		  SVC, TS, TBT, THI, TI,
		  MOVE, CPAN);

      MnemString = ARRAY [ 0..6 ] OF CHAR;

   VAR
      Mnem : ARRAY [ 0 .. 255 ] OF MnemString;

END MCMnemonics.
