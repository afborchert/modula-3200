DEFINITION MODULE SysIoctl;

   FROM SYSTEM IMPORT WORD;

   CONST
      Tandem = { 31 };
      Cbreak = { 30 };
      Lcase  = { 29 };
      Echo   = { 28 };
      Crmod  = { 27 };
      Raw    = { 26 };
      Oddp   = { 25 };
      Evenp  = { 24 };
      Anyp   = Oddp + Evenp;
      Nldelay = { 22 , 23 };
      Tbdelay = { 20 , 21 };
      Xtabs  = { 20 , 21 };
      Crdelay = { 18 , 19 };
      Vtdelay = { 17 };
      Bsdelay = { 16 };
      Alldelay = Bsdelay + Vtdelay + Crdelay +
                 Xtabs + Tbdelay + Nldelay;

   TYPE

      RequestType = (getd, setd, hpcl, modg, mods,
        unused5, unused6, unused7, getp, setp, setn,
	unused11, unused12, excl, nxcl, mode,flush, setc,
        getc, empty);

      Sgttyb =
         RECORD
            ispeed: CHAR;
            ospeed: CHAR;
            erase: CHAR;
            kill: CHAR;
            flags: BITSET;
         END;

      Tchars =
         RECORD
            intrc: CHAR;
            quitc: CHAR;
            startc: CHAR;
            stopc: CHAR;
            eofc: CHAR;
            brkc: CHAR;
         END;

   PROCEDURE Ioctl(fd: CARDINAL; request: RequestType;
                   VAR argp: ARRAY OF WORD) : BOOLEAN;

   PROCEDURE Stty(fd: CARDINAL; argp: Sgttyb) : BOOLEAN;

   PROCEDURE Gtty(fd: CARDINAL; VAR argp: Sgttyb) : BOOLEAN;

   PROCEDURE Isatty(fd: CARDINAL) : BOOLEAN;

END SysIoctl.
