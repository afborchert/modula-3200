IMPLEMENTATION MODULE SysIoctl;

   FROM SYSTEM IMPORT UNIXCALL, ADR, WORD;
   FROM Errno IMPORT errno;
   FROM Sys IMPORT ioctl;

   (* (* from definition module *)
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
      Alldelay = Bsdelay + Vtdelay + Crdelay + Xtabs + Tbdelay + Nldelay;

   TYPE

      RequestType = (getd, setd, hpcl, modg, mods, unused5, unused6, unused7,
        getp, setp, setn, unused11, unused12, excl, nxcl, mode,flush, setc,
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

   *)

   PROCEDURE Ioctl(fd: CARDINAL; request: RequestType;
                   VAR argp: ARRAY OF WORD) : BOOLEAN;
      CONST
         rch = 't';
      VAR r0, r1: CARDINAL;
          requ: BITSET;
   BEGIN
      requ := BITSET(ORD(rch) * 400B) + BITSET(request);
      IF NOT UNIXCALL(ioctl, r0, r1, fd, requ, ADR(argp)) THEN
         errno := r0;
         RETURN FALSE;
      ELSE
         RETURN TRUE;
      END;
   END Ioctl;

   PROCEDURE Stty(fd: CARDINAL; argp: Sgttyb) : BOOLEAN;
   BEGIN
      RETURN Ioctl(fd, setp, argp);
   END Stty;

   PROCEDURE Gtty(fd: CARDINAL; VAR argp: Sgttyb) : BOOLEAN;
   BEGIN
      RETURN Ioctl(fd, getp, argp);
   END Gtty;

   PROCEDURE Isatty(fd: CARDINAL) : BOOLEAN;
      VAR ttyb: Sgttyb;
   BEGIN
      RETURN Gtty(fd, ttyb);
   END Isatty;

END SysIoctl.
