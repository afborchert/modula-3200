IMPLEMENTATION MODULE RandomGenerator;

   FROM SYSTEM IMPORT ADR, UNIXCALL;
   FROM Sys IMPORT getpid, times;
   FROM SysTime IMPORT Time;
   FROM SystemTypes IMPORT TIME;

   CONST 
      modulo = 65536;
      add    = 13;
      mult   = 117;

   VAR 
      randzahl : INTEGER;

   PROCEDURE RandomInit;
      VAR seed: TIME;

      PROCEDURE Times(): INTEGER;
	 TYPE Buf = RECORD u,s,cu,cs: CARDINAL END;
	 VAR buf: Buf; r0, r1: INTEGER;
      BEGIN
	 IF NOT UNIXCALL(times,r0,r1,ADR(buf)) THEN END;
	 RETURN r0
      END Times;

      PROCEDURE GetPid(): INTEGER;
	 VAR r0, r1: INTEGER;
      BEGIN
	 IF NOT UNIXCALL(getpid, r0, r1) THEN END;
	 RETURN r0 + r1
      END GetPid;

   BEGIN 
      IF NOT Time(seed) THEN
	 randzahl := Times();
      ELSE
	 randzahl := INTEGER(seed) * GetPid() + Times();
      END;
      randzahl := ABS(randzahl) MOD modulo 
   END RandomInit;

   PROCEDURE Random(a, b: INTEGER) : INTEGER;
   BEGIN 
      randzahl := (mult*(randzahl+add)) MOD modulo;
      RETURN a + ((b + 1 - a)*randzahl) DIV modulo;
   END Random;

BEGIN
   RandomInit;
END RandomGenerator. 
