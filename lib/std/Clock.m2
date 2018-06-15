IMPLEMENTATION MODULE Clock;

   FROM SYSTEM	IMPORT ADR, UNIXCALL;
   FROM Sys	IMPORT times;

   TYPE Buf = RECORD u,s,cu,cs: CARDINAL END;

   VAR
      ignore,
      real,
      cpu:  CARDINAL;

   PROCEDURE Times(VAR buf: Buf): CARDINAL;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF ~UNIXCALL(times,r0,r1,ADR(buf)) THEN END;
      RETURN r0
   END Times;

   PROCEDURE RealTime(reset: BOOLEAN): CARDINAL;
      VAR
	 result: CARDINAL;
	 buf:	 Buf;
   BEGIN
      result := Times(buf) - real;
      IF reset THEN
	 INC(real,result)
      END;
      RETURN result
   END RealTime;

   PROCEDURE CPUTime (reset: BOOLEAN): CARDINAL;
      VAR
	 result: CARDINAL;
	 buf:	 Buf;
   BEGIN
      ignore := Times(buf);
      WITH buf DO
	 result := u + s + cu + cs - cpu;
      END;
      IF reset THEN
	 INC(cpu,result)
      END;
      RETURN result
   END CPUTime;

BEGIN
   ignore := RealTime(TRUE);
   cpu := 0;
   DEC(real, CPUTime(FALSE) );
END Clock.
