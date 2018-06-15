IMPLEMENTATION MODULE SysTime;

   FROM SYSTEM IMPORT UNIXCALL, ADR;
   FROM Sys IMPORT time;
   FROM Errno IMPORT errno;
   FROM SystemTypes IMPORT TIME;

   PROCEDURE Time(VAR t: TIME) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(time, r0, r1) THEN
	 t := TIME(r0);
	 RETURN TRUE;
      ELSE
	 errno := r0;
	 RETURN FALSE;
      END;
   END Time;

END SysTime.
