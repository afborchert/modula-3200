IMPLEMENTATION MODULE SysAlarm;

   FROM SYSTEM IMPORT UNIXCALL;
   FROM Sys IMPORT alarm;
   FROM Errno IMPORT errno;

   PROCEDURE Alarm(sec: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(alarm, r0, r1, sec) THEN
         previous := r0;
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END Alarm;

END SysAlarm.
