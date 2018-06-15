IMPLEMENTATION MODULE SysPause;

   FROM Sys IMPORT pause;
   FROM SYSTEM IMPORT UNIXCALL;

   PROCEDURE Pause;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(pause, r0, r1) THEN
         (* ignore result *)
      END;
   END Pause;

END SysPause.
