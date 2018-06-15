IMPLEMENTATION MODULE SysLockf;

   FROM Errno IMPORT errno;
   FROM Sys IMPORT lockf;
   FROM SystemTypes IMPORT OFF;
   FROM SYSTEM IMPORT UNIXCALL;

   PROCEDURE Lockf(fd: CARDINAL; function: LockFunction;
		   size: OFF) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(lockf, r0, r1, fd, function, size) THEN
         RETURN TRUE
      ELSE
         errno := r0;
         RETURN FALSE
      END;
   END Lockf;

END SysLockf.
