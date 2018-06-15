IMPLEMENTATION MODULE SysSetuid;

   FROM SYSTEM IMPORT UNIXCALL;
   FROM Sys IMPORT setuid, setgid;
   FROM Errno IMPORT errno;

   PROCEDURE Setuid(uid: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(setuid, r0, r1, uid) THEN
	 RETURN TRUE
      ELSE
	 errno := r0;
	 RETURN FALSE
      END;
   END Setuid;

   PROCEDURE Setgid(gid: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(setgid, r0, r1, gid) THEN
	 RETURN TRUE
      ELSE
	 errno := r0;
	 RETURN FALSE
      END;
   END Setgid;

END SysSetuid.
