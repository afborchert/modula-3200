IMPLEMENTATION MODULE SysGetuid;

   FROM SYSTEM IMPORT UNIXCALL;
   FROM Sys IMPORT getuid, geteuid, getgid, getegid;

   PROCEDURE Getuid() : CARDINAL;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(getuid, r0, r1) THEN END;
      RETURN r0
   END Getuid;

   PROCEDURE Geteuid() : CARDINAL;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(geteuid, r0, r1) THEN END;
      RETURN r1
   END Geteuid;

   PROCEDURE Getgid() : CARDINAL;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(getgid, r0, r1) THEN END;
      RETURN r0
   END Getgid;

   PROCEDURE Getegid() : CARDINAL;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(getegid, r0, r1) THEN END;
      RETURN r1
   END Getegid;

END SysGetuid.
