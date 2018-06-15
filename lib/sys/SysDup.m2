IMPLEMENTATION MODULE SysDup;

   FROM Sys IMPORT dup;
   FROM Errno IMPORT errno, EBADF;
   FROM SYSTEM IMPORT UNIXCALL;
   FROM SysClose IMPORT Close;
   FROM SysFcntl IMPORT Fcntl, dupfd;

   PROCEDURE Dup(fd: CARDINAL; VAR newfd: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(dup, r0, r1, fd) THEN
         newfd := r0;
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END Dup;

   PROCEDURE Dup2(fd, newfd: CARDINAL) : BOOLEAN;
      VAR fd2: CARDINAL;
   BEGIN
      fd2 := newfd;
      IF NOT Close(fd2) THEN END;
      IF Fcntl(fd, dupfd, fd2) THEN
	 IF fd2 = newfd THEN
	    RETURN TRUE;
	 ELSE
	    errno := EBADF;
	    IF NOT Close(fd2) THEN END;
	    RETURN FALSE;
	 END;
      ELSE
	 RETURN FALSE;
      END;
   END Dup2;

END SysDup.
