IMPLEMENTATION MODULE SysPerror; (* AFB 2/84 *)

   FROM Errno IMPORT errno, EPERM, ENOENT, ESRCH, EINTR, EIO, ENXIO, E2BIG,
      ENOEXEC, EBADF, ECHILD, EAGAIN, ENOMEM, EACCES, EFAULT, ENOTBLK, EBUSY,
      EEXIST, EXDEV, ENODEV, ENOTDIR, EISDIR, EINVAL, ENFILE, EMFILE, ENOTTY,
      ETXTBSY, EFBIG, ENOSPC, ESPIPE, EROFS, EMLINK, EPIPE, EDOM, ERANGE,
      ENOMSG, EIDRM, EDEADLOCK;
   FROM StdIO IMPORT stderr;
   FROM FtdIO IMPORT FwriteString, FwriteLn;

   PROCEDURE Perror(str: ARRAY OF CHAR);
      VAR ErrMess: ARRAY[0..30] OF CHAR;
   BEGIN
      FwriteString(stderr, str);
      FwriteString(stderr, ": ");
      GetErrorString(errno, ErrMess);
      FwriteString(stderr, ErrMess);
      FwriteLn(stderr);
   END Perror;

   PROCEDURE GetErrorString(errno: CARDINAL; VAR buf: ARRAY OF CHAR);
      VAR str: ARRAY[0..30] OF CHAR;
          i: CARDINAL;
   BEGIN
      CASE errno OF
        0:  str := "#Error 0";
      | EPERM: str := "Not owner";
      | ENOENT : str := "No such file or directory";
      | ESRCH  : str := "No such process";
      | EINTR  : str := "Interrupted system call";
      | EIO    : str := "I/O error";
      | ENXIO  : str := "No such device or address";
      | E2BIG  : str := "Arg list too long";
      | ENOEXEC: str := "Exec format error";
      | EBADF  : str := "Bad file number";
      | ECHILD : str := "No children";
      | EAGAIN : str := "No more processes";
      | ENOMEM : str := "Not enough core";
      | EACCES : str := "Permission denied";
      | EFAULT : str := "Bad address";
      | ENOTBLK: str := "Block device required";
      | EBUSY  : str := "Mount device busy";
      | EEXIST : str := "File exists";
      | EXDEV  : str := "Cross-device link";
      | ENODEV : str := "No such device";
      | ENOTDIR: str := "Not a directory";
      | EISDIR : str := "Is a directory";
      | EINVAL : str := "Invalid argument";
      | ENFILE : str := "File table overflow";
      | EMFILE : str := "Too many open files";
      | ENOTTY : str := "Not a typewriter";
      | ETXTBSY: str := "Text file busy";
      | EFBIG  : str := "File too large";
      | ENOSPC : str := "No space left on device";
      | ESPIPE : str := "Illegal seek";
      | EROFS  : str := "Read-only file system";
      | EMLINK : str := "Too many links";
      | EPIPE  : str := "Broken pipe";
      | EDOM   : str := "Argument too large";
      | ERANGE : str := "Result too large";
      | ENOMSG : str := "No message of desired type";
      | EIDRM  : str := "Identifier Removed";
      | EDEADLOCK: str := "Deadlock Condition Detected";
      ELSE
	 str := "Unknown error";
      END; (* CASE *)
      i := 0;
      WHILE (i <= HIGH(buf)) AND (i <= HIGH(str)) AND (str[i] <> 0C) DO
         buf[i] := str[i];
         INC(i);
      END;
      IF i <= HIGH(buf) THEN
         buf[i] := 0C;
      END;
   END GetErrorString;

END SysPerror.
