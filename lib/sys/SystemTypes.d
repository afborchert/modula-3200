(* Modula-2 Library    -  UNIX System V  -     AFB 9/88 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
DEFINITION MODULE SystemTypes; (* and constants *)

   (* see...
	/usr/include/fcntl.h
	/usr/include/signal.h
	/usr/include/sys/dir.h
	/usr/include/sys/param.h
	/usr/include/sys/types.h
   *)

   CONST
      DirSize = 14;
      MaxOpenFiles = 40;
      (* file control options; arguments of fcntl(2) and open(2) *)
      rdonly = {};
      wronly = { 31 };
      rdwr = { 30 };
      ndelay = { 29 };
      append = { 28 };
      creat = { 23 };
      trunc = { 22 };
      excl = { 21 };
   TYPE
      Sig = (SIGRTI, SIGHUP, SIGINT, SIGQUIT, SIGILL,
             SIGTRAP, SIGIOT, SIGEMT, SIGFPE, SIGKILL,
	     SIGBUS, SIGSEGV, SIGSYS, SIGPIPE,
             SIGALRM, SIGTERM, SIGUSR1, SIGUSR2,
	     SIGCLD, SIGPWR);

      ProcessId = INTEGER;	(* ProcessId may be -1 for kill *)
      TIME = LONGINT;
      OFF = LONGINT;		(* offset/size of files *)

END SystemTypes.
