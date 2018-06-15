IMPLEMENTATION MODULE PrioTab;

   FROM SystemTypes IMPORT Sig;

BEGIN
   PrioTab[SIGHUP] := 2;
   PrioTab[SIGINT] := 1;
   PrioTab[SIGQUIT] := 2;
   PrioTab[SIGILL] := 3;
   PrioTab[SIGTRAP] := 3;
   PrioTab[SIGIOT] := 3;
   PrioTab[SIGEMT] := 3;
   PrioTab[SIGFPE] := 3;
   PrioTab[SIGBUS] := 3;
   PrioTab[SIGSEGV] := 3;
   PrioTab[SIGSYS] := 3;
   PrioTab[SIGPIPE] := 2;
   PrioTab[SIGALRM] := 1;
   PrioTab[SIGTERM] := 2;
   PrioTab[SIGUSR1] := 1;
   PrioTab[SIGUSR2] := 1;
END PrioTab.
