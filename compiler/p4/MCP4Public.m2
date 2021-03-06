(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for ENDLICH        *
*                                       *
*     running on ENDLICH                *
*                                       *
*     MCP4Public:                       *
*                                       * 
*     public part of the common base    *
*     of the Modula-2 compiler          *
*     and argument handling             *
*                                       * 
****************************************)

IMPLEMENTATION MODULE MCP4Public;   (* AFB 3/84 *)

   FROM SysPerror IMPORT Perror;
   FROM MCStop IMPORT Stop;
   FROM MCBase IMPORT loading, xelos;

   FROM StdIO IMPORT
      FILE, Fopen, Fputc, stderr;
   FROM FtdIO IMPORT
      FwriteLn, FwriteString;

   PROCEDURE WorkupArguments;
      VAR buf: FileName;
          index: CARDINAL;
          start: CARDINAL;
          ign: BOOLEAN;
	  i: CARDINAL;

      PROCEDURE Usage;
      BEGIN
         FwriteString(stderr, "Usage: ");
	 ARGV(buf, 0);
	 FwriteString(stderr, buf);
	 IF xelos THEN
	    FwriteString(stderr,
	       " storage [ -flags ] sourcefilename il1 out [ il2 ]");
	 ELSE
	    FwriteString(stderr, " storage [ -flags ] il1 out [ il2 ]");
	 END;
         FwriteLn(stderr);
         Stop(1);
      END Usage;

   BEGIN (* WorkupArguments *)
      IF ARGC() < 4 THEN Usage END;
      IF xelos AND (ARGC() = 4) THEN Usage END;

      (* look for flags *)

      ARGV(buf, 2);
      IF buf[0] = '-' THEN
         IF ARGC() < 5 THEN Usage END;
         index := 1;
         WHILE (buf[index] <> 0C) AND (index <= HIGH(buf)) DO
            CASE buf[index] OF
              'p' : (* profiling *)
                  profile := TRUE;
            | 'S' : (* nice output *)
                  Sflag := TRUE;
            | 'l', 'L' : (* produce line number labels *)
                  lflag := TRUE;
	    | 'R' : (* no range checks *)
		  Rflag := TRUE;
	    | 's' : (* no stack checks *)
		  sflag := TRUE;
            ELSE
               FwriteString(stderr, "m3: Unknown flag: ");
               ign := Fputc(buf[index], stderr);
               FwriteLn(stderr);
               Usage;
            END;
            INC(index);
         END;
         start := 3;
      ELSE
         start := 2;
      END;
      IF xelos THEN
	 ARGV(sourcefilename, start);
	 INC(start);
      END;

      FOR index := start TO ARGC()-1 DO
         ARGV(buf, index);
         CASE index-start OF
         | 0 : (* il1 *)
	       il1Name := buf;
         | 1 : (* out *)
	       assName := buf;
         | 2 : (* error file *)
               errName := buf;
               errFile := TRUE;
         ELSE
            Usage;
         END;
      END;
   END WorkupArguments;

BEGIN
   IF loading THEN Stop(0) END;
   ErrorsFound := FALSE;
   profile := FALSE;
   Sflag := FALSE;
   lflag := FALSE;
   errFile := FALSE;
   sflag := FALSE;
   Rflag := FALSE;
   WorkupArguments;
END MCP4Public.
