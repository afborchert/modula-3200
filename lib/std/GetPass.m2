IMPLEMENTATION MODULE GetPass; (* AFB and MH *)

   FROM SysTermIO IMPORT echo,icanon,vmin,vtime,TermIO, GetTermIO,SetTermIO;
   FROM SysOpen IMPORT Open;
   FROM FtdIO IMPORT FreadChar, FwriteChar, FwriteString, FwriteLn;
   FROM ASCII IMPORT nl, nak, bs;
   FROM RandomGenerator IMPORT Random;
   FROM StdIO IMPORT FILE, stdin, stdout, Fdopen, Fclose, read, write;

   PROCEDURE GetPass(prompt: ARRAY OF CHAR; VAR passwd: ARRAY OF CHAR);
      CONST
         MaxPasswdLen = 8;			(* as in getpass(3C) *)
	 Terminal = "/dev/tty";
	 Read = 0;
	 Write = 1;
      VAR
         index: CARDINAL; (* in passwd *)
         oldterm, term: TermIO;
         ch: CHAR; (* last character read *)
	 termin, termout: FILE;
	 termfdin: CARDINAL; (* file descriptor of terminal input *)
	 termfdout: CARDINAL;
   BEGIN
      (* As long as no function Isatty() exists we have to go sure that
       * the password comes from a terminal by opening /dev/tty in any case.
       *)
      IF    NOT Open(termfdin, Terminal, Read) OR
	    NOT Open(termfdout, Terminal, Write) OR
	    NOT Fdopen(termin, termfdin, read, (* buffered = *) FALSE) OR
	    NOT Fdopen(termout, termfdout, write, (* buffered = *) FALSE) THEN
	 RETURN
      END;
      FwriteString(termout, prompt);
      IF NOT GetTermIO(termfdin, term) THEN RETURN END;
      oldterm := term;
      WITH term DO
         linemodes := linemodes - echo - icanon;
	 cc[vmin] := 1C; cc[vtime] := 0C
      END;
      IF NOT SetTermIO(termfdin, term) THEN RETURN END;
      index := 0;
      REPEAT
         FreadChar(termin, ch);
         CASE ch OF
         | nl: FwriteChar(termout, ch);
         | bs:
             IF (index > 0) THEN
                DEC(index);
             END;
         | nak: (* ^U *)
             index := 0;
         ELSE			(* count letters beyond boundary because
				 * of possibly following backspaces!
				 *)
	     IF (index <= HIGH(passwd)) & (index <= MaxPasswdLen) THEN
		passwd[index] := ch;
	     END;
             INC(index);
         END;
      UNTIL (ch = nl);			(* ... same as in getpass(3C) *)
      IF index <= HIGH(passwd) THEN
         passwd[index] := 0C;
      END;
      IF MaxPasswdLen <= HIGH(passwd) THEN
	 passwd[MaxPasswdLen] := 0C;
      END;
      IF NOT SetTermIO(termfdin, oldterm) THEN RETURN END;
      IF Fclose(termin) AND Fclose(termout) THEN END;
   END GetPass;

END GetPass.
