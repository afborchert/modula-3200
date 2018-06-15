IMPLEMENTATION MODULE StdIO; (* AFB 2/84 *)
                             (* REV AFB 6/84: Fdopen *)

   (* the implementation isn't portable: integer must be long (32-bit) *)

   FROM SYSTEM IMPORT ADDRESS, ADR;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM Errno IMPORT errno, EINVAL;
   FROM SysCreat IMPORT Creat;
   FROM SysOpen IMPORT Open;
   FROM SysRead IMPORT Read;
   FROM SysWrite IMPORT Write;
   FROM SysClose IMPORT Close;
   FROM SysLseek IMPORT Lseek, Tell;
   FROM SysIoctl IMPORT Isatty;
   FROM SysExit IMPORT EnterCleanup;

   CONST
      BufSiz = 512;

   TYPE
      Flags = (reading, writing, error, eof);
      FlagsSet = SET OF Flags;
      FILE = POINTER TO FileStr;
      FileStr =
	 RECORD
	    fd: CARDINAL; (* file descriptor from UNIX *)
	    flags: FlagsSet;
            unget: BOOLEAN; (* if on: return ungetc on next call *)
            ungetc: CHAR;
	    CASE nobuf: BOOLEAN OF
	       FALSE:
		  cnt: CARDINAL;
		  index: [0..BufSiz-1];
		  buf: ARRAY [0..BufSiz-1] OF CHAR;
	    |  TRUE:
	    END;
	 END;

      Chain = POINTER TO ChainNode;
      ChainNode =
         RECORD
            f: FILE;
            link: Chain;
         END;

   (* see definition module

      MODE = (read, write, append);

   VAR
      stdin, stdout, stderr: FILE;

   *)

   VAR AllFiles: Chain;

   (* local procedures *)

   PROCEDURE Init;

      PROCEDURE Connect(VAR f: FILE; fd: CARDINAL; m: MODE);
      BEGIN
         (* stderr always unbuffered *)
         IF NOT Fdopen(f, fd, m, (fd <> 2) AND NOT Isatty(fd)) THEN
            f := NIL;
         END;
      END Connect;

   BEGIN
      AllFiles := NIL;
      Connect(stdin, 0, read);
      Connect(stdout, 1, write);
      Connect(stderr, 2, write);
   END Init;

   (* enter a file pointer into the chain *)

   PROCEDURE Enter(fp: FILE);
      VAR
         ptr: Chain;
   BEGIN
      NEW(ptr);
      WITH ptr^ DO
         f := fp;
         link := AllFiles;
      END;
      AllFiles := ptr;
   END Enter;

   PROCEDURE FillBuf(f: FILE) : BOOLEAN;
   BEGIN
      WITH f^ DO
	 cnt := BufSiz;
	 IF NOT Read(fd, ADR(buf), cnt) THEN
	    flags := flags + FlagsSet{error};
	    RETURN FALSE;
	 ELSIF cnt = 0 THEN
	    flags := flags + FlagsSet{eof};
            RETURN FALSE;
	 END;
         flags := flags - FlagsSet{eof, error};
         index := 0;
	 RETURN TRUE;
      END;
   END FillBuf;

   (* exported procedures *)

   PROCEDURE Fflush(f: FILE) : BOOLEAN;
   BEGIN
      WITH f^ DO
         IF NOT (writing IN flags) OR nobuf THEN RETURN FALSE END;
	 IF NOT Write(fd, ADR(buf), cnt) THEN
	    flags := flags + FlagsSet{error};
	    RETURN FALSE;
	 ELSE
	    cnt := 0;
            index := 0;
            flags := flags - FlagsSet{error};
	    RETURN TRUE;
	 END;
      END;
   END Fflush;

   PROCEDURE Fdopen(VAR f: FILE; filedesc: CARDINAL; mode: MODE;
                    buffered: BOOLEAN) : BOOLEAN;
   BEGIN
      IF buffered THEN
         NEW(f, FALSE);
      ELSE
         NEW(f, TRUE);
      END;
      WITH f^ DO
         nobuf := NOT buffered;
         unget := FALSE;
         fd := filedesc;
	 CASE mode OF
	   read:
	       flags := FlagsSet{reading};
	 | write:
	       flags := FlagsSet{writing};
	 | append:
               IF NOT Lseek(fd, 0, 2) THEN
                  IF nobuf THEN DISPOSE(f, TRUE); ELSE DISPOSE(f, FALSE); END;
		  RETURN FALSE;
	       END;
	       flags := FlagsSet{writing};
	 ELSE
            errno := EINVAL; (* invalid argument *)
            IF nobuf THEN DISPOSE(f, TRUE); ELSE DISPOSE(f, FALSE); END;
	    RETURN FALSE;
	 END; (* CASE *)
         IF buffered THEN
            cnt := 0;
            index := 0;
         END;
      END; (* WITH *)
      Enter(f);
      RETURN TRUE;
   END Fdopen;

   PROCEDURE Fopen(VAR f: FILE; name: ARRAY OF CHAR; mode: MODE;
		   buffered: BOOLEAN) : BOOLEAN;
      CONST CreateMode = 666B; (* rw-rw-rw- *)
      VAR fd: CARDINAL;
   BEGIN
      CASE mode OF
	read:
	    IF NOT Open(fd, name, (* read = *) 0) THEN
               RETURN FALSE;
	    END;
      | write:
	    IF NOT Creat(fd, name, CreateMode) THEN
               RETURN FALSE;
	    END;
      | append:
	    IF NOT Open(fd, name, (* write = *) 1) AND
               NOT Creat(fd, name, CreateMode) THEN
               RETURN FALSE;
	    END;
            (* lseek will be done by Fdopen *)
      ELSE
            errno := EINVAL; (* invalid argument *)
            RETURN FALSE;
	 END; (* CASE *)
      RETURN Fdopen(f, fd, mode, buffered);
   END Fopen;

   PROCEDURE CloseAll() : BOOLEAN;
      VAR ok: BOOLEAN;
          ptr: Chain;
   BEGIN
      ok := TRUE;
      WHILE AllFiles <> NIL DO
         WITH AllFiles^ DO (* that's no endless loop, see Fclose !!! *)
            ok := Fclose(f) AND ok;
         END;
      END;
      RETURN ok;
   END CloseAll;

   PROCEDURE Cleanup;
   BEGIN
      IF NOT CloseAll() THEN (* ignore flush errors *) END;
   END Cleanup;

   PROCEDURE Fclose(f: FILE) : BOOLEAN;
      VAR ok: BOOLEAN;
          prev, ptr: Chain;
   BEGIN
      IF f = NIL THEN RETURN FALSE END;
      (* look for the file pointer in the chain *)
      ptr := AllFiles;
      prev := NIL;
      LOOP
	 IF ptr = NIL THEN
	    errno := EINVAL; (* invalid argument *)
	    RETURN FALSE; (* f isn't a result from Fopen !!! *)
	 END;
	 IF ptr^.f = f THEN
	    EXIT;
	 END;
	 prev := ptr;
	 ptr := ptr^.link;
      END;
      (* flush the buffer and dispose the structure *)
      WITH f^ DO
	 IF NOT nobuf
            AND NOT(error IN flags) AND (writing IN flags) AND (cnt > 0) THEN
	       ok := Fflush(f);
         ELSE
               ok := TRUE;
	 END;
         ok := ok AND Close(fd);
         IF nobuf THEN
            DISPOSE(f, TRUE);
         ELSE
            DISPOSE(f, FALSE);
         END;
         IF prev = NIL THEN
            AllFiles := ptr^.link;
         ELSE
            prev^.link := ptr^.link;
         END;
         DISPOSE(ptr);
      END;
      RETURN ok;
   END Fclose;

   PROCEDURE Fread(ptr: ADDRESS; size: CARDINAL; VAR nitems: CARDINAL;
		   f: FILE) : BOOLEAN;
      TYPE TextPtr = POINTER TO ARRAY [0..BufSiz-1] OF CHAR;
      VAR text: TextPtr;
          bytecount: CARDINAL;
          ok: BOOLEAN;
          ch: CHAR;
   BEGIN
      WITH f^ DO
         IF nobuf THEN
            bytecount := size*nitems;
            IF unget AND (bytecount > 0) THEN
               text := TextPtr(ptr);
               text^[0] := ungetc;
               unget := FALSE;
               DEC(bytecount);
               ptr := ADDRESS(CARDINAL(ptr)+1);
            END;
            ok := Read(fd, ptr, bytecount);
            nitems := bytecount DIV size;
            RETURN ok;
         ELSE
            text := TextPtr(ptr);
            FOR bytecount := 0 TO size*nitems-1 DO
               IF NOT Fgetc(ch, f) THEN
                  nitems := bytecount DIV size;
                  RETURN FALSE;
               END;
               text^[bytecount] := ch;
            END;
            RETURN TRUE;
         END;
      END;
   END Fread;

   PROCEDURE Fwrite(ptr: ADDRESS; size: CARDINAL; VAR nitems: CARDINAL;
		    f: FILE) : BOOLEAN;
      TYPE TextPtr = POINTER TO ARRAY [0..BufSiz-1] OF CHAR;
      VAR text: TextPtr;
          bytecount: CARDINAL;
          ok: BOOLEAN;
   BEGIN
      WITH f^ DO
         IF nobuf THEN
            bytecount := size*nitems;
            ok := Write(fd, ptr, bytecount);
            IF NOT ok THEN
               nitems := bytecount DIV size;
            END;
            RETURN ok;
         ELSE
            text := TextPtr(ptr);
            FOR bytecount := 0 TO size*nitems-1 DO
               IF NOT Fputc(text^[bytecount], f) THEN
                  nitems := bytecount DIV size;
                  RETURN FALSE;
               END;
            END;
            RETURN TRUE;
         END;
      END;
   END Fwrite;

   PROCEDURE Fseek(f: FILE; off: INTEGER; whence: CARDINAL) : BOOLEAN;
      VAR pos: INTEGER;
   BEGIN
      WITH f^ DO
         flags := flags - FlagsSet{eof};
         IF NOT nobuf THEN (* seek in buffer ??? *)
            IF writing IN flags THEN
               IF NOT Fflush(f) THEN RETURN FALSE END;
            ELSIF whence = 0 THEN (* absolute seek *)
               IF NOT Ftell(f, pos) THEN RETURN FALSE END;
               IF pos - off = 0 THEN
                  RETURN TRUE;
               ELSIF (off - pos > 0) AND (off - pos < INTEGER(cnt)) THEN
                  DEC(cnt, CARDINAL(off-pos));
                  INC(index, CARDINAL(off-pos));
                  RETURN TRUE;
               ELSIF (off - pos < 0) AND (pos - off <= INTEGER(index)) THEN
                  INC(cnt, CARDINAL(pos-off));
                  DEC(index, CARDINAL(pos-off));
                  RETURN TRUE;
               END;
            ELSIF whence = 1 THEN (* relative seek *)
               IF off = 0 THEN
                  RETURN TRUE;
               ELSIF (off > 0) AND (off < INTEGER(cnt)) THEN
                  DEC(cnt, CARDINAL(off));
                  INC(index, CARDINAL(off));
                  RETURN TRUE;
               ELSIF (off < 0) AND (ABS(off) <= INTEGER(index)) THEN
                  INC(cnt, CARDINAL(off));
                  DEC(index, CARDINAL(off));
                  RETURN TRUE;
               ELSE (* calculate correct offset for Lseek *)
                  DEC(off, cnt);
               END;
            END;
            index := 0;
            cnt := 0;
         END;
         RETURN Lseek(fd, off, whence);
      END;
   END Fseek;

   PROCEDURE Ftell(f: FILE; VAR pos: INTEGER) : BOOLEAN;
   BEGIN
      WITH f^ DO
         IF NOT Tell(fd, pos) THEN RETURN FALSE END;
         IF NOT nobuf THEN
            IF reading IN flags THEN
               pos := pos - INTEGER(cnt);
            ELSE
               pos := pos + INTEGER(index);
            END;
         END;
      END;
      RETURN TRUE;
   END Ftell;

   PROCEDURE Ferror(f: FILE) : BOOLEAN;
   BEGIN
      RETURN error IN f^.flags;
   END Ferror;

   PROCEDURE Feof(f: FILE) : BOOLEAN;
   BEGIN
      RETURN eof IN f^.flags;
   END Feof;

   PROCEDURE Fgetc(VAR ch: CHAR; f: FILE) : BOOLEAN;
      VAR cha: ARRAY[0..0] OF CHAR;
	  (* ARRAY OF CHAR is packed, CHAR not !!! *)
	  ok: BOOLEAN;
          bytecount: CARDINAL;
   BEGIN
      bytecount := 1;
      WITH f^ DO
         IF unget THEN
            ch := ungetc;
            unget := FALSE;
            RETURN TRUE;
         END;
	 IF nobuf THEN
	    ok := Read(fd, ADR(cha), bytecount);
            IF bytecount = 0 THEN   (* eof ?? *)
               flags := flags + FlagsSet{eof};
               ok := FALSE;
               ch := 0C;
	    ELSIF ok THEN
	       ch := cha[0];
            END;
	 ELSE
	    IF cnt = 0 THEN
	       ok := FillBuf(f);
	    ELSE
	       ok := TRUE;
	    END;
            IF ok THEN
               DEC(cnt);
               INC(index);
               ch := buf[index-1];
	    ELSE
	       ch := 0C;
            END;
	 END;
      END;
      RETURN ok;
   END Fgetc;

   PROCEDURE Fputc(ch: CHAR; f: FILE) : BOOLEAN;
      VAR cha: ARRAY[0..0] OF CHAR;
          bytecount: CARDINAL;
   BEGIN
      WITH f^ DO
	 IF nobuf THEN
	    cha[0] := ch; (* pack ch *)
            bytecount := 1;
	    RETURN Write(fd, ADR(cha), bytecount);
	 ELSE
	    IF cnt = BufSiz THEN
	       IF NOT Fflush(f) THEN RETURN FALSE END;
	    END;
	    INC(cnt);
	    INC(index);
	    buf[index-1] := ch;
	    RETURN TRUE;
	 END;
      END;
   END Fputc;

   PROCEDURE Fungetc(ch: CHAR; f: FILE) : BOOLEAN;
   BEGIN
      WITH f^ DO
         IF nobuf THEN
            IF unget THEN RETURN FALSE END;
            ungetc := ch;
            unget := TRUE;
         ELSE
            IF cnt <> 0 THEN  (* if buffer isn't empty *)
               IF index > 0 THEN
                  DEC(index);
                  INC(cnt);
                  buf[index] := ch;
               ELSE
                  IF unget THEN RETURN FALSE END;
                  ungetc := ch;
                  unget := TRUE;
               END;
            ELSE
               buf[0] := ch;
               INC(cnt);
               index := 0;
            END;
         END;
      END;
      RETURN TRUE;
   END Fungetc;

BEGIN
   Init;
   EnterCleanup(Cleanup);
END StdIO.
