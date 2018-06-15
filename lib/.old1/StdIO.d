DEFINITION MODULE StdIO; (* AFB 1/84 *)

   FROM SYSTEM IMPORT ADDRESS;

   TYPE
      FILE; (* hidden *)
      MODE = (read, write, append);

   VAR
      stdin, stdout, stderr: FILE;

   (* all functions return FALSE in error case *)

   PROCEDURE Fopen(VAR f: FILE; name: ARRAY OF CHAR; mode: MODE;
		   buffered: BOOLEAN) : BOOLEAN;

   PROCEDURE Fclose(f: FILE) : BOOLEAN;

   PROCEDURE Fread(ptr: ADDRESS; size: CARDINAL; VAR nitems: CARDINAL;
		   f: FILE) : BOOLEAN;

   PROCEDURE Fwrite(ptr: ADDRESS; size: CARDINAL; VAR nitems: CARDINAL;
		    f: FILE) : BOOLEAN;

   PROCEDURE Fseek(f: FILE; offset: INTEGER; whence: CARDINAL) : BOOLEAN;

   PROCEDURE Ftell(f: FILE; VAR pos: INTEGER) : BOOLEAN;

   PROCEDURE Feof(f: FILE) : BOOLEAN;

   PROCEDURE Ferror(f: FILE) : BOOLEAN;

   PROCEDURE Fgetc(VAR ch: CHAR; f: FILE) : BOOLEAN;

   PROCEDURE Fputc(ch: CHAR; f: FILE) : BOOLEAN;

   PROCEDURE Fungetc(ch: CHAR; f: FILE) : BOOLEAN;

   PROCEDURE CloseAll() : BOOLEAN;

   PROCEDURE Fflush(f: FILE) : BOOLEAN;

   PROCEDURE Fdopen(VAR f: FILE; fd: CARDINAL; mode: MODE;
                    buffered: BOOLEAN) : BOOLEAN;

END StdIO.
