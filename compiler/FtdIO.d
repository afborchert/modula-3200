DEFINITION MODULE FtdIO; (* stripped version for compiler *)

   FROM SYSTEM IMPORT WORD;
   FROM StdIO IMPORT FILE;

   VAR Done: BOOLEAN;

   PROCEDURE FwriteInt(f: FILE; int: INTEGER; w: CARDINAL);

   PROCEDURE FwriteCard(f: FILE; card: CARDINAL; w: CARDINAL);

   PROCEDURE FwriteString(f: FILE; str: ARRAY OF CHAR);

   PROCEDURE FwriteLn(f: FILE);

   PROCEDURE Fread(f: FILE; VAR arr: ARRAY OF WORD);

   PROCEDURE Fwrite(f: FILE; arr: ARRAY OF WORD);

   PROCEDURE FreadWord(f: FILE; VAR w: WORD);

   PROCEDURE FwriteWord(f: FILE; w: WORD);

   PROCEDURE FreadChar(f: FILE; VAR ch: CHAR);

   PROCEDURE FwriteChar(f: FILE; ch: CHAR);

END FtdIO.
