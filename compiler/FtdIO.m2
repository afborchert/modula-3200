IMPLEMENTATION MODULE FtdIO; (* stripped version for compiler *)

   FROM SYSTEM IMPORT WORD, ADR;
   FROM StdIO IMPORT FILE, Fputc, Fgetc;
   FROM ReadIntCard IMPORT Read, Type;
   FROM Conversions IMPORT ConvertInteger, ConvertCardinal;
   IMPORT ReadIntCard, StdIO;

   (* (* from definition module *)
   VAR Done: BOOLEAN;
   *)
   CONST
      nl = 12C;
      tab = 11C;

   VAR
      fp: FILE;

   PROCEDURE FwriteInt(f: FILE; arg: INTEGER; w: CARDINAL);
      VAR field: ARRAY[0..10] OF CHAR;
   BEGIN
      ConvertInteger(arg, w, field);
      FwriteString(f, field);
   END FwriteInt;

   PROCEDURE FwriteCard(f: FILE; arg: CARDINAL; w: CARDINAL);
      VAR field: ARRAY[0..10] OF CHAR;
   BEGIN
      ConvertCardinal(arg, w, field);
      FwriteString(f, field);
   END FwriteCard;

   PROCEDURE FwriteString(f: FILE; str: ARRAY OF CHAR);
      VAR cnt: CARDINAL;
   BEGIN
      cnt := 0;
      Done := TRUE;
      WHILE (cnt <= HIGH(str)) AND (str[cnt] <> 0C) DO
         Done := Done AND StdIO.Fputc(str[cnt], f);
         INC(cnt);
      END;
   END FwriteString;

   PROCEDURE FwriteLn(f: FILE);
   BEGIN
      Done := Fputc(nl, f);
   END FwriteLn;

   PROCEDURE Fread(f: FILE; VAR arr: ARRAY OF WORD);
      VAR cnt: CARDINAL;
   BEGIN
      cnt := 1;
      Done := StdIO.Fread(ADR(arr), SIZE(arr), cnt, f) AND (cnt = 1);
   END Fread;

   PROCEDURE Fwrite(f: FILE; arr: ARRAY OF WORD);
      VAR cnt: CARDINAL;
   BEGIN
      cnt := 1;
      Done := StdIO.Fwrite(ADR(arr), SIZE(arr), cnt, f) AND (cnt = 1);
   END Fwrite;

   PROCEDURE FreadWord(f: FILE; VAR w: WORD);
      VAR cnt: CARDINAL;
   BEGIN
      cnt := 1;
      Done := StdIO.Fread(ADR(w), SIZE(w), cnt, f) AND (cnt = 1);
   END FreadWord;

   PROCEDURE FwriteWord(f: FILE; w: WORD);
      VAR cnt: CARDINAL;
   BEGIN
      cnt := 1;
      Done := StdIO.Fwrite(ADR(w), SIZE(w), cnt, f) AND (cnt = 1);
   END FwriteWord;

   PROCEDURE FreadChar(f: FILE; VAR ch: CHAR);
   BEGIN
      Done := Fgetc(ch, f);
   END FreadChar;

   PROCEDURE FwriteChar(f: FILE; ch: CHAR);
   BEGIN
      Done := Fputc(ch, f);
   END FwriteChar;

END FtdIO.
