
IMPLEMENTATION MODULE Bytes;

   FROM SYSTEM IMPORT ADDRESS, BYTE, WORD;

   PROCEDURE PINC(VAR add : ADDRESS; inc : CARDINAL);

   BEGIN
      INC(add,inc);
   END PINC;

   PROCEDURE PDEC(VAR add : ADDRESS; dec : CARDINAL);

   BEGIN
      DEC(add,dec);
   END PDEC;

   PROCEDURE ByteNCopy(to,from : ADDRESS; no : CARDINAL);

   BEGIN
      to := to;
      from := from;
      no := no;
   END ByteNCopy;

END Bytes.
