(* Modula-2 Library    -  UNIX System V  -     AFB 2/89 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
IMPLEMENTATION MODULE Directories;

   FROM FtdIO IMPORT Fread, Done;
   FROM StdIO IMPORT FILE, read, Fopen, Fseek, Ftell, Fclose;
   FROM SystemTypes IMPORT DirSize, OFF;

   (* (* exported from definition module *)
   TYPE 
      DIR;

      FileName = ARRAY [0..DirSize-1] OF CHAR;
      Direct = 
         RECORD 
            ino: CARDINAL;
            name: FileName;
         END;
   *)
   CONST
      dirsize = 16;	(* size of a directory entry *)
   TYPE
      DIR = FILE;

   PROCEDURE OpenDir(VAR dirp: DIR; filename: ARRAY OF CHAR) : BOOLEAN;
   BEGIN
      RETURN Fopen(dirp, filename, read, (* buffered = *) TRUE)
   END OpenDir;

   PROCEDURE ReadDir(dirp: DIR; VAR direct: Direct) : BOOLEAN;
      VAR
	 dirbuf: ARRAY [0..dirsize-1] OF CHAR;
	 inode: CARDINAL;
	 index: CARDINAL;
   BEGIN
      REPEAT
	 Fread(dirp, dirbuf);
	 IF ~Done THEN RETURN FALSE END;
	 inode := ORD(dirbuf[0]) * 100H + ORD(dirbuf[1]);
      UNTIL inode # 0;
      WITH direct DO
	 ino := inode;
	 FOR index := 2 TO HIGH(dirbuf) DO
	    name[index-2] := dirbuf[index];
	 END;
      END;
      RETURN TRUE
   END ReadDir;

   PROCEDURE TellDir(dirp: DIR; VAR offset: OFF) : BOOLEAN;
   BEGIN
      RETURN Ftell(dirp, offset)
   END TellDir;

   PROCEDURE SeekDir(dirp: DIR; offset: OFF) : BOOLEAN;
   BEGIN
      RETURN (offset >= 0) & (offset MOD dirsize = 0) & Fseek(dirp, offset, 0)
   END SeekDir;

   PROCEDURE RewindDir(dirp: DIR) : BOOLEAN;
   BEGIN
      RETURN Fseek(dirp, 0, 0)
   END RewindDir;

   PROCEDURE CloseDir(VAR dirp: DIR);
   BEGIN
      IF ~Fclose(dirp) THEN END;
   END CloseDir;

END Directories.
