IMPLEMENTATION MODULE SysStat; (* XELOS *)

   FROM SystemTypes IMPORT TIME, OFF;
   FROM UnixString IMPORT Copy, Buffer;
   FROM Sys IMPORT stat, fstat;
   FROM Errno IMPORT errno;
   FROM SYSTEM IMPORT UNIXCALL, ADR;

   (* (* from definition module *)
   TYPE
      StatBuf =
         RECORD
            dev: CARDINAL;
            ino: CARDINAL;
            mode: BITSET;
            nlink: CARDINAL;
            uid: CARDINAL;
            gid: CARDINAL;
            rdev: CARDINAL;
            size: OFF;
            atime: TIME;
            mtime: TIME;
            ctime: TIME;
         END;
   CONST
      (* bit masks for mode; bits 0..15 used *)
      FileType = { 0..3 };
      (* IF mask * FileType = ... *)
      IfDir = { 1 };      (* directory *)
      IfChr = { 2 };      (* character special *)
      IfBlk = { 1..2 };   (* block special *)
      IfReg = { 0 };      (* regular *)
      IfMpc = { 2..3 };   (* multiplexed char special *)
      IfMpb = { 1..3 };   (* multiplexed block special *)
      (* IF ... <= mask THEN *)
      IsUid =  { 4 };     (* set user id on execution *)
      IsGid =  { 5 };     (* set group id on execution *)
      IsVtx =  { 6 };     (* save swapped text even after use *)
      (* permissions on file *)
      OwnerRead = { 7 };  (* read permission, owner *)
      OwnerWrite = { 8 }; (* write permission, owner *)
      OwnerExec = { 9 };  (* execute/search permission, owner *)
      GroupRead = { 10 };
      GroupWrite = { 11 };
      GroupExec = { 12 };
      WorldRead = { 13 };
      WorldWrite = { 14 };
      WorldExec = { 15 };
   *)

   TYPE
      StructStat = (* C-compatible structure *)
         RECORD
            dev1, dev2: CHAR;
            ino1, ino2: CHAR;
            mode1, mode2: CHAR;
            nlink1, nlink2: CHAR;
            uid1, uid2: CHAR;
            gid1, gid2: CHAR;
            rdev1, rdev2: CHAR;
            size: LONGCARD;
            atime, mtime, ctime: TIME;
         END;

   PROCEDURE Expand(VAR from: StructStat; VAR to: StatBuf);

      PROCEDURE Convert(ch1, ch2: CHAR; VAR value: CARDINAL);
      BEGIN
         value := ORD(ch1) * 100H + ORD(ch2);
      END Convert;

   BEGIN
      WITH from DO
         WITH to DO
            Convert(dev1, dev2, dev);
            Convert(ino1, ino2, ino);
            mode := BITSET(ORD(mode1) * 1000000H + ORD(mode2) * 10000H);
            Convert(nlink1, nlink2, nlink);
            Convert(uid1, uid2, uid);
            Convert(gid1, gid2, gid);
            Convert(rdev1, rdev2, rdev);
         END;
         to.size := size;
         to.atime := atime;
         to.mtime := mtime;
         to.ctime := ctime;
      END;
   END Expand;

   PROCEDURE Stat(file: ARRAY OF CHAR; VAR buf: StatBuf) : BOOLEAN;
      VAR sb: StructStat; r0, r1: CARDINAL; Buf: Buffer;
   BEGIN
      Copy(Buf, file);
      IF UNIXCALL(stat, r0, r1, ADR(Buf), ADR(sb)) THEN
         Expand(sb, buf);
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END Stat;

   PROCEDURE Fstat(fd: CARDINAL; VAR buf: StatBuf) : BOOLEAN;
      VAR sb: StructStat; r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(fstat, r0, r1, fd, ADR(sb)) THEN
         Expand(sb, buf);
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END Fstat;

END SysStat.
