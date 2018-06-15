DEFINITION MODULE SysFcntl;

   FROM SYSTEM IMPORT WORD;

   TYPE
      FcntlRequest = (dupfd, getfd, setfd, getfl, setfl);

   PROCEDURE Fcntl(fd: CARDINAL; cmd: FcntlRequest; VAR arg: WORD) : BOOLEAN;

END SysFcntl.
