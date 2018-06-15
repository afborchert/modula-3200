DEFINITION MODULE SysKill;

   FROM SystemTypes IMPORT Sig, ProcessId;

   PROCEDURE Kill(pid: ProcessId; sig: Sig) : BOOLEAN;

END SysKill.
