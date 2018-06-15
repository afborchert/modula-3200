DEFINITION MODULE SysLockf;

   FROM SystemTypes IMPORT OFF;

   TYPE
      LockFunction = (unlock, lock, testandlock, test);

   PROCEDURE Lockf(fd: CARDINAL; function: LockFunction;
		   size: OFF) : BOOLEAN;

END SysLockf.
