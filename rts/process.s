*	TYPE
*	   PROCESS = POINTER TO ProcessDesc;
*	   ProcessDesc =
*	      RECORD
*		 base, top, limit: ADDRESS;
*		 p: PROC;
*		 started: BOOLEAN;
*	      END;
*
*	PROCEDURE NEWPROCESS(P: PROC;
*                            A: ADDRESS;
*                            n: CARDINAL;
*                            VAR new: PROCESS);
*
pd_size	struc		TYPE ProcessDesc
pd_base	dsf	1
pd_top	dsf	1
pd_limit dsf	1
pd_p	dsf	1
pd_start dsf	1
	ends
