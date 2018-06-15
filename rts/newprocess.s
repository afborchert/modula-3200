*
*	modula-2 library -- newprocess
*
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
include(equ.s)
include(process.s)
*
param	struc
P	dsf	1
A	dsf	1
n	dsf	1
new	dsf	1
	ends
*
*
	entry	.newproc
	extrn	.stack
	pure
	align	adc
.newproc equ	*
	lhi	r0,pd_size
	c	r0,-param+n(top)
	bp	.stack			Stack too small ???
*
	l	r1,-param+A(top)	new := PROCESS(A);
	st	r1,pd_top(r1)		new^.top := A;
	am	r0,pd_top(r1)		INC(new^.top, TSIZE(ProcessDesc));
	lis	r0,0
	st	r0,pd_base(r1)		new^.base := 0;
	l	r0,-param+n(top)
	ar	r0,r1
	st	r0,pd_limit(r1)		new^.limit := A+n;
	l	r0,-param+P(top)
	st	r0,pd_p(r1)		new^.p := P;
	lis	r0,0
	st	r0,pd_start(r1)		new^.started := FALSE;
*
	lr	r0,r1
	l	r1,-param+new(top)
	st	r0,0(r1)		return VAR new: PROCESS
*
	shi	top,param
	br	rf
*
	end
