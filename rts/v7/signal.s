*
*	modula-2 runtime library -- signal
*
*	ref to libc/sys/signal.s
*
	entry	.signal
	extrn	.stack
*
*	PROCEDURE Signal(sig: CARDINAL; p: PROC; VAR old: PROC;
*	                 VAR error: CARDINAL) : BOOLEAN;
*
p_size	struc
p_sig	dsf	1
p_p	dsf	1
p_old	dsf	1
p_error	dsf	1
	ends
*
nsig	equ	16	* add to brtab if this constant changes *
EINVAL	equ	22
	pure
.signal	equ	*
*
* save previous signal value
*
	l	r1,-p_size+p_sig(top)	sig
	bnp	illsig			must be +ve
	chi	r1,nsig			check range
	bp	illsig			too high
	lr	r0,r1			save sig number
	slls	r1,2			offset in sig table
	l	r5,sigtab-adc(r1)	previous value
	l	r9,-p_size+p_old(top)	load address of "old: PROC"
*
* if proc is nonzero and even, it's a procedure address
*
	l	r6,-p_size+p_p(top)	proc
	st	r6,sigtab-adc(r1)	set into sig table
	bz	nofunc			zero - not an address
	ni	r6,1			check low bit
	bnz	nofunc			odd - not an address
*
* replace function by branch table address so regs can be saved
*
	lr	r8,r1			signo * 4
	slls	r8,2			offset in branch table
	la	r6,brtab-16(r8)		branch table address
nofunc	equ	*
*
* call signal svc
*
	st	r0,svc.no		set signal number
	st	r6,svc.fn		set procedure address
	svc	0,0			* signal *
	dc	a(svcsig)
	bc	sigerr			carry bit -- error
*
* return previous signal value
*
	lis	r8,1			check low bit
	nr	r8,r0
	bz	notodd
	st	r8,0(r9)		odd - signal previously ignored
	lis	r0,1			return TRUE
	b	return
notodd	equ	*
	st	r5,0(r9)		store actual proc address
	lis	r0,1			return TRUE
	b	return
*
* error exits
*
illsig	equ	*
	lhi	r0,EINVAL		invalid argument
sigerr	equ	*
	l	r1,-p_size+p_error(top)	load address of "error: CARDINAL"
	st	r0,0(r1)
	lis	r0,0			return FALSE
	b	return
*
* return from signal routine
*
return	equ	*
	si	top,p_size
	br	rf
*
*
* branch table -- save regs & transfer to actual function address
* note: entries must be 16 bytes long!
*
*
	align	adc
brtab	equ	*	nsig entries
	shi	limit,16*adc
	stm	r0,0(limit)
	lhi	r1,1*adc(0)
	b	callproc(0)
*
	shi	limit,16*adc
	stm	r0,0(limit)
	lhi	r1,2*adc(0)
	b	callproc(0)
*
	shi	limit,16*adc
	stm	r0,0(limit)
	lhi	r1,3*adc(0)
	b	callproc(0)
*
	shi	limit,16*adc
	stm	r0,0(limit)
	lhi	r1,4*adc(0)
	b	callproc(0)
*
	shi	limit,16*adc
	stm	r0,0(limit)
	lhi	r1,5*adc(0)
	b	callproc(0)
*
	shi	limit,16*adc
	stm	r0,0(limit)
	lhi	r1,6*adc(0)
	b	callproc(0)
*
	shi	limit,16*adc
	stm	r0,0(limit)
	lhi	r1,7*adc(0)
	b	callproc(0)
*
	shi	limit,16*adc
	stm	r0,0(limit)
	lhi	r1,8*adc(0)
	b	callproc(0)
*
	shi	limit,16*adc
	stm	r0,0(limit)
	lhi	r1,9*adc(0)
	b	callproc(0)
*
	shi	limit,16*adc
	stm	r0,0(limit)
	lhi	r1,10*adc(0)
	b	callproc(0)
*
	shi	limit,16*adc
	stm	r0,0(limit)
	lhi	r1,11*adc(0)
	b	callproc(0)
*
	shi	limit,16*adc
	stm	r0,0(limit)
	lhi	r1,12*adc(0)
	b	callproc(0)
*
	shi	limit,16*adc
	stm	r0,0(limit)
	lhi	r1,13*adc(0)
	b	callproc(0)
*
	shi	limit,16*adc
	stm	r0,0(limit)
	lhi	r1,14*adc(0)
	b	callproc(0)
*
	shi	limit,16*adc
	stm	r0,0(limit)
	lhi	r1,15*adc(0)
	b	callproc(0)
*
	shi	limit,16*adc
	stm	r0,0(limit)
	lhi	r1,16*adc(0)
*
*
* call modula-2 procedure
*
callproc equ	*
	shi	limit,16*adc
	stmd	0,0(limit)		save floating point registers
	sis	limit,3*adc		allocate stack for prio, -1 and signo
	l	r6,sigtab-adc(r1)	procedure address
	srls	r1,2			get signal number
	lb	r0,priotab(r1)		get software priority of signal
	c	r0,32+3+3*adc(limit)	compare with priority of process
	bnp	ignore			ignore if proc-prio >= signal-prio
	nhi	top,x'fffc'		align top
	ahi	top,1+3*adc		and allocate procmarkspace
	l	r0,32+3+3*adc(limit)	load previous priority
	st	r0,0(limit)		and push it
	lcs	r0,1			-1 for mdb
	st	r0,adc(limit)		indicates interrupt
	st	r1,2*adc(limit)		store signal number on stack
	cr	top,limit		stack overflow ???
	bnm	.stack
	lis	base,1			signals interrupt for mdb
	balr	rf,r6			call procedure
	b	rti
*
* if signal has been ignored reset procedure address
*
ignore	equ	*
	lr	r8,r1			signo
	slls	r8,4			offset in branch table
	la	r6,brtab-16(r8)		branch table address
	st	r1,svc.no		set signal number
	st	r6,svc.fn		set procedure address
	svc	0,0			* signal *
	dc	a(svcsig)
*
* restore registers
*
rti	equ	*
	ais	limit,3*adc		prio, -1 and signo
	lmd	0,0(limit)		restore floating point regs
	ahi	limit,16*adc
	lm	r0,0(limit)		restore regs
	ahi	limit,16*adc		pop stack
*
* return from interrupt
*
	svc	0,signal		* signal 0 *
	dc	0
	impur
*
* signal svc
*
	align	adc
svcsig	equ	*
	svc	0,signal		* signal *
svc.no	dc	0			signal number
svc.fn	dc	0			function address
*
* sig table - func value for each signal
*
sigtab	equ	*
	do	nsig
	dc	0
*
* priority table - (software) priority of each signal
*
priotab	equ	*
	db	0			SIGRTI (return from interrupt)
	db	2			SIGHUP
	db	1			SIGINT
	db	2			SIGQUIT
	db	3			SIGILL
	db	3			SIGTRAP
	db	3			SIGIOT
	db	3			SIGEMT
	db	3			SIGFPE
	ds	1			SIGKILL (dummy entry)
	db	3			SIGBUS
	db	3			SIGSEGV
	db	3			SIGSYS
	db	2			SIGPIPE
	db	1			SIGALRM
	db	2			SIGTERM
*
	end
