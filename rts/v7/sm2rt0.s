*
*	modula-2 library -- runtime initialization
*
*	version using the stack segment (afb 7/84)
*
*	afb 5/84
*
	extrn	_M2START
	extrn	end
	extrn	.stack
	extrn	SysExit
	entry	_ENV
	entry	_ARGC,_ARGV
	entry	_BREAK
*
	pure
*
*	make arguments and environment available for modula-2
*
	l	r0,0(limit)
	st	r0,_ARGC
	la	r1,adc(limit)
	st	r1,_ARGV
envlp	equ	*
	l	r0,0(r1)
	bz	fndenv
	ais	r1,adc
	b	envlp
fndenv	equ	*
	ais	r1,adc
	st	r1,_ENV
*
*	find lower limit of stack
*
	lis	r0,SIGSEGV	
	st	r0,svc.no	signal number
	la	r1,sigsegv
	st	r1,svc.fn	function address
	svc	0,0		* signal *
	dc	a(svcsig)
	bc	.stack		carry bit -- error
	st	r0,prev_fn	old function address (SIGINT or SIG_DFL)
	lr	base,limit	
	lis	r0,0
	st	r0,caught
stacklp	equ	*
	sis	base,adc
	l	r0,0(base)
	l	r0,caught
	bz	stacklp
*
	lr	top,base
	lis	base,0		for mdb
	lis	r0,SIGSEGV	restore old function address
	st	r0,svc.no	signal number
	l	r0,prev_fn
	st	r0,svc.fn	function address
	svc	0,0		* signal *
	dc	a(svcsig)
	bc	.stack		carry bit -- error
*
*
	la	r1,end
	st	r1,_BREAK
*	push priority 0
*
	sis	limit,adc
	lis	r0,0
	st	r0,0(limit)
*
*	start modula-2
*
	ahi	top,5*adc
	bal	rf,_M2START
*
*	call SysExit.Exit(0)
*
	lis	r0,0		normal termination: exit code = 0
	st	r0,-16(top)	store parameter
	l	r1,SysExit+4	call procedure with number = 1
	balr	rf,r1
*	NOTREACHED
*
*-----------------------------------------------------------------------------*
*	interrupt service routine                                             *
*-----------------------------------------------------------------------------*
SIGSEGV	equ	11
sigsegv	equ	*
	ais	base,adc
	lis	r4,1
	st	r4,caught	set flag
	lis	r4,0		return from interrupt
	st	r4,svc.no
	st	r4,svc.fn
	svc	0,0		* signal *
	dc	a(svcsig)
	bc	.stack
*-----------------------------------------------------------------------------*
*	signal svc							      *
*-----------------------------------------------------------------------------*
	impur
svcsig	equ	*
	svc	0,signal
svc.no	dc	0
svc.fn	dc	0
*-----------------------------------------------------------------------------*
	bss
_ARGC	dsf	1
_ARGV	dsf	1
_ENV	dsf	1
_BREAK	dsf	1
prev_fn	dsf	1
caught	dsf	1
	end
