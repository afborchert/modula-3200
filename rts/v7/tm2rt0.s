*
*	modula-2 library -- runtime initialization
*
*	version with timelimit (afb 10/84)
*
*	afb 5/84
*
	extrn	_M2START
	extrn	end
	extrn	.stack,.size
	extrn	.env,.conv
	extrn	SysExit
	entry	_ENV
	entry	_ARGC,_ARGV
	entry	_BREAK
*
SIGALRM	equ	14
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
*	allocate stack
*
	la	top,end
	lis	base,0
	lr	limit,top
	a	limit,.size
	st	limit,_break	set break address
	st	limit,_BREAK
	svc	r0,0		indir
	dc	.break
	bc	.stack		break failed ???
*
*	push priority 0
*
	sis	limit,adc
	lis	r0,0
	st	r0,0(limit)
*
*	set timelimit
*
	la	r1,timelimit	r1 -> "TIMELIMIT"
	bal	rf,.env		get value of timelimit
	lr	r1,r1		set condition codes
	bz	start		no timelimit given
	bal	rf,.conv	convert string to CARDINAL
	lr	r1,r1		set condition codes
	bz	start		timelimit = 0 ???
	lis	r0,SIGALRM	set up interrupt service routine
	st	r0,svc.no	signal number
	la	r0,isr_alrm	function address
	st	r0,svc.fn
	svc	0,0		* signal *
	dc	a(svcsig)
	bc	start		carry bit -- error
	lr	r0,r1		#seconds in r0
	svc	0,alarm		set alarm clock
*
*	start modula-2
*
start	equ	*
	ahi	top,5*adc
	bal	rf,_M2START
*
*	call SysExit.Exit(0)
*
	lis	r0,0		normal termination: exit code = 0
	st	r0,-16(top)	store parameter
	l	r1,SysExit+4	call procedure with number = 1
	balr	rf,r1
*-----------------------------------------------------------------------------*
*	interrupt service routine                                             *
*-----------------------------------------------------------------------------*
isr_alrm equ	*
	lis	r0,2		file descriptor in r0 (stderr)
	la	r1,message
	st	r1,svc.buf	set buffer address
	lhi	r1,messlen
	st	r1,svc.n	set buffer len
	svc	0,0		* write *
	dc	a(svcwrite)
	lis	r0,1		termination code 1
	svc	r0,exit
*
message	db	c'Timelimit exceeded.',10
messlen	equ	*-message
*-----------------------------------------------------------------------------*
	impur
	align	adc
*-----------------------------------------------------------------------------*
*	break svc                                                             *
*-----------------------------------------------------------------------------*
.break	equ	*
	svc	r0,break
_break	equ	*
	ds	4
*-----------------------------------------------------------------------------*
*	signal svc                                                            *
*-----------------------------------------------------------------------------*
svcsig	equ	*
	svc	0,signal
svc.no	dc	0
svc.fn	dc	0
*-----------------------------------------------------------------------------*
*	write svc                                                             *
*-----------------------------------------------------------------------------*
svcwrite equ	*
	svc	0,write
svc.buf	dc	0
svc.n	dc	0
*-----------------------------------------------------------------------------*
timelimit db	c'TIMELIMIT',0
	align	adc
*
	bss
_ARGC	dsf	1
_ARGV	dsf	1
_ENV	dsf	1
_BREAK	dsf	1
	end
