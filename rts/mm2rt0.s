*
*	modula-2 library -- runtime initialization with monitor
*	XELOS version
*
*	afb 6/84
*
include(equ.s)
include(sys.s)
	extrn	_M2START
	extrn	_end
	extrn	_etext
	extrn	SysExit_P1	SysExit.Exit
	extrn	SysMonitor_P1	SysMonitor.Monitor
	extrn	.stack
	entry	_ENV
	entry	_ARGC,_ARGV
	entry	_BREAK
	entry	.countb
*
cbufs	equ	1000		no. of words in call-count buffer
*
	pure
*
*	make arguments and environment available for modula-2
*
	l	r0,0(base)
	st	r0,_ARGC
	l	r1,adc(base)
	st	r1,_ARGV
	l	r1,adc*2(base)
	st	r1,_ENV
	la	r1,_end
	st	r1,_BREAK
*
*	profile buffer size = 3 + cbufs*2 + (ADR(etext) - ADR(eprol) + 15)/16
*
	la	rc,_etext
	si	rc,eprol
	ais	rc,15
	srls	rc,4
	lhi	r0,cbufs
	slls	r0,1
	ar	rc,r0
	ais	rc,3
*
*	get storage for profile buffer
*
	lr	r0,rc
	slls	r0,2
	l	rb,_BREAK
	ar	r0,rb
	st	r0,0(base)	base == ap
	st	r0,_BREAK
	svc	0,break
	bc	.stack		break failed ???
*
*	start profiling
*
	lr	r0,rb
	ais	r0,3*adc
	st	r0,.countb
*
	lr	top,base
	lis	base,0
	lis	limit,0			no priorities nor coroutines
*
*	Monitor(ADR(eprol), ADR(etext), buf (in rb), bufsiz (in rc), cbufs);
*
	ai	top,pspace+5*adc
	lhi	r0,cbufs
	st	r0,-adc-pspace(top)
	st	rc,-2*adc-pspace(top)
	st	rb,-3*adc-pspace(top)
	la	r0,_etext
	st	r0,-4*adc-pspace(top)
	la	r0,eprol
	st	r0,-5*adc-pspace(top)
	bal	rf,SysMonitor_P1
*
*	start modula-2
*
	bal	rf,_M2START
*
*	call SysExit.Exit(0)	calls Monitor(0)
*
	lis	r0,0		normal termination: exit code = 0
	st	r0,-16(top)	store parameter
	bal	rf,SysExit_P1
*	NOTREACHED
*
eprol	equ	*		end of prologue
*
	impur
	align	adc
.break	equ	*
	svc	r0,break
_break	equ	*
	ds	4
	align	adc
.countb	dc	0		next available call-count buffer entry
*
	bss
_ARGC	dsf	1
_ARGV	dsf	1
_ENV	dsf	1
_BREAK	dsf	1
	end
