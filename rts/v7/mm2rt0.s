*
*	modula-2 library -- runtime initialization with monitor
*
*	afb 6/84
*
	extrn	_M2START
	extrn	end
	extrn	etext
	extrn	.stack,.size
	extrn	SysExit
	entry	_ENV
	entry	_ARGC,_ARGV
	entry	_BREAK
	entry	.countb
	extrn	SysMonit		Modula-2 module
*
cbufs	equ	100		no. of words in call-count buffer
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
	lr	limit,top
	lis	base,0		for mdb
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
*	profile buffer size = 3 + cbufs*2 + (ADR(etext) - ADR(eprol) + 15)/16
*
	la	rc,etext
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
	l	rd,_BREAK
	ar	r0,rd
	st	r0,_break
	st	r0,_BREAK
	svc	r0,0		indir
	dc	.break
	bc	.stack		break failed ???
*
*	start profiling
*
	lr	r0,rd
	ais	r0,3*adc
	st	r0,.countb
*
*	Monitor(ADR(eprol), ADR(etext), buf (in rd), bufsiz (in re), cbufs);
*
	ai	top,pspace+5*adc
	lhi	r0,cbufs
	st	r0,-adc-pspace(top)
	st	rc,-2*adc-pspace(top)
	st	rd,-3*adc-pspace(top)
	la	r0,etext
	st	r0,-4*adc-pspace(top)
	la	r0,eprol
	st	r0,-5*adc-pspace(top)
	l	r1,SysMonit+4
	balr	rf,r1
*
*	start modula-2
*
	ahi	top,5*adc
	bal	rf,_M2START
*
*	Monitor(0, 0, 0, 0, 0); (* write mon.out file and exit *)
*
	ai	top,pspace+5*adc
	lis	r0,0
	st	r0,-5*adc-pspace(top)
	l	r1,SysMonit+4
	balr	rf,r1
*
*	call SysExit.Exit(0)
*
	lis	r0,0		normal termination: exit code = 0
	st	r0,-16(top)	store parameter
	l	r1,SysExit+4	call procedure with number = 1
	balr	rf,r1
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
