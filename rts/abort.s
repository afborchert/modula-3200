*
*	abort -- modula-2 runtime library
*
*	produce core dump and exit
*
include(sys.s)
include(equ.s)
	entry	.abort
	entry	.code
	entry	.base
	entry	.top
*
	bss
	align	4
.base	dsf	1		store old base and top
.top	dsf	1
.code	dsf	1
_FLAG	dsf	1
*
	pure
	align	adc
.abort	equ	*
	ts	_FLAG		looping ???
	bm	bye
*
	lr	rf,r1		old pc in r1
	st	base,.base
	st	top,.top
*
	st	r0,.code	error code (for mdb)
	svc	r0,getpid	=> pid in r0
*
	lr	base,top
	ai	top,8
	st	r0,0(base)	process id
	li	rc,6		IOT - Trap
	st	rc,4(base)
	svc	r0,kill
*	if SIGIOT is ignored or caught...
bye	equ	*
	l	r0,.code
	st	r0,0(base)
	svc	r0,exit
*	NOTREACHED
