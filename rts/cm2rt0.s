*
*	modula-2 library -- runtime initialization
*
*	for programs with coroutines, stack stacks and priorities
*
*	afb 5/86
*
include(sys.s)
include(equ.s)
	extrn	_M2START
	extrn	_end
	extrn	SysExit
	extrn	PrioTab_P0
	entry	_ENV
	entry	_ARGC,_ARGV
	entry	_BREAK
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
*	initialize PrioTab module
*
	bal	rf,PrioTab_P0
*
*	start modula-2
*
	ai	base,1024		allocate priority stack
	lr	limit,base
	oi	limit,y'40000000'	=> top < limit
	si	limit,poffs+adc
	lis	r0,0			push priority 0
	st	r0,0(limit)
	lr	top,base
	lis	base,0			end of stack indication for mdb
	bal	rf,_M2START
*
*	call SysExit.Exit(0)		SysExit needn't to be initialized here
*
	lis	r0,0		normal termination: exit code = 0
	st	r0,-16(top)	store parameter
	l	r1,SysExit+4	call procedure with number = 1
	balr	rf,r1
*	NOTREACHED
*
	bss
_ARGC	dsf	1
_ARGV	dsf	1
_ENV	dsf	1
_BREAK	dsf	1
	end
