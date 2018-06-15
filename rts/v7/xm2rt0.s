*
*	modula-2 library -- runtime initialization
*
*	afb 5/84
*
	extrn	_M2START
	extrn	end
	extrn	SysExit
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
	la	r1,end
	st	r1,_BREAK
*
*	start modula-2
*
	lr	top,base
	lis	base,0
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
	bss
_ARGC	dsf	1
_ARGV	dsf	1
_ENV	dsf	1
_BREAK	dsf	1
	end
