*
*	transfer -- modula-2 runtime library
*
*	afb 5/84
*
include(equ.s)
include(process.s)
	extrn	.stack,.crend
	entry	.transfe,.transfer
	pure
param	struc
source	dsf	1		VAR source: PROCESS;		(r4)
dest	dsf	1		VAR destination: PROCESS;	(r5)
	ends
.transfe equ	*
.transfer equ	*
	l	r5,-param+dest(top)
	l	r5,0(r5)	dereference 2nd argument
*				see Modula-2 Report, 13.1, Note
	l	r4,-param+source(top)
	st	top,0(r4)
	ai	top,pd_size
	cr	top,limit			check for stack overflow
	bnm	.stack
	st	base,-pd_size+pd_base(top)
	st	top,-pd_size+pd_top(top)
	st	limit,-pd_size+pd_limit(top)
	st	rf,-pd_size+pd_p(top)		proc := rf;
	lis	r0,1				started := TRUE;
	st	r0,-pd_size+pd_start(top)
*
*	l	r5,0(r5)	must be done before "st top,0(r4)"
	l	base,pd_base(r5)
	l	top,pd_top(r5)
	l	limit,pd_limit(r5)
	l	rf,pd_p(r5)
	l	r1,pd_start(r5)
	si	top,pd_size
*	coroutine started ???
	lr	r1,r1		set condition codes
	bz	not_started
	si	top,param
	br	rf
not_started equ *
	sis	limit,2*adc
	st	rf,adc(limit)	save procedure address
	lis	r0,0
	st	r0,0(limit)	push priority 0
	lr	r1,rf
	ahi	top,poffs
	balr	rf,r1
*	coroutine must not return
	l	r1,adc(limit)	get procedure address
	b	.crend
	end
