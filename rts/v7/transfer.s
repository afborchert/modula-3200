*
*	transfer -- modula-2 runtime library
*
*	afb 5/84
*
*	rc	equ	12	var source
*	rd	equ	13	var dest
*
	extrn	.stack,.crend
	entry	.transfer
	pure
.transfer equ	*
	l	rd,0(rd)	dereference 2nd argument
*				see Modula-2 Report, 13.1, Note
	st	top,0(rc)
	ai	top,adc*5
	cr	top,limit
	bnm	.stack
	st	base,-5*adc(top)
	st	top,-4*adc(top)
	st	limit,-3*adc(top)
	st	rf,-2*adc(top)
	lis	r0,1
	st	r0,-adc(top)
*
*	l	rd,0(rd)	must be done before "st top,0(rc)"
	l	base,0(rd)
	l	top,adc(rd)
	l	limit,2*adc(rd)
	l	rf,3*adc(rd)
	si	top,5*adc
*	coroutine started ???
	l	r1,4*adc(rd)
	bz	not_started
	br	rf
not_started equ *
	sis	limit,2*adc
	st	rf,adc(limit)	save procedure address
	lis	r0,0
	st	r0,0(limit)	push priority 0
	lr	r1,rf
	ahi	top,5*adc
	balr	rf,r1
*	coroutine must not return
	l	r1,adc(limit)	get procedure address
	b	.crend
	end
