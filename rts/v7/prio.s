*
*	modula-2 runtime library -- enter priority and exit priority
*
	entry	.entprio
	entry	.exprio
	extrn	.abort
	extrn	.stack
*
	pure
	align	adc
*
*	Note: value in r0 must be > 0
*
.entprio equ	*		enter priority -- new priority in r0
	sis	limit,adc
	lr	r1,rf		save pc in r1
	cr	top,limit
	bnm	.stack
	st	r0,0(limit)	push priority
	c	r0,adc(limit)	check priority
	bm	error		priority error
return	equ	*
	br	rf
*
error	equ	*
	lis	r0,PRIO		load priority error code
	b	.abort		and abort
*
.exprio	equ	*		exit priority
	ais	limit,adc	pop last priority
	br	rf
*
	end
