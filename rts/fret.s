*
*	modula-2 runtime library -- function returns no value
*
*	called with	bal	r1,.fret
*
include(equ.s)
	entry	.fret
	extrn	.abort
*
	pure
	align	adc
.fret	equ	*
	lis	r0,FRET
	b	.abort
*
	end
