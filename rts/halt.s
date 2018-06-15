*
*	halt -- modula-2 runtime library
*
*	called with	bal	r1,.halt
*
include(equ.s)
	entry	.halt
	extrn	.abort
	pure
.halt	equ	*
	lis	r0,HALT
	b	.abort
	end
