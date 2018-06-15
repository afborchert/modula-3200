*
*	coroutine end error -- modula-2 runtime library
*
include(equ.s)
	entry	.crend
	extrn	.abort
	pure
.crend	equ	*
	lis	r0,CREND
	b	.abort
	end
