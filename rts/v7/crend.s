*
*	coroutine end error -- modula-2 runtime library
*
	entry	.crend
	extrn	.abort
	pure
.crend	equ	*
	lis	r0,CREND
	b	.abort
	end
