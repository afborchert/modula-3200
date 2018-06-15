*
*	case error -- modula-2 runtime library
*
*	called with	bal	r1,.case
*
include(equ.s)
	entry	.case
	extrn	.abort
	pure
.case	equ	*
	lis	r0,CASE
	b	.abort
	end
