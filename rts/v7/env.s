*
*	modula-2 runtime library -- get environment
*
include(equ.s)

	entry	.env
	extrn	_ENV

*-----------------------------------------------------------------------------*
*	.env searches the enviroment list for a string of the form
*	name=value and returns value if such a string is present,
*	otherwise 0
*
*	parameter: r1 -> name
*	return value in r1
*-----------------------------------------------------------------------------*
.env	equ	*
	ahi	top,16		save used registers
	stm	rc,-16(top)
*
	l	rc,_ENV		_ENV -> environment list
search	equ	*
	l	rd,0(rc)	rd -> next name in enviroment list
	bz	notfound	end of enviroment list ???
	lr	rf,r1
compare	equ	*		compare 0(rf) with 0(rd)
	lb	r0,0(rd)	next character of name
	clhi	r0,c'='		end of name reached ??
	be	check
	clb	r0,0(rf)	compare with given name
	bne	nextname
	ais	rd,1		increment pointers
	ais	rf,1
	b	compare
nextname equ	*
	ais	rc,adc
	b	search
*
check	equ	*
	lb	r0,0(rf)	get character from given name
	clhi	r0,0		check for end of string
	bne	nextname
	lr	r1,rd
	ais	r1,1		r1 -> value
	b	return
*
notfound equ	*
	lis	r1,0
	b	return
*
return	equ	*
	lm	rc,-16(top)
	shi	top,16
	br	rf
*
	end
