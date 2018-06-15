*
*	modula-2 runtime library -- convert string to CARDINAL
*
include(equ.s)

	entry	.conv
*-----------------------------------------------------------------------------*
*	parameter: r1 -> string
*	return value in r1
*-----------------------------------------------------------------------------*
.conv	equ	*
	ahi	top,20			save registers
	stm	rb,-20(top)
*
	lis	rb,10
	lis	rc,0
	lis	rd,0
loop	equ	*
	lb	rf,0(r1)		load next character
	lr	rf,rf			set condition codes
	bz	exit
	si	rf,c'0'			convert single character
	mr	rc,rb			value := value * 10
	ar	rd,rf			value := value + digit
	ais	r1,1			increment pointer
	b	loop
exit	equ	*
	lr	r1,rd
	lm	rb,-20(top)
	shi	top,20
	br	rf			return value in r1
	end
