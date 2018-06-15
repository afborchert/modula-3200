		entry	singlepr
		entry	_singleprec
		pure
		align	adc
singlepr	equ	*
~singlep	equ	*
	ld	0,0(7)
	ledr	0,0
	br	15
*
_singleprec	equ	*
	ld	0,0(14)
	ledr	0,0
	br	15
