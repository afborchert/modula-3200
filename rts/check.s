*
*	modula-2 runtime library -- checks
*
*	afb 7/84
*
include(equ.s)
	entry	.chkerr
	entry	.chkz
	entry	.chk
	entry	.uchk
	entry	.chks
	entry	.rgeerr
	extrn	.abort
*
*	error structure for debugging
*
	bss
	align	adc
.rgeerr	equ	*
type	dsf	1	type: unsigned = 1, signed = 2, sign check = 3
*			      dynamic arrays = 4
pc	dsf	1	store error position
value	dsf	1
low	dsf	1
high	dsf	1
*
	pure
.chkerr	equ	*	(index check for dynamic arrays)
	st	r1,pc
	lis	r0,4
	st	r0,type
	b	error
*
.chk	equ	*		signed check
	ais	r1,3*adc-1
	nhi	r1,x'fffc'	align r1
	c	r0,-2*adc(r1)	compare with low
	bm	chkerr
	c	r0,-adc(r1)	compare with high
	bnpr	r1
chkerr	equ	*
	st	r1,pc
	st	r0,value
	lis	r0,2
	st	r0,type
	l	r0,-2*adc(r1)
	st	r0,low
	l	r0,-adc(r1)
	st	r0,high
	b	error
*
.uchk	equ	*		unsigned check
	ais	r1,3*adc-1
	nhi	r1,x'fffc'	align r1
	cl	r0,-2*adc(r1)
	bl	uchkerr
	cl	r0,-adc(r1)
	blr	r1
	ber	r1
uchkerr	equ	*
	st	r1,pc
	st	r0,value
	lis	r0,1
	st	r0,type
	l	r0,-2*adc(r1)
	st	r0,low
	l	r0,-adc(r1)
	st	r0,high
	b	error
*
.chkz	equ	*		check unsigned range [0 .. 0(r1)]
	ais	r1,2*adc-1
	nhi	r1,x'fffc'	align r1
	cl	r0,-adc(r1)
	blr	r1
	ber	r1
	st	r1,pc
	st	r0,value
	lis	r0,1
	st	r0,type
	l	r0,-adc(r1)
	st	r0,high
	lis	r0,0
	st	r0,low
	b	error
*
.chks	equ	*		check sign bit
	bnmr	r1		condition codes are set
	st	r1,pc
	st	r0,value
	lis	r0,3
	st	r0,type
*	b	error
*
error	equ	*
	lis	r0,RANGE
	b	.abort
*
	end
