*	  
	pure
*	general registers
r0	equ	0
r1	equ	1
limit	equ	2
r3	equ	3
r4	equ	4
r5	equ	5
r6	equ	6
r7	equ	7
r8	equ	8
r9	equ	9
ra	equ	10
rb	equ	11
rc	equ	12
top	equ	13
base	equ	14
rf	equ	15
*	floating point double precision registers
fr0	equ	0
fr1	equ	2
fr7	equ	4
fr3	equ	6
fr4	equ	8
fr5	equ	10
fr6	equ	12
fr7	equ	14
	.file	c'Bytes.m2'
	db	c'$Source:@(#)Bytes.m2$',0
	dcf	G_AREA
	entry	Bytes
Bytes	equ	*
P_AREA	equ	*
	dcf	LP.0
	dcf	LP.1
	dcf	LP.2
	dcf	LP.3
	extrn	Bytes
*	  
	bss
_FLAG	dsf	1
G_AREA	dsf	0
*	@ 1
*	@ 8
*	  
	pure
	align	adc
LP.2	equ	*
Bytes_P2	equ	*
	entry	Bytes_P2
	.def	Bytes_P2;	.val	.;	.scl	2;	.type	y'20';	.endef
	stm	base,-8(top)
	lr	base,top
	ai	top,I.1+20
	.def	.bf;	.val	.;	.scl	101;	.line	8;	.endef
*	@ 9
	.ln	2
	l	rf,-20(base)
	l	rc,-16(base)	INC
	am	rc,0(rf)
B.0	equ	*
I.1	equ	0
	.def	.ef;	.val	.;	.scl	101;	.line	3;	.endef
	lr	top,base
	lm	base,-8(top)
	br	rf
*	@ 10
*	@ 14
	align	adc
LP.3	equ	*
Bytes_P3	equ	*
	entry	Bytes_P3
	.def	Bytes_P3;	.val	.;	.scl	2;	.type	y'20';	.endef
	stm	base,-8(top)
	lr	base,top
	ai	top,I.3+20
	.def	.bf;	.val	.;	.scl	101;	.line	14;	.endef
*	@ 15
	.ln	2
	l	rf,-20(base)
	l	r0,-16(base)	DEC
	lis	rc,0
	sr	rc,r0
	am	rc,0(rf)
B.2	equ	*
I.3	equ	0
	.def	.ef;	.val	.;	.scl	101;	.line	3;	.endef
	lr	top,base
	lm	base,-8(top)
	br	rf
*	@ 16
*	@ 20
	align	adc
LP.1	equ	*
Bytes_P1	equ	*
	entry	Bytes_P1
	.def	Bytes_P1;	.val	.;	.scl	2;	.type	y'20';	.endef
	stm	base,-8(top)
	lr	base,top
	.def	.bf;	.val	.;	.scl	101;	.line	20;	.endef
*	@ 21
	.ln	2
*
* load parameters into registers
*
	l	ra,-24(base)		to
	l	r3,-20(base)		from
	l	r6,-16(base)		no
	move	r6,0(ra),r6,0(r3)
B.4	equ	*
I.5	equ	0
	.def	.ef;	.val	.;	.scl	101;	.line	5;	.endef
	lr	top,base
	lm	base,-8(top)
	si	top,4
	br	rf
*	@ 26
	align	adc
LP.0	equ	*
Bytes_P0	equ	*
	entry	Bytes_P0
	.def	Bytes_P0;	.val	.;	.scl	2;	.type	y'20';	.endef
	stm	base,-8(top)
	lr	base,top
	ts	r0,_FLAG
	bm	B.6
	ai	top,I.7+20
	.def	.bf;	.val	.;	.scl	101;	.line	26;	.endef
B.6	equ	*
I.7	equ	0
	.def	.ef;	.val	.;	.scl	101;	.line	1;	.endef
	lr	top,base
	lm	base,-8(top)
	br	rf
*	@ 27
*	  
	impur
*	  
	pure
	align	adc	unused procedure labels
	end
