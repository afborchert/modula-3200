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
fr2	equ	4
fr3	equ	6
fr4	equ	8
fr5	equ	10
fr6	equ	12
fr7	equ	14
	.file	c'SysWrite.m2'
	db	c'$Source:@(#)SysWrite.m2$',0
	dcf	G_AREA
	entry	SysWrite
SysWrite	equ	*
P_AREA	equ	*
	dcf	LP.0
	dcf	LP.1
	extrn	SysWrite
	extrn	Errno
	extrn	Sys
*	  
	bss
_FLAG	dsf	1
G_AREA	dsf	0
*	@ 1
*	@ 9
*	  
	pure
	align	adc
LP.1	equ	*
SysWrite_P1	equ	*
	entry	SysWrite_P1
	.def	SysWrite_P1;	.val	.;	.scl	2;	.type	y'20';	.endef
	stm	base,-8(top)
	lr	base,top
	ai	top,I.1+28
	.def	.bf;	.val	.;	.scl	101;	.line	10;	.endef
*	@ 10
	.ln	1
*	@ 11
	.ln	2
	l	rf,-24(base)	UNIXCALL
	st	rf,S.3(top)
	l	rf,-20(base)
	st	rf,S.3+4(top)
	l	rf,-16(base)
	l	rf,0(rf)
	st	rf,S.3+8(top)
	la	rf,0(base)
	la	rc,4(base)
	lr	ra,base
	la	base,S.3(top)
	svc	r0,4
	bnc	I.4
	lis	rb,0
	b	I.5
I.4	equ	*
	lis	rb,1
I.5	equ	*
	lr	base,ra
	st	r0,0(rf)
	st	r1,0(rc)
	lr	rb,rb
S.3	equ	-12
	be	I.6
*	@ 12
	.ln	3
	l	rf,-16(base)
*	@ 13
	.ln	4
	l	rc,0(base)
	st	rc,0(rf)
	li	r0,1
	b	B.0
	b	I.2
I.6	equ	*
*	@ 14
	.ln	5
*	@ 15
	.ln	6
*	@ 16
	.ln	7
	l	rf,0(base)
	extrn	Errno_V0
	st	rf,Errno_V0+0
	l	rf,-16(base)
*	@ 17
	.ln	8
	li	rc,0
	st	rc,0(rf)
	li	r0,0
	b	B.0
I.2	equ	*	endif
*	@ 18
	.ln	9
	extrn	.fret
	bal	r1,.fret
B.0	equ	*
I.1	equ	0
	.def	.ef;	.val	.;	.scl	101;	.line	10;	.endef
	lr	top,base
	lm	base,-8(top)
	si	top,24
	br	rf
*	@ 19
*	@ 21
	align	adc
LP.0	equ	*
SysWrite_P0	equ	*
	entry	SysWrite_P0
	.def	SysWrite_P0;	.val	.;	.scl	2;	.type	y'20';	.endef
	stm	base,-8(top)
	lr	base,top
	ts	r0,_FLAG
	bm	B.7
	ai	top,I.8+20
	extrn	Errno_P0
	bal	rf,Errno_P0
	extrn	Sys_P0
	bal	rf,Sys_P0
	.def	.bf;	.val	.;	.scl	101;	.line	21;	.endef
B.7	equ	*
I.8	equ	0
	.def	.ef;	.val	.;	.scl	101;	.line	1;	.endef
	lr	top,base
	lm	base,-8(top)
	br	rf
*	@ 22
*	  
	impur
*	  
	pure
	align	adc	unused procedure labels
	end
