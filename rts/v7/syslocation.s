*	  
	extrn	end,etext,edata,_BREAK,_ENV
	pure
*	general registers
r0	equ	0
r1	equ	1
top	equ	2
r3	equ	3
r4	equ	4
r5	equ	5
r6	equ	6
limit	equ	7
r8	equ	8
r9	equ	9
ra	equ	10
rb	equ	11
rc	equ	12
rd	equ	13
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
	dcf	G_AREA
	entry	SysLocat
SysLocat	ds	0
P_AREA	equ	*
	dcf	LP.0
	extrn	SysLocat
*	  
	bss
_FLAG	dsf	1
G_AREA	dsf	5
*	  
	pure
	align	adc
LP.0	equ	*
	stm	base,-8(top)
	lr	base,top
	ts	r0,_FLAG
	bm	B.0
	ai	top,I.1+20
	la	r1,I.2(top)
	cr	r1,limit
	bm	I.3
	extrn	.stack
	bal	r1,.stack
I.3	equ	*
BB.0	equ	*
	la	rf,end
	st	rf,G_AREA
	la	rf,etext
	st	rf,G_AREA+4
	la	rf,edata
	st	rf,G_AREA+8
	l	rf,_BREAK
	st	rf,G_AREA+12
	l	rf,_ENV
	st	rf,G_AREA+16
B.0	equ	*
I.2	equ	0
I.1	equ	0
BE.0	equ	*
	lr	top,base
	lm	base,-8(top)
	br	rf
*	  
	impur
*	  
	pure
	align	adc	unused procedure labels
	end
