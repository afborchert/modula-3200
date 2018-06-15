*	  
	extrn	_end,_etext,_edata,_BREAK,_ENV
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
	dcf	G_AREA
	entry	SysLocations
SysLocations	ds	0
P_AREA	equ	*
	dcf	LP.0
	extrn	SysLocations
*	  
	bss
_FLAG	dsf	1
G_AREA	dsf	5
	entry	SysLocations_V0
	entry	SysLocations_V4
	entry	SysLocations_V8
	entry	SysLocations_V12
	entry	SysLocations_V16
SysLocations_V0	equ	G_AREA
SysLocations_V4	equ	G_AREA+4
SysLocations_V8	equ	G_AREA+8
SysLocations_V12	equ	G_AREA+12
SysLocations_V16	equ	G_AREA+16
*	  
	pure
	align	adc
SysLocations_P0	equ	*
	entry	SysLocations_P0
LP.0	equ	*
	stm	base,-8(top)
	lr	base,top
	ts	r0,_FLAG
	bm	B.0
	ai	top,I.1+20
BB.0	equ	*
	la	rf,_end
	st	rf,G_AREA
	la	rf,_etext
	st	rf,G_AREA+4
	la	rf,_edata
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
