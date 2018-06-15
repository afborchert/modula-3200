* mcount -- count subroutine calls during profiling
* XELOS version
*
* called at function entry (after the save sequence) by
*	la	r1,LABEL
*	bal	rf,mcount
* where LABEL is a word in the bss segment pointing to the function's
* entry in the count buffer
* each count buffer entry is two words: a pointer to the function entry
* point (actually pointing after the 'bal' to mcount, and initialized
* at the first call of mcount), and the count of calls
*
* ref to libc/crt/mcount.s	Unix Edition VII
*
	pure
	.file	c'mcount.s'	looks like a Modula-2 module
mcount_	equ	*
*
include(equ.s)
	entry	.mcount
	entry	mcount_P0_
	entry	mcount_
	extrn	.countb		next available count buffer entry
*
	pure
*
mcount_P0_ equ	*		label for mprof
	.def	mcount_P0_;	.val	.;	.scl	2;	.type	y'20';	.endef
.mcount	equ	*
	.def	.bf;	.val	.;	.scl	101;	.line	26;	.endef
	l	rc,0(r1)	pointer to count buffer entry
	bnz	mc1		skip if already initialized
	l	rc,.countb
	bzr	rf		no buffer - give up
	lr	r0,rc
	ais	r0,8		allocate buffer entry for this function
	st	r0,.countb
	st	rf,0(rc)	set back pointer to function
	ais	rc,adc
	st	rc,0(r1)	set forward pointer to count buffer entry
mc1	equ	*
	lis	r0,1
	am	r0,0(rc)
	.def	.ef;	.val	.;	.scl	101;	.line	1;	.endef
	br	rf
	end
