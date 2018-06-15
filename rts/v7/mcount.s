* mcount -- count subroutine calls during profiling
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
* ref to libc/crt/mcount.s
*
	entry	.mcount
	extrn	.countb		next available count buffer entry
*
	pure
*
.mcount	equ	*
	l	rd,0(r1)	pointer to count buffer entry
	bnz	mc1		skip if already initialized
	l	rd,.countb
	bzr	rf		no buffer - give up
	lr	r0,rd
	ais	r0,8		allocate buffer entry for this function
	st	r0,.countb
	st	rf,0(rd)	set back pointer to function
	ais	rd,adc
	st	rd,0(r1)	set forward pointer to count buffer entry
mc1	equ	*
	lis	r0,1
	am	r0,0(rd)
	br	rf
	end
