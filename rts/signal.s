*
*	modula-2 runtime library -- signal
*
*	ref to libc/sys/signal.s
*	XELOS version
*
*	(c) Andreas Borchert 1986, University of Ulm
*
include(equ.s)
include(sys.s)
	entry	.signal
	extrn	.stack
	extrn	PrioTab_V0
*
*	PROCEDURE Signal(sig: CARDINAL; p: PROC; VAR old: PROC;
*	                 VAR error: CARDINAL) : BOOLEAN;
*
p_size	struc		structure of parameters and allocated area
p_sig	dsf	1
p_p	dsf	1
p_old	dsf	1
p_error	dsf	1
s_base	dsf	1	save base
s_rf	dsf	1	and rf
a_sig	dsf	1	arguments of signal: signal and
a_p	dsf	1	procedure
	ends
*
nsig	equ	20	number of signals; see /usr/include/signal.h
EINVAL	equ	22	error number for 'invalid value'
	pure
.signal	equ	*
	ai	top,p_size-s_base	save base+rf and allocate args
	stm	base,-p_size+s_base(top)
	la	base,-p_size+a_sig(top)	set argument pointer (= base)
*
* save previous signal value
*
	l	r1,-p_size+p_sig(top)	sig
	bnp	illsig			must be +ve
	chi	r1,nsig			check range
	bp	illsig			too high
	lr	r0,r1			save sig number
	slls	r1,2			offset in sig table
	l	r5,sigtab-adc(r1)	previous value
	l	r9,-p_size+p_old(top)	load address of "old: PROC"
*
* if proc is nonzero and even, it's a procedure address
*
	l	r6,-p_size+p_p(top)	proc
	st	r6,sigtab-adc(r1)	set into sig table
	bz	nofunc			zero - not an address
	ni	r6,1			check low bit
	bnz	nofunc			odd - not an address
*
* replace function by 'callproc' address so floating point regs can be saved
*
	la	r6,callproc
nofunc	equ	*
*
* call signal svc
*
	st	r0,0(base)		set signal number
	st	r6,4(base)		set procedure address
	svc	0,signal		signal(sig, proc)
	bc	sigerr			carry bit -- error
*
* return previous signal value
*
	lis	r8,1			check low bit
	nr	r8,r0
	bz	notodd
	st	r8,0(r9)		odd - signal previously ignored
	lis	r0,1			return TRUE
	b	return
notodd	equ	*
	st	r5,0(r9)		store actual proc address
	lis	r0,1			return TRUE
	b	return
*
* error exits
*
illsig	equ	*
	lhi	r0,EINVAL		invalid argument
sigerr	equ	*
	l	r1,-p_size+p_error(top)	load address of "error: CARDINAL"
	st	r0,0(r1)
	lis	r0,0			return FALSE
	b	return
*
* return from signal routine
*
return	equ	*
	lm	base,-p_size+s_base(top)
	si	top,p_size
	br	rf
*
* interrupt service routine which calls the modula-2 procedure
*
callproc equ	*
	ai	top,16*adc
	stmd	0,-16*adc(top)		save floating point registers
	l	r6,0(base)		get signal number (base = ap)
	slls	r6,2			offset in sig table / prio table
*
* check for priority; call isr only if interrupt priority > current priority
*
	lr	limit,limit		set condition codes
	bz	noprio			no priority used
	l	r0,PrioTab_V0(r6)	load priority of interrupt
	c	r0,0(limit)		compare priorities
	bnp	ignore
noprio	equ	*
	l	r6,sigtab-adc(r6)	procedure address
	ai	top,pspace+adc+adc	allocate procmarkspace
	stm	base,-pspace-adc-adc(top) save rf
	oi	base,1			interrupt indication for mdb
	balr	rf,r6			call procedure
*
* return from isr
*
rti	equ	*
	lm	base,-pspace-adc-adc(top) restore rf
	si	top,pspace+adc		free procmarkspace
rti_ign	equ	*
	lmd	0,-16*adc(top)		restore floating point registers
	si	top,16*adc
	br	rf			rf -> svc 0,67 (allocated by XELOS)
*
* if signal has been ignored reset procedure address
*
ignore	equ	*
	l	r4,4(base)		save old contents
	la	r5,callproc		second arg of signal
	st	r5,4(base)
	svc	0,signal		signal(signr, callproc)
	st	r4,4(base)		restore old contents
	b	rti_ign			return from interrupt
*
* sig table - func value for each signal
*
	impur
sigtab	equ	*
	do	nsig
	dc	0
