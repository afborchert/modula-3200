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
top	equ	13	UNIX stack pointer
base	equ	14
rf	equ	15
*
HALT	equ	1	call to procedure halt
CASE	equ	2	no case label
STACK	equ	3	stack overflow
CREND	equ	4	coroutine end
PRIO	equ	5	priority error
FRET	equ	6	function returns no value
RANGE	equ	7	range check fails
*
pspace	equ	12	procedure mark space
poffs	equ	20	see MCP4Block.m2
