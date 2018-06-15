/*
 *	global definitions
 */

#ifndef GLOBDEF_DEF
#define	GLOBDEF_DEF

typedef unsigned long address;
typedef unsigned val;

#define	READ	1
#define	WRITE	2

#define	TEXT	1
#define	DATA	2

#ifdef xelos
#define	NIL	(0xefffff)
#else
#define	NIL	(-1)
#endif

#endif

#ifndef xelos
#define void int
#endif
