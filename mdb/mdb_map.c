/*
 *	mdb - read map-dependent
 */

#include	<stdio.h>
#include	"mdb.h"
#include	"mdb_map.h"

/*
 *	set value to the contents of specified address
 *	(for text and data segment)
 *	return -1 on failure
 *		0 if ok
 */

readm(fp, mp, addr, value)
	FILE *fp;
	struct map mp;
	address addr;
	long *value;	/* OUT-parameter */
{	struct map * m = &mp;

	if ( within(m->b1, addr, m->e1) )
		*value = readlong(fp, addr + m->f1 - m->b1);
	else if ( within(m->b2, addr, m->e2) )
		*value = readlong(fp, addr + m->f2- m->b2);
	else
		return -1;
	return 0;
}

/* check:  low <= in <= high */

within(low, in, high)
	char *low, *in, *high;
{
	if ( low <= in && in < high )
		return 1;
	else
		return 0;
}

long readlong(fp, offset)
	FILE *fp;
	address offset;
{	long value;

	if ( fseek(fp, offset, 0) ) {
		perror(fp);
		return 0L;
		}
	fread(&value, sizeof(long), 1, fp);
	return value;
}

/* the result should be a multiple of `b' */

long round(a,b)
long a, b;
{	long w;

	w = ((a+b-1)/b)*b;
	return w;
}
