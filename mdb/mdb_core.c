/*
 *	mdb - core file handling
 */

#include	<stdio.h>
#include	<sys/param.h>
#ifdef xelos
#include	<sys/sysmacros.h>
#include	<sys/types.h>
#include	<sys/seg.h>
#include	<sys/signal.h>
#endif
#include	<sys/dir.h>
#include	<sys/reg.h>
#include	<sys/user.h>
#include	<core.h>
#ifdef CORE_BUG
#undef stacktop
#undef stackbas
#define stacktop(siz) (0xF00000 + siz)
#define stackbas(siz) (0xF00000)
#else
#endif
#include	"mdb_map.h"

int coremode = 0;		/* is a core file opened ??? */

/*
 *	in corhdr we have the user structure of the process
 */

int _corhdr[ctob(USIZE)/sizeof(int)];
struct user * corhdr = (struct user *) _corhdr;

/*
 *	endhdr points to the saved registers
 */

int *endhdr;
static FILE *fp;
static int txtsiz, datsiz, stksiz;

struct map datmap;

/*
 *	initialisize the map above
 */

core_init(core)
	char	*core;		/* core file name */
{	int	magic;		/* magic number of core file */

	if ( (fp = fopen(core, "r")) == NULL )
	{	my_perror(core);
		return;
	}
	if ( fread(corhdr, sizeof(char), ctob(USIZE), fp) != ctob(USIZE) )
	{	my_perror(core);
		return;
	}
	magic = corhdr->u_exdata.ux_mag;
	txtsiz = ctob(corhdr->u_tsize);
	datsiz = ctob(corhdr->u_dsize);
	stksiz = ctob(corhdr->u_ssize);
	datmap.b1 = (magic==0410?round(txtsiz,TXTRNDSIZ):0);
	datmap.e1 = (magic == 0407 ? txtsiz : datmap.b1) + datsiz;
	datmap.f1 = ctob(USIZE);
#ifdef xelos
	datmap.b2 = stackbas(stksiz);
	datmap.e2 = stacktop(stksiz);
#else
	datmap.b2 = corhdr->u_sseg << 16;
	datmap.e2 = datmap.b2 + stksiz;
#endif
	datmap.f2 = ctob(USIZE) + (magic == 0410 ? datsiz : datmap.e1);
	endhdr = (char *) corhdr + ((int) corhdr->u_ar0 & 0xffff);
	++coremode;
}

core_end()
{
	if (! coremode) return;
	fclose(fp);
	coremode = 0;
}

/*
 *	return value of a register
 */

getreg(roffs)
	int	roffs;
{
	return endhdr[roffs];
}

readcore(addr, value)
	char * addr;
	long * value;
{
	return readm(fp, datmap, addr, value);
}
