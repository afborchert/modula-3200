/*
 *	MODULA 2 - debugger
 *
 *	(c) Andreas Borchert 1983, University of Ulm
 */

#include	<stdio.h>
#include	<sys/reg.h>

#define	USAGE	fprintf(stderr, usage, name), exit(1)

char	*usage = { "Usage: %s [a.out-file] [corefile | -]\n" };
char	*core  = { "core" };
char	*aout  = { "a.out" };

/*
 *	version 1:	word addressed
 *	version 2:	base at the begin of the activation record
 *	version 3:	links first; then parameters
 */

int version = 0;	/* Compiler-Version */
int fflag = 0;		/* if on: no command mode; print backtrace only */

main(argc, argv)
	int	argc;
	char	**argv;
{	char	*name;
	long	addr;
	long	value;

	name = *argv;
	while ( --argc && **++argv == '-' ) switch ( *++*argv ) {
	case 'v' :
		if (*++* argv)
			version = atoi(* argv);
		else if (--argc && **++argv)
			version = atoi(* argv);
		else
			USAGE;
		if (version < 0 || version > 3)
		{	fprintf(stderr, "Unkown version: %d\n", version);
			exit(1);
		}
		break;
	case 'f' :
		++fflag;
		break;
	default:
		USAGE;
	}
	if (argc)
	{	--argc;
		aout = *argv++;
	}
	if (argc)
	{	--argc;
		core = *argv++;
	}
	aout_init(aout);
#ifndef DEBUG_AOUT
	if (! fflag)
		init(); /* of screen */
	if (strcmp(core, "-"))
		core_init(core);
	if (fflag)
		fast();
	else
		cmdmode();
#endif DEBUG_AOUT
}

#ifdef xelos

extern char * malloc();
extern int strlen();

char * strsave(s)
	char * s;
{	char * cp;

	if ((cp = malloc(strlen(s)+1)) == NULL)
	{	fprintf(stderr, "Not enough memory\n");
		exit(1);
	}
	strcpy(cp, s);
	return cp;
}
#endif
