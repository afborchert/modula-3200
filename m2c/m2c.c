/*
 *	m2c - compile MODULA-2
 *
 *	afb 3/84
 *	rev afb 5/84 : output generally on stderr
 *	rev afb 6/84 : listings
 *	rev afb 11/84 : MODPATH & MODLIB
 *	rev afb 4/86 : xelos
 */

/*
 *	define ...
 *
 *	LOCAL		for local version (living in $HOME/lib)
 *	PROG		= group id of beginners
 *	MODLIB		for respecting the environment parameter MODLIB
 *	MODPATH		for respecting the environment parameter MODPATH
 *	ULM		if compiler is running in ULM
 *	BATCH		if compiler runs in batch
 */

#if defined(ULM) && !defined(xelos)
#define	BATCH
#define	PROG	3
#endif

#ifdef xelos
#define	AS_BUG			/* output arg of as will be cut to 14 chars */
#endif


/*
 *	for compiler transportation change
 *
 *	libdir	(here are m0, m1, m2, m3, msym)
 *
 *	environment parameters
 *
 *	MODLIB		(here are m0, m1, ...; overrides libdir)
 *	MODPATH		search path of directories
 *			for symbolfiles, referencefiles and libs.
 */

#ifdef	LOCAL
#define	MODLIB	"LMODLIB"
#define	MODPATH	"LMODPATH"
#else
#define	MODLIB	"MODLIB"
#define	MODPATH	"MODPATH"
#endif

#include	<stdio.h>
#include	<ctype.h>
#include	<signal.h>
#include	<a.out.h>
#include	<sys/types.h>
#include	<sys/dir.h>
#include	<sys/stat.h>
#include	<time.h>
#include	<errno.h>
#include	<string.h>
#ifdef	xelos
#define	index(s,c)	strchr(s,c)
#define	rindex(s,c)	strrchr(s,c)
#endif
#ifdef	xelos
#include	<fcntl.h>
#endif

extern char * calloc();
extern char * malloc();
extern char * getenv();

char * setsuf();
char * alloc();
char * scandir();
char * search_lib();
char * get_libdir();
char * test_lib();
char * basename();
char * gettmp();	/* get temporary file name */
char * callm();		/* schedule Modula-2 Compiler */
int isr();		/* interrupt service routine */


#define	MAXARG	512

/*
 *	used commands
 */

#ifndef xelos
char	*mstrip = { "/usr/bin/mstrip" };
#endif
char	*as  = { "/bin/as" };
char	*ld  = { "/bin/ld" };
char	*ar  = { "/bin/ar" };
#if	defined(xelos) && defined(ULM)
char	*pr  = { "/u/bin/pr" };
#else
char	*pr  = { "/bin/pr" };
#endif

/*
 *	passes of the compiler (in libdir)
 *
 *	exit codes of the passes:
 *
 *	0 : okay
 *	1 : errors occured, nevertheless start next pass
 *	2 : stop the compilation and run lister
 *	3 : stop the compilation -- error messages have been printed
 */

char	m0[]    = { "m0" };
char	m1[]	= { "m1" };
char	m2[]	= { "m2" };
char	m3[]	= { "m3" };
char	msym[]	= { "msym" };
char	m2e[]	= { "m2e" };

char	*passes[] = { m0 , m1 , m2 , m3 , msym , NULL };
#if	defined(xelos) && defined(ULM)
char	*libdir = { "/u/lib/modula/" };
#else
char	*libdir = { "/usr/lib/modula/" };
#endif
char	*m2lib;
char	*rts_prefix = { "" };
char	*rts[]  = { "m2rt0.o", NULL };
char	*mrts[] = { "mm2rt0.o", NULL };
char	*symlib = "SYM";
char	*reflib = "REF";
char	*prefix = NULL;
char	*ld_args[MAXARG];	/* pick up args for ld */
char	**ptr = ld_args;	/* points to current arg of ld */
char	*symfiles[MAXARG];	/* all symbol files in arg list */
char	**sym_ptr = symfiles;

/*
 *	default temp directory
 *	may be changed with the -T option
 */

char	*tmpdir	= { "/tmp" };


/*
 *	level of temp files
 */

#define	ALL_TMP	3
#define	LNK_TMP	2
#define	ASS_TMP	1
#define	CMP_TMP	0

#define	VARARGS	_p1, _p2, _p3, _p4, _p5, _p6

#define	USAGE	fprintf(stderr, usage, name), die(100)


/*
 *	usage not complete but typical
 */

char usage[] = { "Usage: %s [-o outfile] [-S] [-c] [-L | -LP] [-Ttmpdir] [option...] file...\n" };
char	*name;

/*
 *	flags
 */

int	main_flag = 0;	/* if == 1: take following arg as main module */
int	Bflag = 0;	/* if on: use backup version */
int	aflag = 0;	/* if on: don't archive anything */
#ifndef xelos
int	kflag = 0;	/* if on: stack size is `stack_size' */
int	stack_size;
#endif
int	sflag = 0;	/* run compiler only */
#ifndef	xelos
int	strip = 0;	/* 1: run mstrip after linkage */
#endif
int	cflag = 0;	/* no linkage */
int	pflag = 0;	/* if on: profile */
#ifdef	xelos
int	Mflag = 1;
#else
int	Mflag = 0;	/* if on: no stack checks */
#endif
int	Cflag = 0;	/* if on: no range checks */
int	Nflag = 0;	/* if on: generate labels with line numbers */
int	vflag = 0;	/* verbose flag */
int	Vflag = 0;	/* no execution, verbose output only */
int	Rflag = 0;	/* 1: don't remove anything */
int	revision = 2;	/* Modula-2 version to be taken */
int	Lflag = 0;	/* if on: produce listing */
int	LPflag = 0;	/* if on: produce listing on stdout */
#ifndef	xelos
int	stack_seg = 0;	/* if on: use UNIX stack segment for Modula-2 stack */
#endif
int	loading = 0;	/* if on: load compiler into swap area */
#ifdef xelos
int	Xelos = 1;	/* if on: produce code for XELOS */
#else
int	Xelos = 0;	/* if on: produce code for XELOS */
#endif
char	* outfile = "a.out";	/* for -o option */
FILE	*listp = NULL;	/* pipeline to `pr' */
char * listing = NULL;	/* listing file */
int	SYM_found = 0;	/* if on: aflag isn't on and SYM exists */
int	ignore = 0;	/* if on: continue compiling further files;
                           even if previous compilation fails */

main(argc, argv)
	int	argc;
	char	**argv;
{	char * cp;


#ifdef BATCH
	/*
	 *	in batch m2c is called with effective uid = 0
	 */
	setuid(getuid());
	setgid(getgid());
#endif BATCH

	(name = rindex(* argv, '/')) ? ++name : (name = * argv);

	if (signal(SIGINT, SIG_IGN) == SIG_DFL)
	{	signal(SIGINT, isr);
		signal(SIGQUIT, isr);
		signal(SIGHUP, isr);
	}
	else
		signal(SIGHUP, SIG_IGN);
	signal(SIGTERM, isr);
	signal(SIGPIPE, isr);
#ifdef	BATCH
	signal(SIGALRM, SIG_IGN);		/* may be caused by m2ckick */
#endif	BATCH

	setbuf(stderr, NULL);
	if (argc == 1)
		USAGE;
	*ptr++ = ld;
#ifdef xelos
	*ptr++ = "-N";	/* overwrite default of ld */
#endif

#ifdef PROG
	/* switch another runtime start for beginners */
	if (getgid() == PROG)
		rts_prefix = "p";
#endif

	while ( --argc && **++argv == '-' ) { switch ( *++*argv ) {
	default :
		/* pass argument to ld */
		*ptr++ = --*argv;
		continue;
	case 'o' :
		*ptr++ = "-o";
		if (--argc)
		{	outfile = *++argv;
			*ptr++ = outfile;
		}
		else
			USAGE;
		continue;
	case 'l' :
	case 'm' :
		--*argv;
		break;
	case 'S' :
		++sflag;
		++cflag;
		continue;
#ifndef xelos
	case 's' :
		++strip;
		continue;
#endif
	case 'c' :
		++cflag;
		continue;
	case 'p' :
		++pflag;
#ifdef PROG
		/* reset default value for rts_prefix */
		rts_prefix = "";
#endif
		continue;
	case 'M' :
#ifdef xelos
		Mflag = 0;
#else
		Mflag = 1;
#endif
		continue;
	case 'C' :
		++Cflag;
		continue;
	case 'L' :
		++Lflag;
		switch(*++* argv) {
		case 'O' :	/* load compiler into swap area */
				die(load_compiler(++* argv));
		case 'P' :	++LPflag; break;
		case '\0' :	break;
		default :	USAGE;
		}
		continue;
	case 'N' :
		++Nflag;
		continue;
	case 'V' :
		++Vflag;
	case 'v' :
		++vflag;
		continue;
	case 'R' :
		++Rflag;
		continue;
	case 'T' :
		if (*++* argv)
			tmpdir = *argv;
		else
			USAGE;
		continue;
	case 'B' :	/* backup version */
		++Bflag;
		if (*++* argv)
			prefix = *argv;
		else
			prefix = "old";
		continue;
	case '0' :	/* prefix of m2rt0.o */
		if (*++* argv)
			rts_prefix = *argv;
		else
			rts_prefix = "";
#ifndef xelos
		stack_seg = strcmp(rts_prefix, "s") == 0;
		if (stack_seg)
			Mflag = 1;	/* no checks for stack overflow */
		continue;
#endif
#ifndef	xelos
	case 'k' :	/* stack size */
		++kflag;
		if (*++* argv)
			stack_size = atoi(* argv);
		else
			USAGE;
		continue;
#endif
	case 'a' :
		++aflag;
		continue;
	case 'r' :
		if (*++* argv)
		{	revision = atoi(* argv);
			if (revision < 0 || revision > 2)
				USAGE;
		}
		else
			revision = 0;
		continue;
	case 'i' :
		++ignore;
		continue;
	case '7' :
		Xelos = 0;
		continue;
	case 'X' :
		Xelos = 1;
		continue;
	} break; }
	if (! argc)
		USAGE;
#ifdef xelos
	if (! Xelos)
#else
	if (Xelos)
#endif
	{	++sflag;	/* cross compiling: don't use as or ld */
		++cflag;
	}

	/*
	 *	look for environment parameters
	 */

#ifdef	LOCAL
	cp = getenv("HOME");
	if (cp)
	{	libdir = alloc(strlen(cp) + 4);
		strcpy(libdir, cp);
		strcat(libdir, "/lib/");
	}
#endif	LOCAL

#ifdef MODLIB
	if (cp = getenv(MODLIB))
	{	if (e_access(cp, /* execute = */ 1))
		{	fprintf(stderr, "Warning: MODLIB parameter ignored: ");
			perror(cp);
		}
		else
		{	libdir = alloc(strlen(cp) + /* "/" */ 1);
			strcpy(libdir, cp);
			strcat(libdir, "/");
		}
	}
#endif MODLIB


        m2lib = alloc(strlen(get_libdir()) + /* plibm2.a */ 9);
	strcpy(m2lib, get_libdir());
	if (pflag)
		strcat(m2lib, "plibm2.a");
	else
		strcat(m2lib, "libm2.a");

	die(compile(argc, argv));
}	/* main */

/*
 *	scheduler
 *	return 0 if ok
 */

compile(argc, argv)
	int	argc;
	char	**argv;
{	char	*args[MAXARG];
	int	argi;
	char	*cp;			/* only temporary used */
	register char **rts_ptr;	/* -> list of first args of ld */
	register int  libdirlen;
	char * out;			/* output file of compilation */
	int objects = 0;		/* # objects for ld */
	int h_argc;
	char ** h_argv;
	int exit_state = 0;		/* only if ignore is on */
#ifdef MODPATH
	int fd;
	char * modpath;
#endif MODPATH

	h_argc = argc;
	h_argv = argv;
	while (h_argc)
	{	if (**h_argv != '-')
		{	if (test_filename(* h_argv, NULL))
				if (ignore)
					++exit_state;
				else
					return 1;
			else if (! test_sym(* h_argv))
				* sym_ptr++ = * h_argv;
			switch (getsuf(*h_argv))
			{	case 'm' :
				case 'o' :
				case 's' :
					if (! cflag && !sflag)
						++objects;
					break;
			}
		}
		++h_argv, --h_argc;
	}

	symbolfile_list();
#ifdef xelos
	*ptr++ = "-VS";	/* version stamp of compiler */
/***	*ptr++ = "1";	*** is now used by cc... */
	*ptr++ = "2001";
#else
	*ptr++ = "-X";
#endif
#ifndef xelos
	if (kflag && stack_seg)
	{	*ptr++ = "-k";
		if((*ptr = malloc(8)) == NULL)
			perror(name), die(1);
		sprintf(*ptr++, "%d", stack_size);
	}
#endif
	if (! cflag)
	{	for ( rts_ptr = pflag ? mrts : rts ; *rts_ptr ; ++rts_ptr )
		{	* ptr = alloc(strlen(* rts_ptr) + strlen(rts_prefix)
					+ strlen(get_libdir()));
			strcpy(* ptr, get_libdir());
			strcat(*ptr, rts_prefix);
			strcat(*ptr, *rts_ptr);
			++ptr;
		}
	}
	do	/* for all arguments ... */
	{	if (**argv == '-') switch (*++*argv) {
		case 'l' :
			*ptr++ = test_lib(--*argv);
			break;
		case 'm' :
			if (main_flag++ == 2)
			{	fprintf(stderr, "%s: -m specified twice\n",
					name);
				die(100);
			}
			break;
		case 'i' :
			++ignore;
			break;
		default :
			USAGE;
		}
		else if (! test_sym(* argv))
			;	/* symbolfile already entered in list */
		else switch (getsuf(*argv)) {
		case 'o' :
		case 'a' :
			if (! sflag && ! cflag)
				*ptr++ = *argv;
			break;
		case 's' :
			/* cannot be `.sy' */
			if (sflag)
				break;
			argi = 0;
			args[argi++] = as;
#ifdef xelos
			args[argi++] = "-Y";
#endif xelos
#ifdef AS_BUG
			*ptr++ = basename(setsuf(* argv, "o"));
#else
			args[argi++] = "-o";
			args[argi++] = setsuf(*argv, "o");
			*ptr++ = args[argi-1];	/* pick up object for ld */
#endif
			args[argi++] = *argv;
			args[argi++] = NULL;
			if ( callsys(as, args) )
				if (ignore)
					++exit_state;
				else
					return 1;
			break;
		case 'd' :
			/*
			 *	compile definition module
			 */

			if (test_filename(* argv, ".d") || callm(* argv) == NULL)
				if (ignore)
				{	++exit_state;
					break;
				}
				else
					return 1;
			/* exist SYM-library ??? */
			if (SYM_found ||
			    !aflag && e_access(symlib, /* write = */ 2) == 0)
			{	args[0] = ar;
				args[1] = "r";
				args[2] = symlib;
				args[3] = setsuf(*argv, "sy");
				args[4] = NULL;
				if (callsys(ar, args) == 0)
					remove(args[3]);
				SYM_found = 1;
			}
			break;
		case 'm' : /* .m2 */
			/* check for `.m2' */
			if (test_filename(* argv, ".m2"))
				if (ignore)
				{	++ exit_state;
					break;
				}
				else
					return 1;

			/*
			 *	compile
			 */

			if (! (out = callm(* argv)))
				if (ignore)
				{	++ exit_state;
					break;
				}
				else
					return 1;

			/*
			 *	if REF-library exists try to archive
			 *	the reference file
			 */

			if (!aflag && e_access(reflib, /* write = */ 2) == 0)
			{	args[0] = ar;
				args[1] = "r";
				args[2] = reflib;
				args[3] = setsuf(* argv, "r");
				args[4] = NULL;
				if (callsys(ar, args) == 0)
					remove(args[3]);
			}

			if (sflag) /* run the compiler only */
				break;

			/*
			 *	assembly
			 */

			argi = 0;
			args[argi++] = as;
#ifdef xelos
			args[argi++] = "-Y";
#endif
			args[argi++] = "-o";
			if (objects == 1)
				args[argi++] = gettmp(LNK_TMP);
			else
				args[argi++] = setsuf(*argv, "o");
			*ptr++ = args[argi-1];	/* pick it up for linkage */
			args[argi++] = out;
			args[argi++] = NULL;
			if (callsys(as, args))
			{	fprintf(stderr,
				   "%s: error in compiler output\n", * argv);
				copy(out, setsuf(* argv, "s"));
				return 1;
			}
			
			rmall(ASS_TMP);
			break;
		default :
			fprintf(stderr,"%s: unknown suffix\n", *argv);
			if (ignore)
				++exit_state;
			else
				return 1;
		}
 	}
	while (--argc && **++argv);
	if (exit_state)
		return exit_state;
	if (cflag)
		return 0;
#ifndef	xelos
	if (kflag && ! stack_seg)
	{	args[0] = as;
		args[1] = "-o";
		args[2] = gettmp(LNK_TMP);
		args[3] = gettmp(ASS_TMP);
		args[4] = NULL;
		if (sizefile(args[3], stack_size))
			return 1;
		if (callsys(as, args))
			return 1;
		*ptr++ = args[2];	/* object */
		rmall(ASS_TMP);
	}
#endif

	/*
	 *	enter libraries into the arglist of ld
	 */

#ifdef MODPATH
	if (modpath = getenv(MODPATH))
		while (1)
		{	if (cp = index(modpath, ':'))
				* cp = '\0';
			while (* ptr = search_lib(modpath))
				++ ptr;
			if (cp)
			{	* cp = ':';
				modpath = cp + 1;
			}
			else
				break;
		}
#endif MODPATH

	/*
	 *	standard library
	 */

	*ptr++ = m2lib;

	*ptr = NULL;
	if (objects == 0)
		return 0;
	if (callsys(ld, ld_args))
		return 1;
	rmall(LNK_TMP);
#ifndef xelos
	if (strip)
	{	args[0] = mstrip;
		args[1] = outfile;
		args[2] = NULL;
		if (callsys(mstrip, args))
			return 1;
	}
#endif
	return 0;
}	/* compile */

#ifndef xelos
sizefile(file, size)
	char * file;
	int size;
{	FILE * fp;

	if (Vflag)
		return 0;
	if ((fp = fopen(file, "w")) == NULL)
	{	perror(file);
		return 1;
	}
	if (size % 4)		/* align size */
		size += 4 - size % 4;
	fprintf(fp, "\timpur\n");
	fprintf(fp, "\tentry\t.size\n");
	fprintf(fp, ".size\tdcf\t%d\n", size);
	fprintf(fp, "\tend\n");
	fclose(fp);
	return 0;
}
#endif

/*
 *	call the modula-2 compiler
 *	return 0 if ok
 *
 *	for argument syntax see MCP?Public.m2, PROCEDURE Usage;
 *
 */

char * callm(src)
	char * src;			/* source file */
{	char * il1, * il2, * asc;	/* interpass files */
	char * storage;			/* saved dynamic storage */
	char * out, * ref;		/* output files */
	char * args[MAXARG];
	int argi;
	int pass;			/* 0..3 */
	int okay = 1;
	int defmod;			/* if on: src is a definition mod */
	char buf[64];			/* libdir + passname */
	int errors = 0;			/* if on: call m2e after compilation */
	int new_error = 0;
	int abort = 0;			/* if on: abort compilation */
	char * callm = NULL;		/* output file */
	char flags[10];
	char * cp;			/* only temporarily used */
	char ** sp;			/* -> symfiles */

	if (! Vflag && e_access(src, /* read = */ 4))
	{	perror(src);
		return callm;
	}
	defmod = getsuf(src) == 'd';
	if (Lflag)
	{	if (! LPflag)
			if (defmod)
				listing = setsuf(src, "ld");
			else
				listing = setsuf(src, "l");
		header(src);
	}
	storage = gettmp(CMP_TMP);
	il1 = gettmp(CMP_TMP);
	il2 = gettmp(CMP_TMP);
	asc = gettmp(CMP_TMP);
	if (! defmod)
		ref = gettmp(CMP_TMP);
	if (defmod)
		out = setsuf(src, "sy");
	else if (! sflag)
		out = gettmp(ASS_TMP);
	else
		out = setsuf(src, "s");

	init_comp(storage);		/* initialize storage file */

	for (pass = 0; !abort && pass < 4; ++pass)
	{	if (Lflag)
			lprintf("p%d\n", pass+1);
		args[1] = storage;	/* always */
		switch(pass) {
		case 0 :
			args[0] = m0;
			argi = 2;
			flags[0] = '-';
			flags[1] = '\0';
			if (revision == 1)
				strcat(flags, "r");
			else if (revision == 2)
				strcat(flags, "2");
			if (Lflag)
				strcat(flags, "L");
			if (main_flag == 1)
			{	strcat(flags, "m");
				++main_flag;
			}
			if (Xelos)
				strcat(flags, "x");
			if (flags[1])
				args[argi++] = flags;
			args[argi++] = src;
			args[argi++] = il1;
			args[argi++] = asc;

			for (sp = symfiles; sp < sym_ptr; ++sp)
				args[argi++] = * sp;
			args[argi++] = NULL;
			break;
		case 1 :
			args[0] = m1;
			args[2] = il1;
			args[3] = il2;
			args[4] = asc;
			if (! defmod)
				args[5] = ref;
			else
				args[5] = NULL;
			args[6] = NULL;
			break;
		case 2 :
			if (! defmod)
			{	args[0] = m2;
				args[2] = il1;
				args[3] = il2;
				args[4] = asc;
				args[5] = NULL;
			}
			else /* definition module */
			{	args[0] = msym;
				args[2] = asc;
				args[3] = out;
				args[4] = NULL;
				++pass; /* no pass 4 */
			}
			break;
		case 3 :
			args[0] = m3;
			argi = 2;
			flags[0] = '-';
			flags[1] = '\0';
			if (Cflag)
				strcat(flags, "R");
			if (Mflag)
				strcat(flags, "s");
			if (pflag)
				strcat(flags, "p");
			if (sflag)
				strcat(flags, "S");
			if (! Nflag)
				strcat(flags, "l");
			if (flags[1])
				args[argi++] = flags;
			if (Xelos)
				args[argi++] = basename(src);
			args[argi++] = il1;
			args[argi++] = out;
			args[argi++] = il2;
			args[argi++] = NULL;
			break;
		}
		strcpy(buf, get_libdir());
		strcat(buf, args[0]);
		args[0] = buf;
		new_error = 0;
		switch (callsys(buf, args))
		{	case 0 :
				/* do nothing */
				break;
			case 1 :
				++errors;
				++new_error;
				break;
			default :
				++abort;
				++errors;
				++new_error;
				break;
		}
		if (pass == 2 && errors)
			++abort;
		else if (pass == 1 && defmod && errors)
			++abort;
		if (new_error && Lflag)
			if (pass == 0 && abort)
				lprintf(" --- symbolfiles missing\n");
			else
				lprintf(" --- error\n");
		if (abort)
			break;
	} /* for */

	/* clean up */

	if (! abort)
		--pass;
	if (Lflag)
		lprintf("lister\n");
	if (errors)
	{	fprintf(stderr, "errors in %s", src);
		if (! Lflag)
			fprintf(stderr, ":");
		fprintf(stderr, "\n");
		call_m2e(pass, src, il1, il2);
	}
	else
	{	if (! defmod)
			copy(ref, setsuf(src, "r"));
		else if (! SYM_found)
			* sym_ptr++ = setsuf(src, "sy");
		if (Lflag)
		{	pass = 0;
			call_m2e(pass, src, il1, il2);
		}
		callm = out;
	}
	if (Lflag)
		lprintf("end compilation\n");
	rmall(CMP_TMP);
	listing = NULL;
	if (listp)
	{	pclose(listp);
		listp = NULL;
	}
	return callm;
}

/*
 *	print header of listing
 */

header(src)
	char * src;
{	FILE * fp;
	struct stat st_buf;
	struct tm * tm_buf;
	time_t today;
	static time_t compiler = 0;
	char ** cpp;
	char buf[128];
	char header[80];

	if (Vflag) return;

	/*
	 *	get time of compiler version
	 */
	if (! compiler)
		for (cpp = passes; * cpp; ++cpp)
		{	strcpy(buf, get_libdir());
			strcat(buf, * cpp);
			if (! stat(buf, & st_buf) && st_buf.st_mtime > compiler)
				compiler = st_buf.st_mtime;
		}
	tm_buf = localtime(& compiler);
	sprintf(header, "%s %2d.%02d.%02d ****",
#ifdef LOCAL
		"**** Modula-2 Compiler ---- local version ---",
#else
#ifdef	xelos
		"**** Modula-2 Compiler ---- Version 1.01 X ----",
#else
		"**** Modula-2 Compiler ---- Version 1.01 ----",
#endif	xelos
#endif LOCAL
		tm_buf->tm_mday,
		tm_buf->tm_mon+1, tm_buf->tm_year);
	if (LPflag)
	{	char buf[128];

		sprintf(buf, "%s -h '%s \"%s\"'", pr, header, src);
		if ((listp = popen(buf, "w")) == NULL)
		{	perror(pr);
			die(100);
		}
		fp = listp;
	}
	else
	{	if ((fp = fopen(listing, "w")) == NULL)
			perror(listing), die(100);
		fprintf(fp, "%s\n\n", header);
	}
	if (! LPflag)
	{	/*
		 *	get actual time
		 */
		time(& today);
		tm_buf = localtime(& today);
		fprintf(fp, "**** Time: %2d:%02d:%02d", tm_buf->tm_hour,
			tm_buf->tm_min, tm_buf->tm_sec);
		fprintf(fp, "                     ");
		fprintf(fp, "Date: %2d.%02d.%02d ****\n\n", tm_buf->tm_mday,
			tm_buf->tm_mon+1, tm_buf->tm_year);
	}
	if (Bflag)
		fprintf(fp, "**** backup version: >%s<\n", prefix);
#ifndef	xelos
	if (kflag)
		fprintf(fp, "**** stack size = %d\n", stack_size);
#endif	xelos
	if (pflag)
		fprintf(fp, "**** profiling\n");
	if (Bflag ||
#ifndef	xelos
	    kflag ||
#endif
	    pflag)
		fprintf(fp, "\n");
	fprintf(fp, " source: %s\n", src);
	if (! LPflag)
		fclose(fp);
}

/*
 *	append line to listing file
 */

lprintf(VARARGS)
{	FILE * fp;

	if (Vflag) return;
	if (LPflag)
		fprintf(listp, VARARGS);
	else
	{	if ((fp = fopen(listing, "a")) == NULL)
			perror(listing), die(100);
		fprintf(fp, VARARGS);
		fclose(fp);
	}
}

/*
 *	create storage file for the first pass
 */

init_comp(storage)
	char * storage;
{	FILE * fp;
	char * m3_file;
	static long csize = 0;
#ifdef	xelos
	struct aouthdr x_buf;
#define	SIZE_XBUF	sizeof(struct aouthdr)
#else
	struct exec x_buf;
#define	SIZE_XBUF	sizeof(struct exec)
#endif

	if (Vflag) return;
	if (! csize)
	{	m3_file = alloc(strlen(get_libdir()) + strlen(m3));
		strcpy(m3_file, get_libdir());
		strcat(m3_file, m3);
		if ((fp = fopen(m3_file, "r")) == NULL)
			csize = 150000; /* default value */
#ifdef	xelos
		else if (fseek(fp, sizeof(struct filehdr), 0) == -1)
		{	perror(m3_file); die(100);
		}
#endif xelos
		else if (! fread(& x_buf, SIZE_XBUF, 1, fp))
		{	perror(m3_file); die(100);
		}
		else
		{	fclose(fp);
#ifdef	xelos
			csize = x_buf.tsize + x_buf.dsize + x_buf.bsize;
#else
			csize = (long) x_buf.a_text + (long) x_buf.a_data + (long) x_buf.a_bss;
#endif
			/* align size */
			csize += 3;
			csize /= 4;
			csize *= 4;
		}
	}
	if ((fp = fopen(storage, "w")) == NULL)
	{	perror(storage);
		die(100);
	}

	/*
	 *	write header of storage file :
	 *
	 *	i MUST be > MAX("end"-address of all passes)
	 *	(typically this maximum will be reached in m3)
	 */

	fwrite(& csize, sizeof(int), 1, fp);
	fwrite(& csize, sizeof(int), 1, fp);
	fclose(fp);
}

/*
 *	call m2e in dependency of the last pass
 */

call_m2e(pass, src, il1, il2)
	int pass;	/* pass number, ranges from 0 to 3 */
	char * src;	/* source file */
	char * il1, * il2;	/* interpass files */
{	char * args[6];
	int argi = 0;
	char * lib_m2e;

	if ((lib_m2e = malloc(strlen(libdir) +
			      (prefix ? strlen(prefix)+1 : 0) +
			      strlen(m2e) + 1)) == NULL)
	{	perror("malloc"); die(1);
	}
	strcpy(lib_m2e, libdir);
	if (prefix)
	{	strcat(lib_m2e, prefix);
		strcat(lib_m2e, "/");
	}
	strcat(lib_m2e, m2e);

	args[argi++] = lib_m2e;
	if (pass == 3)
		args[argi++] = "-4";
	if (Lflag)
		args[argi++] = "-L";
	args[argi++] = src;
	if (pass > 0)
		args[argi++] = pass % 2 ? il2 : il1;
	/* else symbolfiles missing */
	args[argi++] = NULL;
	callsys(lib_m2e, args);	/* ignore results */
}

/*
 *	test if library is in lib-directory
 *	and expand '-lxxx' to 'libdir/libxxx.a'
 */

char * test_lib(option)
	char * option;
{	char * libdir;
	char * lib;

	libdir = get_libdir();
	lib = alloc(strlen(libdir) + strlen(option) + /* lib */ 3+ /* .a */ 2);
	strcpy(lib, libdir);
	strcat(lib, "lib");
	option += 2;	/* scan -l */
	strcat(lib, option);
	strcat(lib, ".a");
	if (e_access(lib, /* read = */ 4))
		return option;
	return lib;
}

/*
 *	test if file ends in `SYM' or `.sy'
 *	return 0 if ok
 */

test_sym(file)
	char * file;
{	char * cp;

	(cp = rindex(file, '/')) ? ++cp : (cp = file);
	if (strcmp(cp, symlib) == 0)
		return 0; /* ok */
	if ((cp = rindex(cp, '.')) && strcmp(cp, ".sy") == 0)
		return test_filename(file, ".sy");
	return 1; /* not ok */
}

/*
 *	check string length of basename of filename <= DIRSIZ-3
 *	suffix may be NULL
 *	return 0 if ok
 */

test_filename(file, suffix)
	char * file;
	char * suffix;
{	char * cp;
	char * f_suffix;

	(cp = rindex(file, '/')) ? ++cp : (cp = file);
	if (! suffix && strcmp(cp, symlib) == 0)
		return 0; /* ok */
	if (! suffix)
		if(! (suffix = rindex(cp, '.')))
			suffix = "";
	if (strlen(cp) > DIRSIZ)
	{	fprintf(stderr, "%s: Name too long", file);
		strcpy(cp+DIRSIZ-3, suffix);
		fprintf(stderr, "; name your file better %s\n", cp);
		return 1; /* not ok */
	}
	cp = rindex(cp, '.');
	if (! cp)
	{	fprintf(stderr, "%s: suffix missing\n", file);
		return 1;
	}
	else if (strcmp(cp, suffix))
	{	fprintf(stderr, "%s: incorrect suffix", file);
		strcpy(cp, suffix);
		fprintf(stderr, "; name your file better %s\n", file);
		return 1;
	}
	return 0;	/* ok */
}

/*
 *	return library directory = libdir + prefix
 */

char * get_libdir()
{	static char * dir = NULL;

	if (dir)
		return dir;
	if (prefix)
	{	dir = alloc(strlen(libdir) + strlen(prefix));
		strcpy(dir, libdir);
		strcat(dir, prefix);
		strcat(dir, "/");
	}
	else
		dir = libdir;
	return dir;
}

/*
 *	search for `.a' file in specified directory
 */
char * search_lib(dir)
	char * dir;
{
	if (pflag)
		return scandir(dir, "p", "a");
	else
		return scandir(dir, "^p", "a");
}

/*
 *	get all files with prefix `prefix' (or files not starting with
 *	`prefix', if the first char of prefix is '^') and
 *	suffix `suffix' in directory `dir'
 *	prefix (but not suffix) may be NULL
 */

char * scandir(dir, prefix, suffix)
	char * dir;
	char * prefix;
	char * suffix;
{	struct direct dirbuf;
	static FILE * fp = NULL;	/* for reading the directory */
	static char * old_dir = NULL;	/* last dir argument */
	char * file;			/* qualified filename in dir */
	char * cp;			/* cp -> suffix of file in dir */
	int neg = 0;			/* if on: 1st char of prefix was '^' */

	if (dir != old_dir)
	{	if (fp)
			fclose(fp);
		if ((fp = fopen(dir, "r")) == NULL)
		{	perror(dir);
			return NULL;
		}
		old_dir = dir;
	}

	if (prefix && * prefix == '^')
	{	++ neg;
		++ prefix;
	}
	while (fread(& dirbuf, sizeof(struct direct), 1, fp))
	{	if (! dirbuf.d_ino)
			continue;
		for (cp = dirbuf.d_name+DIRSIZ-1; cp >= dirbuf.d_name; --cp)
			if (* cp == '.')
				break;
		if (* cp != '.')
			continue;
		++cp;
		if (strncmp(cp, suffix, dirbuf.d_name+DIRSIZ-cp))
			continue;
		if (prefix)
			if (strncmp(prefix, dirbuf.d_name, strlen(prefix)))
			{	if (! neg)
					continue;
			}
			else if (neg)
				continue;
		file = alloc(strlen(dir)+DIRSIZ+1);
		strcpy(file, dir);
		strcat(file, "/");
		strncat(file, dirbuf.d_name, DIRSIZ);
		if (e_access(file, /* read = */ 4))
			continue;
		return file;
	}
	fclose(fp);
	old_dir = NULL;
	fp = NULL;
	return NULL;
}

/*
 *	get symbol file list from MODPATH and standard places
 */

symbolfile_list()
{	char * modpath;
	char * cp;
	char * dir;
	char * file;

#ifdef MODPATH
	if (modpath = getenv(MODPATH))
		while (1)
		{	if (cp = index(modpath, ':'))
				* cp = '\0';
			dir = alloc(strlen(modpath)+DIRSIZ+1);
			if (modpath[0] == '\0')
				strcpy(dir, ".");
			else
				strcpy(dir, modpath);
			file = alloc(strlen(dir)+1+strlen(symlib));
			strcpy(file, dir);
			strcat(file, "/");
			strcat(file, symlib);
			if (! e_access(file, 4))
				* sym_ptr++ = file;
			while (* sym_ptr = scandir(dir, NULL, "sy"))
				++sym_ptr;
			if (cp)
			{	* cp = ':';
				modpath = cp + 1;
			}
			else
				break;
		}
#endif MODPATH
	/* check for `SYM' file */
	if (! e_access(symlib, /* read = */ 4))
		* sym_ptr++ = symlib;
	/* check for libdir/SYM file */
	cp = alloc(strlen(get_libdir()) + strlen(symlib));
	strcpy(cp, get_libdir());
	strcat(cp, symlib);
	if (! e_access(cp, /* read = */ 4))
		* sym_ptr++ = cp;
}

/*
 *	string allocation routine with check
 *	`length' means string length; not size of string
 */

char * alloc(length)
	int length;
{	char * cp;

	if ((cp = calloc(length+1, sizeof(char))) == NULL)
	{	perror(name);
		die(100);
	}
	return cp;
}

#ifdef	xelos
char * strsave(s)
	char * s;
{	char * cp;

	cp = alloc(strlen(s));
	strcpy(cp, s);
	return cp;
}
#endif	xelos

/*
 *	if the compiler runs with sticky bit
 *	it should be loaded after booting the system.
 */

load_compiler(arg)
	char * arg;
{	char buf[BUFSIZ];
	char * file;
	char * pass;
	char * storage;
	char * args[5];

	strcpy(buf, libdir);
	if (prefix)
	{	strcat(buf, prefix);
		strcat(buf, "/");
	}
	file = buf+strlen(buf);
	storage = gettmp(CMP_TMP);
	init_comp(storage);
	loading = 1;	/* needed by callsys */

	/* skip LOAD in arg */
	while (isupper(* arg))
		++arg;
	if (! * arg)
		arg = "0123se";	/* all passes */
	while (* arg)
	{	switch (* arg++)
		{	case '0' : pass = m0; break;
			case '1' : pass = m1; break;
			case '2' : pass = m2; break;
			case '3' : pass = m3; break;
			case 's' : pass = msym; break;
			case 'e' : pass = m2e; break;
			default :
				fprintf(stderr, "Unknown pass: `%c'\n", *--arg);
				return 1;
		}
		strcpy(file, pass);
		args[0] = buf;
		args[1] = storage;
		args[2] = NULL;
		/* causes usage message and abort in each pass */
		callsys(buf, args);
	} /* while */
	return 0; /* ok */
}

/*
 *	execute file with args
 */

int pid = 0;	/* process id of son */

callsys(file, args)
	char *file, **args; 
{	int son, status, t;
	register char **ptr;
	int exit_code;
	int (*sigint)();
	int (*sigquit)();

	if (LPflag && listp)
		fflush(listp);
	switch (son = fork()) {
	case 0 :
		if (vflag)
		{	fprintf(stderr, "%s", *args);
			for (ptr = args+1; *ptr; ++ptr)
				fprintf(stderr, " %s", *ptr);
			fprintf(stderr, "\n");
		}
		if (Vflag)
			exit(0);
		if (Lflag && listing)
		{	int fd;

			fd = open(listing, /* write = */ 1);
			dup2(fd, /* stderr = */ 2);
			close(fd);
			lseek(2, 0L, 2); /* append */
			dup2(2, /* stdout = */ 1);
		}
		else if (LPflag && listp)
		{	int fd;

			fd = fileno(listp);
			dup2(fd, /* stderr = */ 2);
			close(fd);
			dup2(2, /* stdout = */ 1);
		}
		else if (loading)
		{	int fd;

			/* ignore usage messages */
			fd = open("/dev/null", /* write = */ 1);
			dup2(fd, /* stdout = */ 1);
			dup2(fd, /* stderr = */ 2);
			close(fd);
		}
		execvp(file, args);
		perror(file);
		die(100);
	case -1 :
		perror(name);
		die(100);
	default :
		sigint = signal(SIGINT, SIG_IGN);
		sigquit = signal(SIGQUIT, SIG_IGN);
		pid = son;
		while(son != wait(&status))
			;
		pid = 0;
		signal(SIGINT, sigint);
		signal(SIGQUIT, sigquit);
		if (t = status&0377)
		{	if (t == SIGINT || t == SIGQUIT)
				fprintf(stderr, "%s has been interrupted\n",
					file);
			else if (t == SIGILL || t == SIGFPE || t == SIGBUS ||
				 t == SIGSEGV || t == SIGSYS)
				fprintf(stderr, "Fatal error in %s\n", file);
			else if (t == SIGHUP)
				fprintf(stderr, "Hangup signal sent to %s\n",
					file);
			else if (t == SIGTERM)
				fprintf(stderr, "Termination signal sent to %s\n", file);
			else
				fprintf(stderr, "%s aborted\n", file);
			die(100);
		}
		exit_code = (status >> 8) & 0377;
		if (exit_code && vflag && ! Vflag)
			fprintf(stderr, "*** exit code = %d\n", exit_code);
		return exit_code;
	}
}

#ifdef xelos
/*
 *	dup2 doesn't exist under XELOS
 */

dup2(fd1, fd2)
	int fd1, fd2;
{	int newfd;

	if ((newfd = fcntl(fd1, F_DUPFD, fd2)) != fd2)
	{	close(fd2);
		return fcntl(fd1, F_DUPFD, fd2);
	}
	return fd2;
}
#endif	xelos

/*
 *	filename manipulation
 */

getsuf(filename)
	char	*filename;
{	char	*ptr;

	if ( (ptr = rindex(filename,'.')) == NULL )
		return NULL;
	else
		return *++ptr;
}

char * setsuf(filename, suf)
	char	*filename;
	char	*suf;
{	register char *ptr;

	if ( (ptr = malloc(strlen(filename)+strlen(suf)+1)) == NULL )
		perror(name), die(1);
	strcpy(ptr, filename);
	filename = ptr;
	if (ptr = rindex(filename, '.'))
		*++ptr = '\0';
	else
	{	ptr = filename + strlen(filename);
		*ptr = '.';
		*++ptr = '\0';
	}
	strcat(filename, suf);
	return filename;
}

char * basename(filename)
	char * filename;
{	char * cp;

	return (cp = rindex(filename, '/'))? ++cp: filename;
}

/*
 *	temp file handling
 */

struct chain {
	char * c_name;
	int c_level;
	struct chain * c_next;
} * tmpfiles = NULL;

char * gettmp(level)
	int level;
{	char buf[128];
	char * tmp;
	struct chain * new;
	static int unique = 0;
#ifdef	AS_BUG
	int len;
#endif	AS_BUG

	sprintf(buf, "%s/mtm%d_%d", tmpdir, getpid(), unique++);
#ifdef	AS_BUG
	if (level == LNK_TMP && (len = strlen(buf)) > DIRSIZ)
		if (len - DIRSIZ <= 4)
			sprintf(buf, "%s/%d%d", tmpdir, getpid(), unique);
		else
			sprintf(buf, "mtm%d_%d", getpid(), unique);
#endif	AS_BUG
	tmp = strsave(buf);
	if ((new = (struct chain *) calloc(sizeof(struct chain), 1)) == NULL)
	{	perror("calloc");
		die(100);
	}
	new->c_name = tmp;
	new->c_next = tmpfiles;
	new->c_level = level;
	tmpfiles = new;
	return tmp;
}

/*
 *	exit with clean up
 */

die(code)
	int	code;
{
	rmall(ALL_TMP);
	exit(code);
}

/*
 *	interrupt service routine
 */

isr(code)
	int code;
{
	signal(code, SIG_IGN);
	fprintf(stderr, "%s has been interrupted\n", name);
	if (pid)
		kill(pid, code);
	die(code);
}

/*
 *	remove all temp files with c_level <= level
 */

rmall(level)
	int level;
{	struct chain * tp;
	struct chain * prev = NULL;

	tp = tmpfiles;
	while (tp)
	{	if (tp->c_level <= level)
		{	remove(tp->c_name);	/* ignore result */
			if (prev)
				prev->c_next = tp->c_next;
			else
				tmpfiles = tp->c_next;
		}
		else
			prev = tp;
		tp = tp->c_next;
	}
}

/*
 *	own check routine;
 *	if real user id != effective user id
 *	access(2) would return wrong results
 *	set (if needed) errno
 *
 *	return 0 if ok
 */

e_access(file, mode)
	char * file;
	int mode;
{	static use_access = 0;	/* if on: use access(2) */
	static init = 0;
	struct stat statbuf;
	int fd;
	extern int errno;

	if (! init)
	{	init = 1;
		use_access = getuid() == geteuid() &&
			     getgid() == getegid();
	}
	if (use_access)
		return access(file, mode);
	switch (mode)
	{	case 1 : /* execute */
			if (stat(file, & statbuf))
				return -1;
			errno = EACCES;	/* for perror */
			return (statbuf.st_mode & S_IEXEC) == 0;
		case 2 : /* write */
		case 4 : /* read */
			/*	2 ---> 1	*/
			/*	4 ---> 0	*/
			mode = (4 - mode) / 2;
			return ((fd = open(file, mode)) < 0) || close(fd);
		case 0 : /* existance */
			return stat(file, & statbuf);
	}
}

/*
 *	built-in commands for efficiency
 */

copy(from, to)
	char * from;
	char * to;
{	FILE * f, * t;
	int ch;

	if (vflag || Vflag)
	{	fprintf(stderr, "cp %s %s\n", from, to);
		if (Vflag)
			return;
	}
	if ((f = fopen(from, "r")) == NULL)
	{	perror(from);
		return;
	}
	if ((t = fopen(to, "w")) == NULL)
	{	perror(to);
		fclose(f);
		return;
	}
	while ((ch = getc(f)) != EOF)
		putc(ch, t);
	fclose(f);
	fclose(t);
}

remove(file)
	char * file;
{
	if (Rflag)
		return;
	if (Vflag || vflag)
	{	fprintf(stderr, "rm -f %s\n", file);
		if (Vflag)
			return;
	}
	unlink(file); /* ignore result */
}
