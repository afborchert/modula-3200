/*
 *	mdb - process handling
 */

#include	<stdio.h>
#include	<assert.h>
#include	<ctype.h>
#include	<signal.h>
#ifdef xelos
#include	<ioctl.h>
#include	<termio.h>
#else
#include	<sgtty.h>
#endif
#ifdef xelos
#include	<sys/types.h>
#include	<sys/seg.h>
#include	<sys/signal.h>
#endif
#include	<sys/param.h>
#include	<sys/dir.h>
#include	<sys/reg.h>
#include	<sys/user.h>
#include	"mdb_ref.h"
#include	"mdb_tree.h"
#include	"mdb_bp.h"
#include	"mdb_ptrace.h"

int pid = 0;	/* process id of child process */
int signo = 0;
extern char * aoutfile;
extern int coremode;
extern struct abp * abplist;	/* list of breakpoints */

extern char * error_mess();
extern struct module * getmod();
extern tree * getproc();

static tree * runpcs();


int reglist[] = {
#ifdef xelos
	PS,
#else
	RPS,
#endif
	PC,
	R15,
	R14,
	R13,
	R12,
	R11,
	R10,
	R9,
	R8,
	R7,
	R6,
	R5,
	R4,
	R3,
	R2,
	R1,
	R0
};

/*
 *	visible routines
 */

tree * run()
{
	endpcs();
	setup();
	if (pid)
		return runpcs(0);
	else
		return NULL;
}

tree * cont(callisr)
	int callisr;
{
	if (pid)
		return runpcs(callisr);
	else
		cmderror("no process");
	/* NOTREACHED */
}

/*
 *	private routines
 */

static endpcs()
{	struct abp * p;

	if (pid)
	{	ptrace(EXIT, pid, 0, 0);
		pid = 0;
	}
	for ( p = abplist ; p ; p = p->abp_link )
	{	p->abp_flag &= ~(ABP_EXEC | ABP_PASSED | ABP_ACTIVE);
		if (p->abp_count = p->abp_initcnt)
			p->abp_flag |= ABP_TOSET;
	}
}

#define	LINSIZ	80
#define	MAXARG	10

/*
 *	read arguments and call "exec"
 */

static doexec()
{	char args[LINSIZ];
	char * argl[MAXARG];
	char * cp, * cp2;
	char prev;
	int argc;
	char * old_arg;
	int fd;
	int i;

	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	getargs(args);
	old_arg = NULL;
	i = 0;
	for ( cp = args ; ; ++cp )
		switch ( *cp )
		{	case ' ' : case '\t' : case '\0' :
				if (old_arg)
				{	argl[i++] = old_arg;
					old_arg = NULL;
				}
				if (! *cp)
					goto break2;
				else
					*cp = '\0';
				break;
			case '<' :
				for ( ++cp ; isspace(*cp) ; ++cp )
					;
				if (*cp)
				{	fclose(stdin);
					for (cp2 = cp; * cp2 &&
					     ! isspace(* cp2); ++ cp2)
						;
					prev = * cp2;
					* cp2 = 0;
					if ( open(cp, 0) < 0 )
					{	my_perror(cp);
						exit(0);
					}
					cp = cp2;
					* cp = prev;
				}
				break;
			case '>' :
				for ( ++cp ; isspace(*cp) ; ++cp )
					;
				if (*cp)
				{	fclose(stdout);
					for (cp2 = cp; * cp2 &&
					     ! isspace(* cp2); ++ cp2)
						;
					prev = * cp2;
					* cp2 = 0;
					if ( creat(cp, 0666) < 0 )
					{	my_perror(cp);
						exit(0);
					}
					cp = cp2;
					* cp = prev;
				}
				break;
			default :
				if (! old_arg)
					old_arg = cp;
				break;
		}
break2:
	for ( fd = 3 ; fd < _NFILE ; ++ fd )
		close(fd);
	argl[i++] = NULL;
	signal(SIGINT, SIG_DFL);
	signal(SIGQUIT, SIG_DFL);
	ptrace(SETTRC, 0, 0, 0);
	execv(aoutfile, argl);
	my_perror(aoutfile);
}

static setup()
{
	switch ( pid = fork() )
	{	case 0 :	/* child process */
			doexec();
			exit(0);
		case -1 :	/* failed */
			my_perror("fork");
			pid = 0;
			break;
		default :	/* father */
			bpwait();	/* wait for "exec"-call of child */
	}
}

static bpwait()
{	register int w;
	int stat;
	int (*sigint)();
	int (*sigquit)();
	char mess[80];

	sigint = signal(SIGINT, SIG_IGN);
	sigquit = signal(SIGQUIT, SIG_IGN);
	while ( (w = wait(&stat)) != pid && w != -1 )
		;
	signal(SIGINT, sigint);
	signal(SIGQUIT, sigquit);
	strcpy(mess, "\n[");
	if ( w == -1 )
	{	my_perror("wait");
		pid = 0;
	}
	else if ((stat & 0177) != 0177)
	{	if (signo = stat & 0177)
			sigprint(signo, mess);
		else
			strcat(mess, "process terminated");
		if (stat & 0200)
		{	strcat(mess, " - core dumped");
			core_end();
			core_init("core");
		}
		pid = 0;
		strcat(mess, "]");
		message(mess);
	}
	else
	{	signo = stat >> 8;
		if (signo != SIGTRAP && signo != SIGILL)
		{	sigprint(signo, mess);
			strcat(mess, "]");
			message(mess);
		}
		else
			signo = 0;
	}
}

sigprint(signo, mess)
	int signo;
	char mess[];
{	char *m;

	switch (signo)
	{	case SIGHUP  :	  m = "hangup"; break;
		case SIGINT  :	  m = "interrupt"; break;
		case SIGQUIT : 	  m = "quit"; break;
		case SIGILL  : 	  m = "illegal instruction"; break;
		case SIGTRAP : 	  m = "trace trap"; break;
		/* run time error */
		case SIGIOT  :	  m = error_mess(); break;
		case SIGEMT  : 	  m = "EMT instruction"; break;
		case SIGFPE  : 	  m = "arithmetic fault"; break;
		case SIGKILL :    m = "kill"; break;
		case SIGBUS  :    m = "memory fault"; break;
		case SIGSEGV :    m = "segmentation violation"; break;
		case SIGSYS  :    m = "bad argument to system call"; break;
		case SIGPIPE : 	  m = "write on a pipe or link with no one to read it"; break;
		case SIGALRM : 	  m = "alarm clock"; break;
		case SIGTERM : 	  m = "software termination signal"; break;
	}
	strcat(mess, m);
}

static setbps()
{	register struct abp * p;

	for ( p = abplist ; p ; p = p->abp_link )
	{	if (p->abp_count == 0)
			continue;
		if (p->abp_flag & ABP_EXEC)
		{	p->abp_flag &= ~ABP_EXEC;
			continue;
		}
		if (p->abp_flag & (ABP_PASSED | ABP_TOSET))
		{	getlong(p->abp_loc, & p->abp_inst);
			putlong(p->abp_loc, 0L, /* text = */ 1);
			p->abp_flag |= ABP_ACTIVE;
			p->abp_flag &= ~(ABP_PASSED | ABP_TOSET);
		}
	}
}

/*
 *	set breakpoint at text-location 0 for preventing restart
 */
static setnullbp()
{
	if (pid)
		putlong(0, 0L, /* text = */ 1);
}

static tree * runpcs(callisr)
	int callisr;	/* if on: continue with signal execsig */
{	static int userpc = 1;
	static int execsig = 0;
#ifdef xelos
	static struct termio usrtty;
	static struct termio mdbtty;
#else
	static struct sgttyb usrtty;
	static struct sgttyb mdbtty;
#endif
	static int usrttyset = 0;
	register long addr;
	register tree * t;
	register struct module * mod;
	register struct abp * p;
	int loopcnt;

	assert(pid);
#ifdef	LINE
	message("[running...]");
#else
	message("\n[running...]");
#endif
	if (! callisr)
		execsig = 0;
	loopcnt = 1;
	t = NULL;
	while (loopcnt--)
	{	setbps();
#ifdef xelos
		ioctl(0, TCGETA, & mdbtty);
#else
		gtty(0, & mdbtty);
#endif
		if (usrttyset)
#ifdef xelos
			ioctl(0, TCSETA, & usrtty);
#else
			stty(0, & usrtty);
#endif
		ptrace(CONTIN, pid, userpc, execsig);/* continue child proc */
		bpwait();
		setnullbp();
		if (pid)
		{	readregs();
#ifdef xelos
			ioctl(0, TCGETA, & usrtty);
#else
			gtty(0, &usrtty);
#endif
			usrttyset = 1;
	
			/*
			 *	look for breakpoint
			 */
	
			execsig = signo;
			if (signo == 0)
			{	addr = getreg(PC);
				if (addr == 0)
				{	message("[illegal jump to address 0]");
					break;
				}
				if ((mod = getmod(addr)) &&
				    (t = getproc(mod, addr)) &&
				    (t->t_bp[0]->abp_loc == addr ||
				     t->t_bp[1]->abp_loc == addr ||
				     t->t_bp[2]->abp_loc == addr ) )
				{	if (t->t_bp[0]->abp_loc == addr)
						p = t->t_bp[0];
					else if (t->t_bp[1]->abp_loc == addr)
						p = t->t_bp[1];
					else
						p = t->t_bp[2];
					if (!(p->abp_flag & ABP_ALWAYS) &&
					    -- p->abp_count)
						++loopcnt;
					else if (p->abp_flag & ABP_SKIP)
						++loopcnt;
					else
						message("[breakpoint]");
					p->abp_flag |= (ABP_PASSED|ABP_EXEC);
					p->abp_flag &= ~ABP_ACTIVE;
					putlong(p->abp_loc, p->abp_inst, 1);
				}
				else
					message("mysterious stop ?!?");
			}
		}
		else
			usrttyset = 0;
#ifdef xelos
		ioctl(0, TCSETA, & mdbtty);
#else
		stty(0, & mdbtty);
#endif
	}
	if (pid || coremode)
	{
#ifndef LINE
		message("\n[please wait a moment...]");
#endif
		new_stack(NULL);
	}
	userpc = 1;
	return t;
}

extern int * corhdr;
extern int * endhdr;

static readregs()
{	register int i;

	endhdr = (char *) corhdr + 
		 (ptrace(RUREGS, pid, & ((struct user *) 0)->u_ar0, 0) & 0xffff);
	for ( i = 0 ; i < 18 ; ++i )
		endhdr[reglist[i]] =
		    ptrace(RUREGS, pid, (char *) &endhdr[reglist[i]] -
		    (char *) corhdr, 0);
}
