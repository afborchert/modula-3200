/*
 *	mdb - backtrace of the actual stack
 */

#include	<stdio.h>
#include	<signal.h>
#include	<sys/reg.h>
#include	"mdb.h"
#include	"mdb_ref.h"
#include	"mdb_tree.h"
#include	"mdb_bp.h"
#include	"mdb_view.h"
#include	"mdb_procstr.h"
#ifdef xelos
#include	"mdb_instr.h"
#endif

extern int version;
#ifdef xelos
extern struct instr instr;	/* see mdb_opset.c */
#endif

extern char * calloc();
extern char * realloc();
extern struct module * getmod();
extern tree * getproc();
#ifdef xelos
extern int disasm();
#endif xelos

int pc = 0;			/* mdb_error.c -> mdb_stack.c */
int e_base = 0;
int e_top = 0;
#ifdef LINE
int ppc = 0;			/* mdb_stack.c -> mdb_lcmd.c */
#endif
static view **pwin = NULL;

static char * sigtext();
static char * e_realloc();

/*
 *	activation record:
 *
 *	+--------------------+            \
 *	|		     |            |
 *	|    parameters      |            |
 *	|		     |            |
 *	+--------------------+            |
 *	|    static link     |            >  parlength
 *	+--------------------+            |
 *	|    dynamic link    |            |
 *	+--------------------+            |
 *	|      old pc        |            |
 *	+--------------------+ <--- base /
 *	|		     |
 *	|    local vars      |
 *	|		     |
 *	+--------------------+
 *	|    temporaries     |
 *	+--------------------+ <--- top
 *
 */

static init(p)
	struct process * p;
{
	long base, top, rf, limit;
#ifdef xelos
	long ra;
#endif xelos
	struct module *mod;
	tree *proc;
	int view_size = 30;
	int unknown = 0;
	int stack_depth = 0;
	view **ptr;
	int lineno;
	char lineno_str[8];

	pwin = NULL;
	if (p)
	{	base = p->p_base;
		top = p->p_top;
		rf = p->p_pc;
		limit = p->p_limit;
		if (! p->p_started)
		{	message("PROCESS not started");
			return;
		}
	}
	else
	{
#ifdef xelos
		base = getreg(R14);
		top = getreg(R13);
		limit = getreg(R2) & 0x3fffffff;	/* see cm2rt0.s */
#else
		if (version > 0 && version <= 3)
		{	base = getreg(R2);
			top = getreg(R3);
		}
		else
		{	base = getreg(R14);
			top = getreg(R2);
			limit = getreg(R7);
		}
#endif
		error_mess();		/* set pc, e_base and e_top */
		if (pc)
		{	rf = pc;
			pc = 0;
		}
		else
			rf = getreg(PC);
		if (e_base)
		{	base = e_base;
			e_base = 0;
		}
		if (e_top)
		{	top = e_top;
			e_top = 0;
		}
	}
#ifdef LINE
	ppc = rf;
#endif

	if ( (pwin = (view **) calloc(view_size, sizeof(view *))) == NULL ) {
		my_perror("calloc");
		return;
	}

	/*
	 *	initialisize P-window
	 */

	ptr = pwin;
	if ( (*ptr = (view *) calloc(1, sizeof(view))) == NULL ||
	     (*++ptr = (view *) calloc(1, sizeof(view))) == NULL ) {
		my_perror("calloc");
		return;
	}
	/*                       123456789012345678901234 12345678901234 12345678 12345678 12345678 */
	strcpy((*pwin)->v_line, "module                   procedure          base      top       pc  line");
	(*pwin)->v_node = NULL; (*pwin)->v_mod = NULL;
#ifdef LINE
	(*pwin)->v_flags = V_START;
#endif
	strcpy((*ptr)->v_line, " ");
	(*ptr)->v_node = NULL; (*ptr)->v_mod = NULL;
#ifdef LINE
	(*ptr)->v_flags = 0;
#endif
	++ptr;
	do {
cont:
		if (ptr - pwin == view_size)
		{	pwin = (view **) e_realloc(pwin, sizeof(view *), view_size += 20);
			ptr = pwin + view_size - 20;
		}
		if ( (*ptr = (view *) calloc(sizeof(view), 1)) == NULL ) {
			my_perror("calloc");
			return;
		}
#ifdef LINE
		(*ptr)->v_flags = 0;
#endif

		/*
		 *	interrupt ?
		 */
#ifdef xelos
		if (base % 4 == 1)
#else
		if (base == 1)
#endif
		{	long value;

			(*ptr)->v_node = NULL;
			(*ptr)->v_mod = NULL;
			(*ptr)->v_lineno = 0;
#ifdef xelos
			
			/*		+---------+
			 *		| regs    |
			 *		| pc + ps |
			 *		+---------+
			 *		| 2 words |
			 *		+---------+
			 *		| signo   |
			 *  base ->	+---------+
			 */
			--base;
			getlong(base, & value); /* get signo */
			sprintf((*ptr)->v_line, "<< interrupted with %s >>", sigtext(value));
			getlong(base+12, & rf);	/* get pc */
			getlong(base+20+2*4, & limit);
			getlong(base+20+13*4, & top);
			getlong(base+20+14*4, & base);
#else /* ! xelos */
			while (getlong(limit, & value) == 0 && value != -1)
				limit += 4;
			if (value != -1)
			{	sprintf((*ptr)->v_line,
				"unknown interrupt / stack demolished");
				break;
			}
			limit += 4;
			getlong(limit, & value); /* signal number */
			sprintf((*ptr)->v_line, "<< interrupted with %s >>", sigtext(value));
			limit += 4;  /* skip signal number */
			limit += 64; /* skip floating point registers */
			getlong(limit + 4*2, & top);
			getlong(limit + 4*14, & base);
			limit += 64; /* skip general registers */
			getlong(limit + 4, & rf); /* program counter */
			getlong(limit + 4*2, & limit);
#endif /* ! xelos */
			++ptr;
			goto cont; /* continue while-loop */
		}

#ifdef xelos
		/*
		 *	UNIXCALL problem: base is misused as ap
		 *	and ra contains the original value of base
		 */
		if ((ra = getreg(R10)) > 0xf00000 && ra < base &&
		    ra % 4 == 0 && ra - base < 1024)
		{	val ins;

			/*
			 *	this check isn't perfect (too hard)
			 *	(if interrupted near but not during svc)
			 */
			if (! getlong(rf-4, & ins) &&
			    disasm(rf-4, DATA, ins) == 4 &&
			    instr.op == 0xe1 /* svc */)
				base = ra;
		}
#endif xelos

		mod = getmod(rf);
		(*ptr)->v_mod = mod;
		if (mod) {
			proc = getproc(mod, rf);
			(*ptr)->v_node = proc;
		}
		else
		{	proc = NULL;
			(*ptr)->v_node = NULL;
		}

		if (proc && ! proc->t_active)
		{	proc->t_active = 1;
			/* calculate correct base */
			if (! version)
			{	if (proc->t_parlength < 12)
					proc->t_parlength = 12;
			}
			else
			{	if (proc->t_parlength < 16)
					proc->t_parlength = 16;
			}
			proc->t_base = base - proc->t_parlength;
		}
		if (proc && proc->t_type == T_PROC)
		{	(*ptr)->v_base = base - proc->t_parlength;
			(*ptr)->v_size = proc->t_parlength >> 2;
		}
		else
		{	(*ptr)->v_base = 0;
			(*ptr)->v_size = 0;
		}
		(*ptr)->v_addr = rf;
		lineno = get_ln(rf);
		if (lineno >= 1)
		{	(*ptr)->v_lineno = lineno;
			sprintf(lineno_str, "%4d", lineno);
		}
		else
		{	(*ptr)->v_lineno = 0;
			strcpy(lineno_str, "    ");
		}
		if (mod && proc)
		{	sprintf((*ptr)->v_line, "%-24s %-14s %8x %8x %8x  %s",
				mod->m_name, proc->t_name,
				base, top, rf, lineno_str);
			if (lineno <= 0)
				(*ptr)->v_lineno = proc->t_line;
		}
		else if (mod)
			sprintf((*ptr)->v_line, "%-24s %-14s %8x %8x %8x  %s",
				mod->m_name, "unknown",
				base, top, rf, lineno_str);
		else
			sprintf((*ptr)->v_line, "%-24s %-14s %8x %8x %8x  %s",
				"unknown", "unknown",
				base, top, rf, lineno_str);
		if (version == 1 || version == 2)
			top = base;
		else if (version == 3)
		{	if (proc)
			{	top = base - proc->t_parlength;
				unknown = 0;
			}
			else if (unknown < 2)
			{	/*
				 *	try parlength = 16
				 *	that's correct for parameterless
				 *	procedures
				 */

				top = base - 16;
				++unknown;
			}
			else
				break; /* give up */
		}
		else
			top = base - 12;
		++ptr;
	}
	while ( getlong(4+top, &base) == 0 &&
		getlong(8+top, &rf)   == 0 &&
		base &&
		(base < top || base == top && version != 1 && version != 2) &&
		stack_depth++ < 100);
	if (ptr - pwin >= view_size)
	{	view_size = ptr-pwin+1;
		pwin = (view **) e_realloc(pwin, sizeof(view *), view_size);
		ptr = pwin+view_size-1;
	}
#ifdef LINE
	(*(ptr-1))->v_flags = V_END;
#endif
	*ptr = NULL;
}

view **backtrace()
{
	if (! pwin)
		init(NULL);
	return pwin;
}

extern struct module mod[];
extern int a_mod;

new_stack(p)
	struct process * p;
{	int i;
	tree * tp;
	view ** old_pwin;

	old_pwin = pwin;
	for ( i = 0 ; i < a_mod ; ++i )
		for ( tp = mod[i].m_ptr ; tp ; tp = tp->t_link )
			tp->t_active = 0;
	init(p);
	if (! pwin)
	{	pwin = old_pwin;
		return;
	}
	if (old_pwin)
		kill_view(old_pwin);
}

address localaddr(var)
	tree *var;
{
	if (var->t_father)
		var = var->t_father;
	while (var->t_father && var->t_type == T_MOD)
		var = var->t_father;
	return var->t_base;
}

static char * e_realloc(ptr, size, cnt)
	char * ptr;
	unsigned size;
	unsigned cnt;
{
	if ((ptr = realloc(ptr, size*cnt)) == NULL)
	{	my_perror("realloc");
		pwin = NULL;
		cmderror(NULL);
	}
	return ptr;
}

static char * sigtext(signo)
	int signo;
{
	switch (signo)
	{	case SIGHUP:	return "SIGHUP";
		case SIGINT:	return "SIGINT";
		case SIGQUIT:	return "SIGQUIT";
		case SIGILL:	return "SIGILL";
		case SIGTRAP:	return "SIGTRAP";
		case SIGIOT:	return "SIGIOT";
		case SIGEMT:	return "SIGEMT";
		case SIGFPE:	return "SIGFPE";
		case SIGKILL:	return "SIGKILL";
		case SIGBUS:	return "SIGBUS";
		case SIGSEGV:	return "SIGSEGV";
		case SIGSYS:	return "SIGSYS";
		case SIGPIPE:	return "SIGPIPE";
		case SIGALRM:	return "SIGALRM";
		case SIGTERM:	return "SIGTERM";
		default:	return "undefined";
	}
}
