#ifdef MPROF
/*
 *	mprof - handling of the a.out file
 */
#else
/*
 *	mdb - handling of the a.out file
 */
#endif

#include	<stdio.h>
#ifdef xelos
#include	<ctype.h>
#include	<string.h>
#endif
#include	<a.out.h>
#ifdef MPROF
#include	"mp.h"
#include	"mp_map.h"
#else
#include	"mdb.h"
#include	"mdb_ref.h"
#include	"mdb_tree.h"
#include	"mdb_bp.h"
#include	"mdb_map.h"
#endif

#ifndef MPROF
extern int version;
#endif

#ifndef xelos
static struct nlist *readsym();

#define TXTRNDSIZ 65536L
#endif


#define	MAXMOD	100

extern char * strsave();
extern char * calloc();
#ifdef MPROF
extern struct proc ** build();
#else
extern tree * build();
#endif

static void new();
address procentry();

#ifdef xelos

static void symtab();
static void readstring();
static void enterfile();
static int scanprocnum();
static void enterlines();
static void enterbrkpt();
static void checkmodname();
static void checkvarname();
static void ext_modname();
static void enterproc();
static void sortpchains();
#ifdef DEBUG_AOUT
static void printtabs();
#endif DEBUG_AOUT

static struct filehdr filehdr;
static struct aouthdr aouthdr;
static struct scnhdr text_scn;
static struct scnhdr data_scn;
static struct scnhdr bss_scn;
static long strings;	/* offset to string area */
static long symbas;	/* file pointer to symbol table */
#ifndef MPROF
static int globi = 0;	/* index for glist */
static struct {
	address var0;
	char * modname;
} glist[MAXMOD];
#endif	/* ! MPROF */
#else	/* ! xelos */
static struct exec header;
#endif	/* xelos */
static FILE *fp;
static char *psym = "P_AREA";
static char *gsym = "G_AREA";
#ifndef MPROF
static char *codesym = ".code";
static char *rgesym = ".rgeerr";
#ifdef xelos
static char *basesym = ".base";
static char *topsym = ".top";
#endif
#endif


/*
 *	exported vars
 */

struct module	mod[MAXMOD];
#ifndef xelos
unsigned	glist[MAXMOD];
#endif
int		a_mod;		/* # modules */
#ifndef MPROF
address		code_addr;	/* value of ".code" label */
address		rge_err;	/* value of ".rgeerr" label */
#ifdef xelos
address		base_addr;	/* value of ".base" label */
address		top_addr;	/* value of ".top" label */
#endif
#endif

struct map txtmap;

#ifndef MPROF
struct bp	* bplist;		/* list of breakpoints */
#endif
char		* aoutfile;

aout_init(aout)
	char	*aout;
{	int	magic;
#ifndef xelos
	long	symbas;
#endif

	aoutfile = aout;
	if ( (fp = fopen(aout,"r")) == NULL )
		perror(aout), exit(1);
#ifdef xelos
	if (fread(& filehdr, FILHSZ, 1, fp) != 1)
		perror(aout), exit(1);
	if (filehdr.f_magic != PEROMAGIC && filehdr.f_magic != PEWRMAGIC)
		fprintf(stderr, "%s: bad magic number\n", aout), exit(1);
	if (filehdr.f_opthdr != sizeof(struct aouthdr))
		fprintf(stderr, "%s: no aouthdr\n", aout), exit(1);
	if (! fread(& aouthdr, sizeof(struct aouthdr), 1, fp) ||
	    ! fread(& text_scn, SCNHSZ, 1, fp) ||
	    ! fread(& data_scn, SCNHSZ, 1, fp) ||
	    ! fread(& bss_scn, SCNHSZ, 1, fp))
		fprintf(stderr, "%s: no section headers\n", aout), exit(1);
	strings = filehdr.f_symptr + filehdr.f_nsyms*SYMESZ;
	txtmap.b1 = text_scn.s_vaddr;
	txtmap.e1 = text_scn.s_size;
	txtmap.f1 = text_scn.s_scnptr;
	txtmap.b2 = data_scn.s_vaddr;
	txtmap.e2 = data_scn.s_size;
	txtmap.f2 = data_scn.s_scnptr;
#ifdef	DEBUG_AOUT
	/*                      12345678 12345678 12345678	*/
	printf("                begin    end      offset\n");
	printf("a.out text map: %08x %08x %08x\n", txtmap.b1, txtmap.e1,
		txtmap.f1);
	printf("      data map: %08x %08x %08x\n", txtmap.b2, txtmap.e2,
		txtmap.f2);
#endif
	symbas = filehdr.f_symptr;
	symtab(symbas, filehdr.f_nsyms);
#ifdef	DEBUG_AOUT
	printtabs();
#endif
#else
	if ( fread(&header, sizeof(struct exec), 1, fp) != 1 )
		perror(aout), exit(1);
	magic = header.a_magic;
	if ( magic != A_MAGIC1 && magic != A_MAGIC2 &&
	     magic != A_MAGIC3 && magic != A_MAGIC4 )
		fprintf(stderr,"%s: bad magic number\n",aout), exit(1);
	symbas = header.a_text + header.a_data;
	txtmap.b1 = 0;
	txtmap.e1 = (magic == A_MAGIC1 ? symbas : header.a_text);
	txtmap.f1 = sizeof(struct exec);
	txtmap.b2 = (magic == A_MAGIC2 ? round(header.a_text,TXTRNDSIZ) : 0);
	txtmap.e2 = txtmap.b2 + (magic == A_MAGIC1 ? symbas : header.a_data);
	txtmap.f2 = sizeof(struct exec)+(magic == A_MAGIC1 ? 0 : txtmap.e1);
	if ( header.a_flag != 1 )
		symbas <<= 1;
	symbas += sizeof(struct exec);
	symtab(symbas);
	sort();
#endif xelos
}

#ifdef xelos

/*
 *	shortforms for some components in union auxent
 */

#define	ax_lnno		x_sym.x_misc.x_lnsz.x_lnno
#define	ax_lnnoptr	x_sym.x_fcnary.x_fcn.x_lnnoptr
#define	ax_fname	x_file.x_fname
#define	ln_paddr	l_addr.l_paddr

#define	SYMLEN		24	/* -> mee/MCBase.d: xmodnamlength */

/*
 *	scan symbol table in common object file format
 */

static void symtab(offset, cnt)
	long offset;		/* file pointer to symbol table  */
	int cnt;		/* count of symbol table entries */
{	struct syment symbuf;	/* buffer for symbol description */
	union auxent auxbuf;	/* auxiliary symbol information  */
	int auxcnt;		/* count auxiliary entries	 */
	char symname[SYMLEN+1];	/* buffer for symbol name	 */
	long linptr = 0;	/* fileptr to lines of curr. fct */
	int procnum = -1;	/* procedure number of curr. fct */
	int lookformodname = 0;	/* =1: just after .file		 */

	a_mod = 0;		/* no modules found 'til now	 */
	fseek(fp, offset, 0);
	while (cnt-- > 0 && fread(& symbuf, SYMESZ, 1, fp))
	{	auxcnt = symbuf.n_numaux;
		if (symbuf.n_zeroes)
		{	strncpy(symname, symbuf.n_name, SYMNMLEN);
			symname[SYMNMLEN] = '\0';
		}
		else
		{	readstring(symname, symbuf.n_offset+strings);
			symname[SYMLEN] = '\0';
		}
		if ((symbuf.n_sclass == C_FILE ||
		     ISFCN(symbuf.n_type) ||
		     symbuf.n_sclass == C_FCN) && auxcnt > 0)
		{	fread(& auxbuf, AUXESZ, 1, fp); auxcnt--; cnt--;
			if (symbuf.n_sclass == C_FILE)
			{	enterfile(auxbuf.ax_fname);
				lookformodname = 1;
			}
			else if (ISFCN(symbuf.n_type))
			{	procnum = scanprocnum(symname);
				if (procnum >= 0)
				{	if (lookformodname)
					{	ext_modname(symname);
						lookformodname = 0;
					}
					enterproc(procnum, symbuf.n_value);
					linptr = auxbuf.ax_lnnoptr;
				}
			}
#ifndef MPROF
			else if (procnum >= 0 && symbuf.n_sclass == C_FCN)
			{	if (symname[1] == 'b')
				{
#ifdef	DEBUG_AOUT
					printf("procnumber %d:\n", procnum);
#endif	DEBUG_AOUT

					enterlines(linptr, auxbuf.ax_lnno);
				}
				enterbrkpt(procnum, symname[1] == 'e',
					symbuf.n_value);
			}
#endif
		}
		else if (symbuf.n_sclass == C_EXT)
		{
#ifndef MPROF
			if (strcmp(symname, codesym) == 0)
				code_addr = symbuf.n_value;
			else if (strcmp(symname, rgesym) == 0)
				rge_err = symbuf.n_value;
			else if (strcmp(symname, basesym) == 0)
				base_addr = symbuf.n_value;
			else if (strcmp(symname, topsym) == 0)
				top_addr = symbuf.n_value;
			else
#endif
			if (symbuf.n_scnum == 1 && /* pure */
				 symbuf.n_type == 0)
				checkmodname(symname, symbuf.n_value);
#ifndef MPROF
			else if (symbuf.n_scnum == 3 &&	/* bss */
				 symbuf.n_type == 0)
				checkvarname(symname, symbuf.n_value);
#endif
		}
		fseek(fp, auxcnt*AUXESZ, 1); cnt -= auxcnt;
	}
	sortpchains();
	if (a_mod == 0)
	{	fprintf(stderr, "No Modula-2 modules found.\n");
		exit(1);
	}
}

/*
 *	read string from string table
 */

static void readstring(symname, offset)
	char * symname;		/* put string therein */
	long offset;		/* position in a.out file */
{	long oldpos = ftell(fp);
	char * cp = symname;

	fseek(fp, offset, 0);
	while (cp-symname < SYMLEN && (* cp++ = getc(fp)))
		;
	fseek(fp, oldpos, 0);
}

/*
 *	check for suffix '.m2' and enter it into module-table
 */

static void enterfile(filename)
	char * filename;
{	char * cp;

	if (a_mod >= MAXMOD)
		return;
	if ((cp = strrchr(filename, '.')) == NULL)
		return;
	if (strcmp(cp, ".mr") && strcmp(cp, ".m2") && strcmp(cp, ".s"))
		return;
	/* filename: start of module */
#ifdef MPROF
	mod[a_mod].m_procs = 0;
#else
	mod[a_mod].m_proccnt = 0;
#endif
	mod[a_mod].m_pchain = NULL;
	mod[a_mod++].m_file = strsave(filename);
}

/*
 *	check for [A-Za-z][A-Za-z0-9]*_P[1-9][0-9]*
 *	and return procedure number (-1 on error)
 */

static int scanprocnum(p)
	register char * p;		/* procedure label */
{	int procnum;

	if (! isalpha(* p))
		return -1;
	for (++p; isalnum(* p); ++p)
		;
	if (* p++ != '_' || * p++ != 'P')
		return -1;
	if (sscanf(p, "%d", & procnum) == 1)
		return procnum;
	else
		return -1;
}

#ifndef MPROF
/*
 *	enter line numbers of current procedure
 */

static void enterlines(linptr, startnum)
	long linptr;			/* file pointer */
	int startnum;			/* procedure start line number */
{	long oldpos = ftell(fp);	/* save old file position */
	struct lineno linbuf;

	if (fseek(fp, linptr+LINESZ, 0) == -1)
		return;
#ifdef	DEBUG_AOUT
	printf("address  linenumber\n");
	printf("-------------------\n");
#endif	DEBUG_AOUT
	while (ftell(fp) < symbas &&
	       fread(& linbuf, LINESZ, 1, fp) && linbuf.l_lnno)
#ifdef	DEBUG_AOUT
		printf("%8x %4d\n", linbuf.ln_paddr, linbuf.l_lnno+startnum-1);
#else
		enter_ln(linbuf.ln_paddr, linbuf.l_lnno+startnum-1);
#endif
	fseek(fp, oldpos, 0);
}

/*
 *	enter breakpoint label into list
 */

static void enterbrkpt(procnum, flag, addr)
	int procnum;
	int flag;
	address addr;
{	struct bp * bptr;

	new(& bptr, sizeof(struct bp));
	bptr->bp_proc = procnum;
	bptr->bp_flag = flag;
	bptr->bp_addr = addr;
	bptr->bp_ptr = bplist;
	bptr->bp_mod = & mod[a_mod-1];
	bplist = bptr;
#ifdef DEBUG_AOUT
	printf("breakpoint: %3d %5s %8x\n", procnum,
		flag? "END": "BEGIN", addr);
#endif
}
#endif	/* ! MPROF */

static void checkmodname(symname, addr)
	char * symname;
	address addr;
{	register char * cp = symname;
	register int i;
	register int mi;

	if (! isalpha(* cp))
		return;
	for (++cp; isalnum(* cp); ++cp)
		;
	/* pseudo Modula-2 modules written in as */
	if (* cp == '_' && *(cp+1) == '\0')
		* cp = '\0';
	if (* cp)
		return;
	for (mi = 0; mi < a_mod; ++mi)
		if (strcmp(symname, mod[mi].m_name) == 0)
			break;
	if (mi == a_mod)
		return;		/* module not found */
#ifndef MPROF
	mod[mi].m_bpset = 0;
#endif
	mod[mi].m_entry = addr;
	mod[mi].m_ptr = NULL;
#ifndef MPROF
	for (i = 0; i < globi; ++i)
		if (glist[i].modname && strcmp(symname, glist[i].modname) == 0)
		{	mod[mi].m_global = glist[i].var0;
			glist[i].var0 = 0;
			cfree(glist[i].modname);
			glist[i].modname = 0;
			break;
		}
#endif MPROF
}

#ifndef MPROF
static void checkvarname(symname, addr)
	char * symname;
	address addr;
{	register char * cp = symname;
	int varoffset;
	int i;

	if (globi >= MAXMOD)
		return;
	if (! isalpha(* cp))
		return;
	for (++cp; isalnum(* cp); ++ cp)
		;
	if (* cp++ != '_' || * cp++ != 'V')
		return;
	if (sscanf(cp, "%d", & varoffset) != 1)
		return;
	if (varoffset != 0)
		return;
	cp = strrchr(symname, '_'); * cp = '\0';
	for (i = 0; i < a_mod; ++i)
		if (strcmp(symname, mod[i].m_name) == 0)
		{	mod[i].m_global = addr;
			* cp = '_';
			return;
		}
	* cp = '_';
	glist[globi].modname = strsave(symname);
	glist[globi++].var0 = addr;
}
#endif /* ! MPROF */

/*
 *	extract module name from procname and
 *	store it into mod[a_mod-1].m_name
 */

static void ext_modname(procname)
	char * procname;
{	char * cp;

	if ((cp = strchr(procname, '_')) == NULL)
	{	mod[a_mod-1].m_name = "???";
		return;
	}
	* cp = 0;
	mod[a_mod-1].m_name = strsave(procname);
	* cp = '_';
}

/*
 *	link procedure into the procedure list of the current module
 */

static void enterproc(procnum, entry)
	int procnum;
	address entry;
{	struct pchain * pcp;

	new(& pcp, sizeof(struct pchain));
	pcp->pc_num = procnum;
	pcp->pc_entry = entry;
	pcp->pc_link = mod[a_mod-1].m_pchain;
	mod[a_mod-1].m_pchain = pcp;
#ifdef MPROF
	if (procnum > mod[a_mod-1].m_procs)
		mod[a_mod-1].m_procs = procnum;
#else
	if (procnum > mod[a_mod-1].m_proccnt)
		mod[a_mod-1].m_proccnt = procnum;
#endif
}

static void sortpchains()
{	struct module * mp;
	register struct pchain * pcp, * ppcp;

	for (mp = mod; mp-mod < a_mod; ++ mp)
	{	pcp = mp->m_pchain;
#ifdef MPROF
		new(& mp->m_pfield, (mp->m_procs+1)*sizeof(struct pfield));
#else
		new(& mp->m_pfield, (mp->m_proccnt+1)*sizeof(struct pfield));
#endif
		ppcp = pcp;
		while (pcp)
		{	mp->m_pfield[pcp->pc_num].pf_entry = pcp->pc_entry;
			ppcp = pcp;
			pcp = pcp->pc_link;
			cfree(ppcp);
		}
	}
}

#ifdef DEBUG_AOUT
static void printtabs()
{	register int i, j;

	printf("DEBUG_AOUT: printtab...\n");
	printf("a_mod = %d, globi = %d\n",
		a_mod, globi);
	/*	12345678901234 123456789012345678901234 12345678 12345678 */
	printf("filename       module                      entry   global\n");
	printf("---------------------------------------------------------\n");
	for (i = 0; i < a_mod; ++i)
			printf("%-14s %-24s %8x %8x\n",
				mod[i].m_file, mod[i].m_name,
				mod[i].m_entry, mod[i].m_global);
	printf("procedures\n");
	printf("----------\n");
	for (i = 0; i < a_mod; ++i)
	{	printf("%-24s", mod[i].m_name);
		for (j = 0; j <= mod[i].m_proccnt; ++j)
		{	if (j > 0 && j % 6 == 0)
				printf("\n%-24s", " ");
			printf(" %8x", mod[i].m_pfield[j].pf_entry);
		}
		printf("\n");
	}
}
#endif	/* DEBUG_AOUT */

#else /* ! xelos */

struct extlist {
	char	e_name[8];	/* symbol name */
	unsigned e_value;	/* value */
	struct	extsym *e_ptr;	/* link */
};

struct psymlist {
	unsigned p_value;	/* value */
	struct psymlist *p_ptr;	/* link */
};

struct extlist	*elist;
struct psymlist	*plist;

static symtab(offset)
	int	offset;
{	struct nlist *sym;
	struct extlist *eptr;
	struct psymlist *pptr;
	struct bp *bptr;

	a_mod = 0;
	if ( fseek(fp, offset, 0) )
		fprintf(stderr, "symbol table missing\n"), exit(1);
	elist = plist = NULL;
#ifndef MPROF
	bplist = NULL;
#endif
	while ( sym = readsym() )
	{	if ( (sym->n_type & N_EXT) && (sym->n_type & N_TEXT) )
		{	new(&eptr, sizeof(struct extlist));
			eptr->e_value = sym->n_value;
			strncpy(eptr->e_name, sym->n_name, 8);
			eptr->e_ptr = elist;
			elist = eptr;
		}
#ifndef MPROF
		else if ( (sym->n_type & N_BSS) &&
			  strncmp(sym->n_name, gsym, 8) == 0 )
		{	glist[a_mod++] = sym->n_value;
		}
#endif
		else if ( (sym->n_type & N_TEXT) &&
			  strncmp(sym->n_name, psym, 8) == 0 )
		{	new(&pptr, sizeof(struct psymlist));
			pptr->p_value = sym->n_value;
			pptr->p_ptr = plist;
			plist = pptr;
		}

#ifndef MPROF
		/* pick up breakpoint labels */

		else if ( (sym->n_type & N_TEXT) &&
			sym->n_name[0] == 'B' &&
			(sym->n_name[1] == 'B' || sym->n_name[1] == 'E' ))
		{	char buf[7];

			strncpy(buf, sym->n_name+2, 6);
			buf[6] = '\0';	/* atoi needs a nullbyte */
			new(&bptr, sizeof(struct bp));
			if (version == 1 || version == 2)
				bptr->bp_proc = atoi(buf);
			else
				bptr->bp_proc = atoi(buf+1);
			bptr->bp_flag = sym->n_name[1] == 'E';
			bptr->bp_addr = sym->n_value;
			bptr->bp_ptr = bplist;
			bplist = bptr;
		}

		/* pick up line number labels */

		else if ((sym->n_type & N_TEXT) &&
			 sym->n_name[0] == 'l' && sym->n_name[1] == '.')
		{	int lineno;

			lineno = atoi(& sym->n_name[2]);
			enter_ln(sym->n_value, lineno);
		}

		/* pick up special labels */

		else if (sym->n_type & N_BSS)
			if (strncmp(sym->n_name, codesym, 8) == 0)
				code_addr = sym->n_value;
			else if (strncmp(sym->n_name, rgesym, 8) == 0)
				rge_err = sym->n_value;
#endif	/* ! MPROF */
	}
}

static struct nlist *readsym()
{	static	struct nlist sym;

	if ( fread(&sym, sizeof(struct nlist), 1, fp) != 1 )
		return NULL;
	else
		return & sym;
}

cmp_gl(el1, el2)
unsigned *el1, *el2;
{
	return *el1 - *el2;
}

cmp_mod(el1, el2)
struct module *el1, *el2;
{
	return el1->m_entry - el2->m_entry;
}

static sort ()
{	struct psymlist *psym;
#ifndef MPROF
	int counter;
#endif
	struct esymlist *esym;

#ifndef MPROF
	counter = 0;
	if ( a_mod == 0 )
		fprintf(stderr,"No MODULA modules found\n"), exit(1);
	qsort(glist, a_mod, sizeof(unsigned), cmp_gl);
#endif MPROF
	for ( psym = plist ; psym ; psym = psym->p_ptr )
	{	for ( esym = elist ; esym ; esym = esym->e_ptr )
			if ( esym->e_value == psym->p_value )
				break;
		if ( !esym )
		{	fprintf(stderr,
				"Warning: module without external ref\n");
			continue;
		}
#ifdef MPROF
		mod[a_mod].m_entry = psym->p_value;
		mod[a_mod].m_name = esym->e_name;
		mod[a_mod].m_ptr = NULL;
		++a_mod;
#else
		mod[counter].m_entry = psym->p_value;
		mod[counter].m_name = esym->e_name;
		mod[counter].m_ptr = NULL;
		mod[counter].m_file = NULL;
		++counter;
#endif
	}
#ifndef MPROF
	if ( counter < a_mod )
	{	fprintf(stderr,
			"Warning: more G_AREA's defined than P_AREA's\n");
		a_mod = counter;
	}
	else if ( counter > a_mod )
	{	fprintf(stderr,
			"Warning: more P_AREA's defined than G_AREA's\n");
		counter = a_mod;
	}
#endif
	if ( a_mod == 0 )
		fprintf(stderr, "No MODULA modules found\n"), exit(1);
	qsort(mod, a_mod, sizeof(struct module), cmp_mod);
}
#endif	/* xelos */

#ifndef DEBUG_AOUT

#ifdef MPROF
int cmp_proc();

p_sort(m)
	struct module * m;
{	struct proc ** pp;

	for (pp = m->m_ptr; pp - m->m_ptr < m->m_procs; ++pp)
		(*pp)->p_addr = procentry(m, (*pp)->p_num);
	qsort(m->m_ptr, m->m_procs, sizeof(struct proc *), cmp_proc);
}

cmp_proc(el1, el2)
	struct proc ** el1, ** el2;
{
	return (*el1)->p_addr - (*el2)->p_addr;
}
#endif	/* MPROF */

struct module * getmod(addr)
	address addr;
{	struct module * ptr;

	if ( addr < mod[0].m_entry ) return NULL;
	if ( addr >= mod[a_mod-1].m_entry ) return &mod[a_mod-1];
	for ( ptr = mod ; ptr < &mod[a_mod] ; ++ptr )
		if ( addr < ptr->m_entry )
			break;
	return --ptr;
}

address procentry(mod, procnum)
	struct module * mod;
	int procnum;
{	address addr;
	address result;

#ifdef xelos
	return mod->m_pfield[procnum].pf_entry;
#else
	addr = mod->m_entry + 4 * procnum;
	getlong(addr, & result);
	return result;
#endif
}

#ifdef MPROF
struct proc * getproc(mod, addr)
	struct module *mod;
	unsigned addr;
{	struct proc ** ptr;
	unsigned paddr;

	if ( ! mod->m_ptr )
	{	mod->m_ptr = build(mod->m_name, & mod->m_procs);
		if (! mod->m_ptr)
			return NULL;
		p_sort(mod);
	}

	ptr = mod->m_ptr;
	if ((*ptr)->p_addr > addr)
		return NULL;
	for (ptr = mod->m_ptr; ptr - mod->m_ptr < mod->m_procs; ++ptr)
	{	paddr = (*ptr)->p_addr;
		if (! paddr) continue;	/* empty body of a local module */
		if (addr < paddr) return *--ptr;
	}
	return *--ptr;
}

#else /* ! MPROF */

tree * getproc(mod, addr)
	struct module * mod;
	address addr;
{	tree * ptr;
	address paddr;
	tree * old_proc;

	if (! mod)
		return NULL;
	if (! mod->m_ptr)
	{	mod->m_ptr = build(mod->m_name);
		if (! mod->m_ptr)
			return NULL;
	}
	ptr = mod->m_ptr;
	/* procedure 0 == module body */
	if (addr >= procentry(mod, 0))
		return ptr;
	old_proc = NULL;

	while (ptr->t_link)
	{	ptr = ptr->t_link;
		if (ptr->t_type == T_PROC)
		{	paddr = procentry(mod, ptr->t_pnum);
#ifndef xelos
			if (! paddr) return NULL;
#endif
			if (addr < paddr) return old_proc;
			old_proc = ptr;
		}
	}
	return old_proc;
}
#endif

#ifndef MPROF
address globaladdr(var)
	tree * var;
{	struct module * mod_ptr;

	while (var->t_father)
		var = var->t_father;
	for (mod_ptr = mod; mod_ptr-mod < a_mod; ++mod_ptr)
		if (mod_ptr->m_ptr == var)
#ifdef xelos
			return mod_ptr->m_global;
#else
			return glist [mod_ptr-mod];
#endif
	for (mod_ptr = mod; mod_ptr-mod < a_mod; ++mod_ptr)
#ifdef xelos
		if (strncmp(mod_ptr->m_name, var->t_name, SYMLEN) == 0)
#else
		if (strncmp(mod_ptr->m_name, var->t_name, 8) == 0)
#endif
		{	mod_ptr->m_ptr = var;
#ifdef xelos
			return mod_ptr->m_global;
#else
			return glist [mod_ptr-mod];
#endif
		}
	message("globaladdr: unreferenced module");
	return 0;
}

int readaout(addr, value)
	address addr;
	long * value;
{
	return readm(fp, txtmap, addr, value);
}

#endif	/* ! MPROF */
#endif	/* ! DEBUG_AOUT */

static void new(ptr, size)
	char **ptr;
	int size;
{
	if ( (*ptr = calloc(1, size)) == NULL )
		perror("malloc"), exit(1);
}

#ifdef MPROF

int getlong(addr, value)
	address addr;
	val * value;
{
	return readm(fp, txtmap, addr, value);
}
#endif
