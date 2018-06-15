/*
 *	mdb - screen oriented user interface
 */

#ifndef LINE

#include	<stdio.h>
#include	<curses.h>
#include	<setjmp.h>
#include	<signal.h>
#include	"mdb_ref.h"
#include	"mdb_bp.h"
#include	"mdb_tree.h"
#include	"mdb_procstr.h"
#include	"mdb_view.h"

/*
 *	following for my_perror
 */

extern int errno;
extern int sys_nerr;
extern char *sys_errlist[];

extern int coremode;	/* see mdb_core.c */
extern int fflag;
extern int pid;		/* see mdb_process.c */
       int noscreen;	/* =1: don't use the curses ! */

view **init_dot();
char getcmd();

jmp_buf	cmd_loop;
int cmderror();


/*
 *	central window
 */

int WIN_COLS, WIN_LINES;
WINDOW *win;

static WINDOW *board;	/* global window */
static WINDOW *paneel;	/* bottom line */
static WINDOW *command;	/* input line */
static int MESS_COLS;
static int MESS_LINE;
static WINDOW *mess;	/* for general messages */

static view **act_view;	/* to be displayed in the central window */
static view **dot;
static view **pdot = NULL;	/* remark the dot of the P-window */
static char mode;	/* 'p' for P-window, 'm' for M-window, ... */

char	MASK[] = { "*** MODULA DEBUGGER ***" };

/*
 *	print formatted string on "mess" window
 */

message(string, p1, p2, p3, p4, p5, p6)
{	char *ptr;
	static char wclr = 0;

	if (fflag)
	{	printf(string, p1, p2, p3, p4, p5, p6);
		printf("\n");
		return;
	}
	if (! string)
	{	if (! wclr)
		{	wclear(mess);
			wrefresh(mess);
			wclr = 1;
		}
		return;
	}
	if (mess && !noscreen)
	{	wclear(mess);
		wprintw(mess,string,p1,p2,p3,p4,p5,p6);
		wrefresh(mess);
		wclr = 0;
	}
	else
	{	printf("%s", LL);
		printf(string,p1,p2,p3,p4,p5,p6);
		printf("\n");
	}

	/*
	 *	repair cursor position
	 */

	if (! noscreen )
		wrefresh(win);
}

/*
 *	read a filename from commandline
 */

getfilename(buf, bufsize)
	char buf[];
	int bufsize;
{
	wclear(command);
	wprintw(command, "filename > ");
	wrefresh(command);
	wgetstr(command, buf);
	wclear(command);
	wrefresh(command);
	wrefresh(win);
}

/*
 *	for core-window: read address and # fullwords
 *	return 1 if ok
 */

getcorewin(addr, len)
	long * addr;
	int * len;
{	char buf[80];

	wclear(command);
	wprintw(command, "> ");
	wrefresh(command);
	wgetstr(command, buf);
	wclear(command);
	wrefresh(command);
	wrefresh(win);
	if (sscanf(buf, "%o %d", addr, len) != 2 &&
	    sscanf(buf, "%o , %d", addr, len) != 2)
	{	if (sscanf(buf, " , %d", len) != 1)
			len = 0;
		return 0;
	}
	else
		return 1;
}

/*
 *	for 'P'-command
 *
 *	get pointer to process descriptor
 */

get_pdesc(addr)
	int * addr;
{	char buf[80];

	message("Enter octal address of process descriptor");
	wclear(command);
	wprintw(command, "> ");
	wrefresh(command);
	wgetstr(command, buf);
	wclear(command);
	wrefresh(command);
	wrefresh(win);
	if (sscanf(buf, "%o", addr) != 1)
		return 0;
	else
		return 1;
}

pre_die(code)
{
	resetty();
	exit(code);
}

/*
 *	screen initialization, see curses(3)
 */

init ()
{
	savetty();
	signal(SIGINT, pre_die);
	initscr();
	board = subwin(stdscr, LINES-1, COLS-1, 0, 0);
	paneel = subwin(stdscr, 1, COLS-1, LINES-1, 0);
	MESS_COLS = COLS-1;
        MESS_LINE = LINES-4;
	mess = subwin(stdscr, 1, MESS_COLS, MESS_LINE, 1);
	command = subwin(stdscr, 1, COLS-1, LINES-3, 1);
	win = subwin(stdscr, LINES-5, COLS-1, 1, 1);

	/*
	 *	we assume all windows are okay
	 *	(this may fail only on evil experiments)
	 */

	WIN_LINES = LINES-5;
	WIN_COLS = COLS-1;
	wrefresh(board);
	wmove(paneel, 0, ((COLS-sizeof(MASK))+4)/2);
	wprintw(paneel, MASK);
	wrefresh(paneel);
}

/*
 *	work up user commands
 */

cmdmode()
{	char cmd;
	tree *t;
	tree * tson;

	noscreen = 0;
	dot = NULL;
	if (coremode)		/* if we have a core file */
	{	print_rte();
		p_window();
		mode = 'p';
		pdot = dot = init_dot();
		if (! dot)
			message("no MODULA stack found");
	}
	if (! dot)
	{	m_window();
		mode = 'm';
		dot = init_dot();
	}
	showwin(act_view);

	/* jump on error to this point */

	setjmp(cmd_loop);
	signal(SIGINT, cmderror);

	/* command loop */

	do
	{	cmd = getcmd(TRUE);
		if (cmd != 'w')
			message(NULL);	/* clear message field */
		switch (cmd) {

		/*
		 *	switch window
		 */

		case 'M' : new_stack(NULL);	/* return to current stack */
			   pdot = NULL;
		case 'p' : p_window();
			   if (pdot)
				   dot = pdot;
			   else
				pdot = dot = init_dot();
			   showwin(act_view);
			   mode = 'p';
			   break;
		case 'd' : if (! dot || !*dot)
			   {	message("no D-window");
				break;
			   }
			   d_window((*dot)->v_node, (*dot)->v_mod);
			   dot = init_dot();
			   showwin(act_view);
			   mode = 'd';
			   break;
		case 'm' : m_window();
			   dot = init_dot();
			   showwin(act_view);
			   mode = 'm';
			   break;
		case 'c' : c_window();
			   dot = init_dot();
			   showwin(act_view);
			   mode = 'c';
			   break;
		case 't' : if (! dot || ! *dot)
			   {	message("no T-window");
				break;
			   }
			   t_window((*dot)->v_node, (*dot)->v_mod,
                              (*dot)->v_lineno);
			   dot = init_dot();
			   showwin(act_view);
			   mode = 't';
			   break;
		case 'q' :
		case 'x' : die(0);
		case '!' : execcmd();
			   break;
		case 'w' : write_win();
			   break;

		/*
		 *	move cursor
		 */

		case '\n' :
		case '+' : avanti(1);
			   break;
		case '-' : avanti(-1);
			   break;
		case 04  :	/* control-D */
			   avanti(WIN_LINES/2);
			   break;
		case 025  :	/* control-U */
			   avanti(- WIN_LINES/2);
			   break;
		case 06 :	/* control-F */
			   avanti(WIN_LINES);
			   break;
		case 02 :	/* control-B */
			   avanti(-WIN_LINES);
			   break;

		/*
		 *	switch process
		 */

		case 'P' :
			   p_switch();
			   break;
		/*
		 *	walk through the tree structure
		 */

		case 'f' :
		case 's' :
			   if ( mode != 'd' )
			   {	message("only in D-window allowed");
				break;
			   }
			   t = (*dot)->v_node;
			   if (! t)
			   {	message("empty node");
				break;
			   }
			   switch (cmd) {
			   case 'f' :
				if (t->t_father)
				{	t = t->t_father;
					tson = t;
					if (t->t_father)
						t = t->t_father;
				}
				else
					message("top level reached");
				break;
			   case 's' :
				if (! t || !t->t_son )
					message("no son existent");
				break;
			   }
			   if (t && t->t_son)
			   {	d_window(t, (*dot)->v_mod);
				if (cmd != 'f')
					dot = init_dot();
				else
				{	for (dot = act_view;
						dot && *dot; ++dot)
						if ( (*dot)->v_node == tson)
							break;
					if (!dot || !*dot )
						dot = init_dot();
				}
				showwin(act_view);
			   }
			   break;

		/*
		 *	child process control
		 */
		
		case 'R' : resetty();
			   noscreen = 1;
			   run();
                           mdbmode();
			   break;
		case 'I' :
		case 'C' : resetty();
			   clearok(stdscr, FALSE);
			   move(LINES-1, 0);
			   refresh();
			   noscreen = 1;
			   /* 'I': ignore signal */
			   cont(cmd == 'C');
                           mdbmode();
			   break;
		/*
		 *	breakpoint handling
		 */

		case 'B' :
		case 'D' :
			   breakpoint(cmd);
			   break;
		/*
		 *	redraw screen
		 */

		case 014 :	/* control-L */
			   redraw();
			   break;

		/*
		 *	show run time error message
		 */

		case 'e' :
			print_rte();
			break;
		default :
			   message("unknown command");
			   break;
		}
	}
	while ( 1 );
}


/*
 *	show actual part of the "view" structure
 */

showwin(v)
	view **v;
{	int diff;
	int i;
	static view **old_v = NULL;
	static view **old_shifted = NULL;

	if ( !v || !*v )
	{	message("window is empty");
		return;
	}
	diff = dot ? dot-v : -1;
	if (diff > WIN_LINES-1)
	{	if (old_v && old_shifted && old_v == v &&
		   dot-old_shifted < WIN_LINES-1 && dot-old_shifted >= 0)
			v = old_shifted;
		else
		{	old_v = v;
			v = dot - WIN_LINES/2;
			old_shifted = v;
		}
		diff = dot-v;
	}
	else
	{	old_v = v;
		old_shifted = v;
	}
	wclear(win);
	while (*v && wprintw(win, "   %s\n", (*v)->v_line) != ERR )
		++v;
	if (dot)
		wmove(win, diff, 1);
	else
		wmove(win, 0, 1);
	wrefresh(win);
}

/*
 *	perror - screen orientated
 */

my_perror(name)
	char	*name;
{
	if ( errno >= 0 && errno < sys_nerr )
		message("%s: %s", name, sys_errlist[errno]);
	else
		message("%s: Unknown error", name);
}


/*
 *	the function `wgetstr' of the curses has a lot of errors...
 */

#define	CTRL_D	04

wgetstr(win, buf)
	WINDOW	*win;
	char	*buf;
{
	char	*buf_start;

	buf_start = buf;
	noecho();
	crmode();
	while ( (*buf = getchar()) != CTRL_D && *buf != '\n' )
	{	if (waddch(win, *buf) == ERR)
			return ERR;
		wrefresh(win);
		if ( *buf == '\b' )
			if ( buf > buf_start )
				--buf;
			else
				;
		else
			++buf;
	}
	*buf = '\0';
	return OK;
}

p_window()
{
	if (! coremode && !pid)
		cmderror("no core file");
	kill_view(act_view);
	act_view = backtrace();
	ppaneel("[P-window]");
	if (! pdot)
		pdot = init_dot();
}

d_window(t, mod)
	tree *t;
	struct module * mod;
{	view ** old;

	if (t && mod)
	{	old = act_view;
		if (mode == 'p' && pdot && (*pdot)->v_base)
			t->t_base = (*pdot)->v_base;
		act_view = data_view(t, mod);
		if (! act_view)
		{	act_view = old;
			cmderror(NULL);
		}
		kill_view(old);
		ppaneel("%s \"%s\" [D-window]", gettypstr(t) , t->t_name);
	}
	else
		cmderror("no D-window available");
}

/*
 *	we assume a m-window exists always
 */

m_window()
{
	kill_view(act_view);
	act_view = module_view();
	ppaneel("[M-window]");
}

c_window()
{	view ** old;
	tree * t;

	if (! coremode && !pid)
		cmderror("no core file");
	if ( !dot || !*dot || !(*dot)->v_node || !(*dot)->v_mod )
		cmderror("no C-window");
	old = act_view;
	if ( act_view = corewin( (*dot)->v_node, (*dot)->v_mod,
                                 (*dot)->v_addr, (*dot)->v_size) )
	{	kill_view(old);
		t = (*dot)->v_node;
		ppaneel("%s \"%s\" [C-window]", gettypstr(t) , t->t_name);
	}
	else
	{	act_view = old;
		cmderror(NULL);
	}
}

t_window(t, mod, line)
	tree * t;
	struct module * mod;
	int line;
{	view ** old;

	old = act_view;
	act_view = textwin(mod, t, line);
	if (! act_view)
	{	act_view = old;
		cmderror(NULL);
	}
	kill_view(old);
	ppaneel("%s \"%s\" [T-window]", gettypstr(t) , t->t_name);
	mode = 't';
}

die(code)
	int code;
{
	ppaneel("");
	resetty();
	move(LINES-1, 0);
	clearok(stdscr, FALSE);
	refresh();
	exit(code);
}

view **init_dot()
{	view **ptr;

	if (! act_view)
		return NULL;
	for ( ptr = act_view ; *ptr && (*ptr)->v_node == NULL ; ++ptr )
		;
	if (! *ptr)
		return NULL;
	return ptr;
}

char getcmd(get)
	int get;
{	char ch;
	static int init = 1;
	static int x, y;

	if (init)
		init = 0;
	else
	{	wmove(win, x, y);
		waddch(win, ' ');
	}
	crmode();
	noecho();
	noscreen = 0;
	showwin(act_view);
	if (! get)
		return;
	ch = getchar();
	getyx(win, x, y);
	/* visible character ? */
	if (ch >= ' ' && ch <= '~')
	{	waddch(win, ch);
		wmove(win, x, y);
	}
	wrefresh(win);
	return ch;
}

/*
 *	dispose the actual window
 *
 *	(only for D-, T- and C-windows)
 */

kill_view(tokill)
	view ** tokill;
{	view ** ptr;

	if (mode != 'd' && mode != 't' && mode != 'c')
		return;
	if (! tokill) return;
	for ( ptr = tokill ; *ptr ; ++ptr )
		cfree(*ptr);
	cfree(tokill);
}

/*
 *	change position of cursor
 */

avanti(offset)
{
	movedot(offset);
	showwin(act_view);
	if (mode == 'p')
		pdot = dot;
}

/*
 *	move dot
 *
 *	(*dot)->v_node should be non zero !
 */

movedot(offset)
{	view ** old_dot;
	int sgn;

	sgn = offset > 0 ? 1 : -1;
	old_dot = dot;
	if (! dot)
	{	dot = init_dot();
		if (! dot) return;
	}
	if (dot+offset < act_view)
	{	dot = init_dot();
		return;
	}
	while (offset && *dot)
	{	dot += sgn;
		offset -= sgn;
	}
	if (*dot && (*dot)->v_node) return;
	if ( sgn == 1 && !*dot && dot > act_view && *(dot-1) &&
	      (*(dot-1))->v_node)
	{	--dot;
		return;
	}
	while (dot > act_view && *dot && !(*dot)->v_node)
		dot += sgn;
	if (*dot && (*dot)->v_node) return;
	if (dot == act_view)
	{	dot = init_dot();
		return;
	}
	if (!*dot)
		dot = old_dot;
}

cmderror(mess)
{
	/*
	 *	attention: mess may be a signal number !
	 */

	if (noscreen)
		mdbmode();
	if (mess > 16)
		message(mess);
	longjmp(cmd_loop);
}

execcmd()
{	char buf[BUFSIZ];

	wclear(command);
	wprintw(command, "!");
	wrefresh(command);
	if ( wgetstr(command, buf) != OK )
		return;
	clearok(stdscr, FALSE);
	move(LINES-1, 0);
	refresh();
	resetty();
	printf("\n");		/* skip paneel */
	system(buf);
	printf("%s", LL);	/* to last line */
	printf("[Hit return]\r");
	crmode();
	getchar();
	redraw();
	wclear(command);
	wrefresh(command);
}

/*
 *	print format on paneel window
 */

ppaneel(cs, p1, p2, p3, p4, p5, p6)
{
	wmove(paneel, 0, 0);
	wclear(paneel);
	wprintw(paneel, cs, p1, p2, p3, p4, p5, p6);
	wrefresh(paneel);
}

gettypstr(t)
	tree * t;
{
	switch ( t->t_type ) {
	case T_MOD :	return "MOD";
	case T_PROC :	return "PROC";
	case T_VAR :	return "VAR";
	}
}

extern char * aoutfile;

/*
 *	read arguments (for child process) from input line
 */

getargs(buf)
	char * buf;
{
	noscreen = 0;
	message("Enter arguments and i/o redirection");
	wclear(command);
	wprintw(command, "%s ", aoutfile);
	wrefresh(command);
	strcpy(buf, aoutfile);
	strcat(buf, " ");
	wgetstr(command, buf+strlen(buf));
	ppaneel("");
	clearok(stdscr, FALSE);
	move(LINES-1, 0);
	refresh();
	resetty();
}

/*
 *	start "mdb"-mode after execution of child process
 */

mdbmode()
{	tree * old_node;
	tree * t;

	clearok(stdscr, FALSE);
	printf("%s", LL);	/* to last line */
	crmode();
	noecho();
	printf("[Hit return]\r");
	getchar();
	wclear(command);

	pdot = NULL;
	switch(mode)
	{	case 'm' :
		case 't' :
			break;
		case 'p' :
			if (pid || coremode)
				p_window();
			else
			{	mode = 'm';
				m_window();
			}
			break;
		case 'c' :
		case 'd' :
			if ( (pid || coremode) && dot && *dot )
			{	old_node = (*dot)->v_node;
				if (mode == 'c')
					c_window();
				else
				{	t = (*dot)->v_node;
					if (! t)
					{	mode = 'm';
						m_window();
						break;
					}
					if (t->t_father)
						t = t->t_father;
					d_window(t, (*dot)->v_mod);
					for ( dot = act_view ; *dot ; ++dot )
						if ((*dot)->v_node == old_node )
							break;
					if (! *dot)
						dot = init_dot();
				}
			}
			else
			{	mode = 'm';
				m_window();
			}
			break;
	}
	if (mode != 'd')
		dot = init_dot();
	showwin(act_view);
	redraw();
	noscreen = 0;
}

/*
 *	wrefresh(curscr) redraws the current screen,
 *	but the cursor is afterwards in home position.
 *	For this reason following...
 */

redraw()
{	int x, y;
	char ch;

	getyx(win, y, x);	/* is a macro ! */
	wrefresh(curscr);
	wmove(win, y, x);
	wrefresh(win);
}

/*
 *	following function for mdb_brkpt.c
 */

readcount(count)
	int count[2];
{	char buf[80];
	char * pattern;

	wclear(command);
	wprintw(command, "counter = ");
	wrefresh(command);
	wgetstr(command, buf);
	if (index(buf, ','))
		pattern = "%d , %d";
	else
		pattern = "%d %d";
	switch(sscanf(buf, pattern, & count[0], & count[1]))
	{	case EOF : case 0 : count[0] = 0; count[1] = 0; break;
		case 1 : count[1] = 0; break;
	}
	wclear(command);
	wrefresh(command);
}

breakpoint(cmd)
	char cmd;
{	char * err;
	tree * t;

	err = "cannot set breakpoint";
	if ( !dot || !*dot || !(*dot)->v_node || !(*dot)->v_mod )
		cmderror(err);
	t = (*dot)->v_node;
	if (t->t_type != T_PROC)
		cmderror("no procedure");
	if (init_brkpt((*dot)->v_mod))
		cmderror(err);
	switch (cmd)
	{	case 'B' :	set_brkpt(t); break;
		case 'D' :	del_brkpt(t); break;
	}
}

print_rte()
{
	message(error_mess());
}

write_win()
{	char buf[80];
	FILE * fp;
	int x, y;
	char * old_mess = malloc(MESS_COLS+1);

	if (noscreen)
		return;

	/* save old message */
	for (y = 0; y < MESS_COLS; ++y)
	{	wmove(stdscr, MESS_LINE, y);
		old_mess[y] = winch(stdscr);
	}
	while (y > 0 && old_mess[--y] == ' ')
		;
	old_mess[++y] = '\0';

	message("Enter output file name");
	getfilename(buf, 80);
	if ((fp = fopen(buf, "a")) == NULL)
	{	my_perror(buf);
		return;
	}

	/* restore old window */
	getcmd(FALSE);
	message(old_mess);

	/* print screen contents into file */
	for (x = 0; x < LINES; ++x)
	{	for (y = 0; y < COLS-1; ++y)
		{	wmove(stdscr, x, y);
			putc(winch(stdscr), fp);
		}
		putc('\n', fp);
	}
	fclose(fp);
}

p_switch()
{	tree * t;
	int addr;
	struct process pr;

	if (! get_pdesc(& addr))
	{	if ( mode != 'd' )
		{	message("only in D-window allowed");
			return;
		}
		t = (*dot)->v_node;
		if (! t)
		{	message("empty node");
			return;
		}
		addr = getvaraddr(t);
		if (! addr) return;
		if (t->t_vartype != Process)
		{	message("no PROCESS");
			return;
		}
		getlong(addr, & addr);
	}
	if (addr % 4)
	{	message("process descriptor address not aligned");
		return;
	}
	get(addr, & pr, sizeof(struct process));
	new_stack(& pr);
	p_window();
	mode = 'p';
	pdot = dot = init_dot();
	if (! dot)
	{	message("PROCESS undefined");
		new_stack(NULL);
		p_window();
		pdot = dot = init_dot();
	}
	showwin(act_view);
}

#endif /* ! LINE */
