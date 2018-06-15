/*
 *	m2e -- fetch error messages
 */

#include	<stdio.h>
#ifdef xelos
#define rindex(s,c)	strrchr(s,c)
#endif

#ifdef LOCAL
#	ifdef xelos
#		define	ERROR	"/usr/modula/borchert/lib/m2_error"
#	else
#		define	ERROR	"/u/modula/borchert/lib/m2_error"
#	endif
#else
#	if defined(xelos) && defined(ULM)
#		define	ERROR	"/u/lib/modula/m2_error"
#	else
#		define	ERROR	"/usr/lib/modula/m2_error"
#	endif
#endif LOCAL

#define	MAXERRS	400

#define	TRUE	1
#define	FALSE	0

static	long	messages[MAXERRS];
static	FILE	*fp;
static	int	only_numbers;

errnr_init ()
{
	char	buf[BUFSIZ];
	long	pos;
	int	index;
	char * modlib;
	char * error_file;

	if (modlib = getenv("MODLIB"))
	{	error_file = malloc(strlen(modlib)+10);
		strcpy(error_file, modlib);
		strcat(error_file, "/m2_error");
	}
	else
		error_file = ERROR;
	if ( (fp = fopen(error_file, "r")) == NULL )
	{	only_numbers = TRUE;
		if (modlib)
			free(error_file);
		return;
	}
	if (modlib)
		free(error_file);
	only_numbers = FALSE;
	pos = 0;
	while ( fgets(buf,BUFSIZ,fp) )
	{	switch ( sscanf(buf,"%d",&index) ) {
#ifdef DEBUG
		case 0 :
			fatal("in `error'-file : error number missing");

#endif DEBUG
		default :
#ifdef	DEBUG
			if ( messages[index] )
				fatal("in `error'-file : error number %d twice declared",index);
#endif	DEBUG
			messages[index] = pos;

		}
		pos = ftell(fp);
	}
}

errnr(nr)
	int	nr;
{	char	buf[BUFSIZ];
	char * ptr;

	if (nr == 0 || messages[nr])
	{	fseek ( fp , messages[nr] , 0 );
		fscanf( fp , "%d" , &nr );
		fgets(buf, BUFSIZ, fp);
		if (ptr = rindex(buf, '\n'))
			*ptr = '\0';
		if ( buf[0] == '\t' )
			error("%s", buf+1);
		else
			error("%s", buf);
	}
	else
		error("error code : %d\n",nr);
}
