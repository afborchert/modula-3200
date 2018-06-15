# {updated by mmm: Thu Mar 24 15:21:12 1988 MEZ}
# {flags: -a- -l- -L- -1- -v-}
# {rules}
.SUFFIXES:
.SUFFIXES:	.o .sy .m2 .d
MFLAGS =	
M2C =		m2c
.m2.o:
		$(M2C) -c $(MFLAGS) $*.m2
.d.sy:
		$(M2C) $(MFLAGS) $*.d
# {sources}
SRC =		Errors.d Errors.m2 FileNames.d FileNames.m2 Library.d \
		Library.m2 Macros.d Macros.m2 Makefile.d Makefile.m2 \
		Options.d Options.m2 Out.d Out.m2 Scan.d Scan.m2 Sources.d \
		Sources.m2 Suffix.d Suffix.m2 SymTab.d SymTab.m2 TopSort.d \
		TopSort.m2 Update.d Update.m2 Write.d Write.m2 mmm.m2
# {all}
all:		mmm
# {linkage}
LDFLAGS =	
mmm:		Errors.o FileNames.o Library.o Macros.o Makefile.o Options.o \
		Out.o Scan.o Sources.o Suffix.o SymTab.o TopSort.o Update.o \
		Write.o mmm.o
		$(M2C) -o $@ $(LDFLAGS) Errors.o FileNames.o Library.o \
		Macros.o Makefile.o Options.o Out.o Scan.o Sources.o \
		Suffix.o SymTab.o TopSort.o Update.o Write.o mmm.o
# {dependencies}
Errors.sy:	
Errors.o:	Errors.sy
FileNames.sy:	
FileNames.o:	Errors.sy FileNames.sy SymTab.sy
Library.sy:	
Library.o:	Errors.sy FileNames.sy Library.sy Suffix.sy SymTab.sy
Macros.sy:	
Macros.o:	Macros.sy Out.sy
Makefile.sy:	
Makefile.o:	FileNames.sy Macros.sy Makefile.sy Options.sy Out.sy \
		Suffix.sy SymTab.sy Write.sy
Options.sy:	
Options.o:	Options.sy
Out.sy:		
Out.o:		Errors.sy Out.sy
Scan.sy:	FileNames.sy SymTab.sy
Scan.o:		Errors.sy FileNames.sy Scan.sy
Sources.sy:	
Sources.o:	Errors.sy FileNames.sy Options.sy Scan.sy Sources.sy \
		SymTab.sy
Suffix.sy:	FileNames.sy SymTab.sy
Suffix.o:	Errors.sy FileNames.sy Suffix.sy SymTab.sy
SymTab.sy:	FileNames.sy
SymTab.o:	Errors.sy FileNames.sy Options.sy Suffix.sy SymTab.sy
TopSort.sy:	SymTab.sy
TopSort.o:	Errors.sy SymTab.sy TopSort.sy
Update.sy:	
Update.o:	Errors.sy FileNames.sy Macros.sy Makefile.sy Options.sy \
		Out.sy Update.sy
Write.sy:	FileNames.sy SymTab.sy
Write.o:	FileNames.sy Out.sy SymTab.sy TopSort.sy Write.sy
mmm.o:		Errors.sy FileNames.sy Library.sy Makefile.sy Options.sy \
		Sources.sy SymTab.sy TopSort.sy Update.sy
# printing
PR = pr
SPOOL = lp
pr:		Pr.date

Pr.date:	${SRC}
		${PR} $? | ${SPOOL}
		-touch Pr.date
