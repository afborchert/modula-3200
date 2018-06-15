#-------------------------------------------------------------
# (c) University of Ulm, Sektion Informatik, D-7900 Ulm -- afb
#-------------------------------------------------------------
# {updated by mmm: Fri Sep 30 15:02:10 1988 MEZ}
# {flags: -a+ -e- -l- -L+ -m- -1- -v-}
# {rules}
.SUFFIXES:
.SUFFIXES:	.o .sy .m2 .d
MFLAGS =	
M2C =		m2c
.m2.o:
		$(M2C) -c $(MFLAGS) $*.m2
.d.a:
		$(M2C) $(MFLAGS) $*.d
# {macros}
SRC =		MCSymFile.m2
TAGSRC =	MCSymFile.m2
OBJS =		MCSymFile.o
SYMS =		
REFS =		MCSymFile.r
# {precious}
.PRECIOUS:	SYM
# {all}
T =		msym
all:		$T
# {linkage}
LIBS =		mee.a
LDFLAGS =	
$T:		MCSymFile.o
		$(M2C) $(MFLAGS) -o $@ $(LDFLAGS) MCSymFile.o $(LIBS)
# {dependencies}
MCSymFile.o:	/u/lib/modula/SYM(Conversions.sy) ../SYM(FtdIO.sy) \
		../SYM(MCBase.sy) ../SYM(MCHalfword.sy) ../SYM(MCStop.sy) \
		../SYM(MCSymFileDe.sy) /u/lib/modula/SYM(StdIO.sy) \
		../SYM(Storage.sy) ../SYM(SysPerror.sy)
#
tags:		$(TAGSRC)
		mtags $(TAGSRC)
#
SPOOL =		lp
pr:		Pr.date
Pr.date:	$(SRC)
		pr $? | $(SPOOL)
		-touch Pr.date
#
clean:		; rm -f $(OBJS) $(SYMS) $(REFS)
