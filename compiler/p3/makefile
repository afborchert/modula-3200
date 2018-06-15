#-------------------------------------------------------------
# (c) University of Ulm, Sektion Informatik, D-7900 Ulm -- afb
#-------------------------------------------------------------
# {updated by mmm: Fri Sep 30 14:44:30 1988 MEZ}
# {flags: -a+ -e- -l+ -L+ -m- -1- -v-}
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
SRC =		MCP3IO.d MCP3IO.m2 MCP3Ident.d MCP3Ident.m2 MCP3Public.d \
		MCP3Public.m2 MCPass3.m2
TAGSRC =	MCP3IO.m2 MCP3Ident.m2 MCP3Public.m2 MCPass3.m2
OBJS =		MCPass3.o
SYMS =		
REFS =		MCP3IO.r MCP3Ident.r MCP3Public.r MCPass3.r
# {precious}
LIB =		p3.a
.PRECIOUS:	$(LIB) SYM
# {all}
T =		m2
all:		$T
# {linkage}
LIBS =		mee.a
LDFLAGS =	
$T:		MCPass3.o $(LIB)(MCP3IO.o) $(LIB)(MCP3Ident.o) \
		$(LIB)(MCP3Public.o)
		$(M2C) $(MFLAGS) -o $@ $(LDFLAGS) MCPass3.o $(LIB) $(LIBS)
# {dependencies}
$(LIB):		$(LIB)(MCP3IO.o) $(LIB)(MCP3Ident.o) $(LIB)(MCP3Public.o)
SYM(MCP3IO.sy):	MCP3IO.d ../SYM(MCBase.sy); $(M2C) $(MFLAGS) MCP3IO.d
$(LIB)(MCP3IO.o): ../SYM(FtdIO.sy) ../SYM(MCBase.sy) ../SYM(MCHalfword.sy) \
		SYM(MCP3IO.sy) SYM(MCP3Public.sy) ../SYM(MCStop.sy) \
		/u/lib/modula/SYM(StdIO.sy) ../SYM(Storage.sy) \
		../SYM(SysPerror.sy) MCP3IO.m2
		$(M2C) $(MFLAGS) -c MCP3IO.m2
		ar rv $(LIB) MCP3IO.o && rm -f MCP3IO.o
SYM(MCP3Ident.sy): MCP3Ident.d ../SYM(MCBase.sy) SYM(MCP3IO.sy); \
		$(M2C) $(MFLAGS) MCP3Ident.d
$(LIB)(MCP3Ident.o): ../SYM(MCBase.sy) SYM(MCP3IO.sy) SYM(MCP3Ident.sy) \
		../SYM(Storage.sy) MCP3Ident.m2
		$(M2C) $(MFLAGS) -c MCP3Ident.m2
		ar rv $(LIB) MCP3Ident.o && rm -f MCP3Ident.o
SYM(MCP3Public.sy): MCP3Public.d; $(M2C) $(MFLAGS) MCP3Public.d
$(LIB)(MCP3Public.o): ../SYM(FtdIO.sy) ../SYM(MCBase.sy) SYM(MCP3Public.sy) \
		../SYM(MCStop.sy) /u/lib/modula/SYM(StdIO.sy) \
		../SYM(SysPerror.sy) MCP3Public.m2
		$(M2C) $(MFLAGS) -c MCP3Public.m2
		ar rv $(LIB) MCP3Public.o && rm -f MCP3Public.o
MCPass3.o:	../SYM(MCBase.sy) ../SYM(MCBigSet.sy) ../SYM(MCOperation.sy) \
		SYM(MCP3IO.sy) SYM(MCP3Ident.sy) SYM(MCP3Public.sy) \
		../SYM(MCStop.sy) ../SYM(MCTypes.sy) ../SYM(Storage.sy)
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
