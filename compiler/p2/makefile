#-------------------------------------------------------------
# (c) University of Ulm, Sektion Informatik, D-7900 Ulm -- afb
#-------------------------------------------------------------
# {updated by mmm: Fri Sep 30 14:39:23 1988 MEZ}
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
SRC =		MCP2IO.d MCP2IO.m2 MCP2Ident.d MCP2Ident.m2 MCP2Public.d \
		MCP2Public.m2 MCP2Referen.d MCP2Referen.m2 MCPass2.m2
TAGSRC =	MCP2IO.m2 MCP2Ident.m2 MCP2Public.m2 MCP2Referen.m2 \
		MCPass2.m2
OBJS =		MCPass2.o
SYMS =		
REFS =		MCP2IO.r MCP2Ident.r MCP2Public.r MCP2Referen.r MCPass2.r
# {precious}
LIB =		p2.a
.PRECIOUS:	$(LIB) SYM
# {all}
T =		m1
all:		$T
# {linkage}
LIBS =		mee.a
LDFLAGS =	
$T:		MCPass2.o $(LIB)(MCP2IO.o) $(LIB)(MCP2Ident.o) \
		$(LIB)(MCP2Public.o) $(LIB)(MCP2Referen.o)
		$(M2C) $(MFLAGS) -o $@ $(LDFLAGS) MCPass2.o $(LIB) $(LIBS)
# {dependencies}
$(LIB):		$(LIB)(MCP2IO.o) $(LIB)(MCP2Ident.o) $(LIB)(MCP2Public.o) \
		$(LIB)(MCP2Referen.o)
SYM(MCP2IO.sy):	MCP2IO.d ../SYM(MCBase.sy); $(M2C) $(MFLAGS) MCP2IO.d
$(LIB)(MCP2IO.o): ../SYM(FtdIO.sy) ../SYM(MCBase.sy) ../SYM(MCHalfword.sy) \
		SYM(MCP2IO.sy) SYM(MCP2Public.sy) ../SYM(MCStop.sy) \
		/u/lib/modula/SYM(StdIO.sy) ../SYM(SysPerror.sy) MCP2IO.m2
		$(M2C) $(MFLAGS) -c MCP2IO.m2
		ar rv $(LIB) MCP2IO.o && rm -f MCP2IO.o
SYM(MCP2Ident.sy): MCP2Ident.d ../SYM(MCBase.sy) SYM(MCP2IO.sy); \
		$(M2C) $(MFLAGS) MCP2Ident.d
$(LIB)(MCP2Ident.o): ../SYM(MCBase.sy) SYM(MCP2IO.sy) SYM(MCP2Ident.sy) \
		../SYM(Storage.sy) MCP2Ident.m2
		$(M2C) $(MFLAGS) -c MCP2Ident.m2
		ar rv $(LIB) MCP2Ident.o && rm -f MCP2Ident.o
SYM(MCP2Public.sy): MCP2Public.d; $(M2C) $(MFLAGS) MCP2Public.d
$(LIB)(MCP2Public.o): ../SYM(FtdIO.sy) ../SYM(MCBase.sy) SYM(MCP2Public.sy) \
		../SYM(MCStop.sy) /u/lib/modula/SYM(StdIO.sy) \
		../SYM(SysPerror.sy) MCP2Public.m2
		$(M2C) $(MFLAGS) -c MCP2Public.m2
		ar rv $(LIB) MCP2Public.o && rm -f MCP2Public.o
SYM(MCP2Referen.sy): MCP2Referen.d ../SYM(MCBase.sy); $(M2C) $(MFLAGS) \
		MCP2Referen.d
$(LIB)(MCP2Referen.o): ../SYM(MCBase.sy) ../SYM(MCHalfword.sy) \
		SYM(MCP2IO.sy) SYM(MCP2Public.sy) SYM(MCP2Referen.sy) \
		../SYM(MCStop.sy) /u/lib/modula/SYM(StdIO.sy) \
		../SYM(SysPerror.sy) MCP2Referen.m2
		$(M2C) $(MFLAGS) -c MCP2Referen.m2
		ar rv $(LIB) MCP2Referen.o && rm -f MCP2Referen.o
MCPass2.o:	../SYM(MCBase.sy) ../SYM(MCBigSet.sy) ../SYM(MCOperation.sy) \
		SYM(MCP2IO.sy) SYM(MCP2Ident.sy) SYM(MCP2Public.sy) \
		SYM(MCP2Referen.sy) ../SYM(MCStop.sy) ../SYM(MCTypes.sy) \
		../SYM(Storage.sy)
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
