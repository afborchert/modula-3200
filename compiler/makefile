#-------------------------------------------------------------
# (c) University of Ulm, Sektion Informatik, D-7900 Ulm -- afb
#-------------------------------------------------------------
# {updated by mmm: Fri Sep 30 14:27:32 1988 MEZ}
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
SRC =		Archive.d Archive.m2 FtdIO.d FtdIO.m2 MCBase.d MCBase.m2 \
		MCBigSet.d MCBigSet.m2 MCDump.d MCDump.m2 MCHalfword.d \
		MCHalfword.m2 MCOperation.d MCOperation.m2 MCStop.d \
		MCStop.m2 MCSymFileDe.d MCSymFileDe.m2 MCTypes.d MCTypes.m2 \
		Storage.d Storage.m2 SysPanic.d SysPanic.m2 SysPerror.d \
		SysPerror.m2
TAGSRC =	Archive.m2 FtdIO.m2 MCBase.m2 MCBigSet.m2 MCDump.m2 \
		MCHalfword.m2 MCOperation.m2 MCStop.m2 MCSymFileDe.m2 \
		MCTypes.m2 Storage.m2 SysPanic.m2 SysPerror.m2
OBJS =		
SYMS =		
REFS =		Archive.r FtdIO.r MCBase.r MCBigSet.r MCDump.r MCHalfword.r \
		MCOperation.r MCStop.r MCSymFileDe.r MCTypes.r Storage.r \
		SysPanic.r SysPerror.r
# {precious}
LIB =		mee.a
.PRECIOUS:	$(LIB) SYM
# {all}
all:		$(LIB)
# {linkage}
LIBS =		
LDFLAGS =	
# {dependencies}
$(LIB):		$(LIB)(Archive.o) $(LIB)(FtdIO.o) $(LIB)(MCBase.o) \
		$(LIB)(MCBigSet.o) $(LIB)(MCDump.o) $(LIB)(MCHalfword.o) \
		$(LIB)(MCOperation.o) $(LIB)(MCStop.o) $(LIB)(MCSymFileDe.o) \
		$(LIB)(MCTypes.o) $(LIB)(Storage.o) $(LIB)(SysPanic.o) \
		$(LIB)(SysPerror.o)
SYM(Archive.sy): Archive.d /u/lib/modula/SYM(SystemTypes.sy); \
		$(M2C) $(MFLAGS) Archive.d
$(LIB)(Archive.o): /u/lib/modula/SYM(ASCII.sy) /u/lib/modula/SYM(StdIO.sy) \
		SYM(Storage.sy) /u/lib/modula/SYM(StrToNum.sy) \
		SYM(SysPanic.sy) /u/lib/modula/SYM(SystemTypes.sy) \
		SYM(Archive.sy) SYM(FtdIO.sy) Archive.m2
		$(M2C) $(MFLAGS) -c Archive.m2
		ar rv $(LIB) Archive.o && rm -f Archive.o
SYM(FtdIO.sy):	FtdIO.d /u/lib/modula/SYM(StdIO.sy); $(M2C) $(MFLAGS) \
		FtdIO.d
$(LIB)(FtdIO.o): /u/lib/modula/SYM(Conversions.sy) \
		/u/lib/modula/SYM(ReadIntCard.sy) \
		/u/lib/modula/SYM(StdIO.sy) SYM(FtdIO.sy) FtdIO.m2
		$(M2C) $(MFLAGS) -c FtdIO.m2
		ar rv $(LIB) FtdIO.o && rm -f FtdIO.o
SYM(MCBase.sy):	MCBase.d; $(M2C) $(MFLAGS) MCBase.d
$(LIB)(MCBase.o): SYM(MCBase.sy) MCBase.m2
		$(M2C) $(MFLAGS) -c MCBase.m2
		ar rv $(LIB) MCBase.o && rm -f MCBase.o
SYM(MCBigSet.sy): MCBigSet.d SYM(MCBase.sy); $(M2C) $(MFLAGS) MCBigSet.d
$(LIB)(MCBigSet.o): SYM(MCBase.sy) SYM(MCBigSet.sy) SYM(Storage.sy) \
		MCBigSet.m2
		$(M2C) $(MFLAGS) -c MCBigSet.m2
		ar rv $(LIB) MCBigSet.o && rm -f MCBigSet.o
SYM(MCDump.sy):	MCDump.d SYM(MCBase.sy); $(M2C) $(MFLAGS) MCDump.d
$(LIB)(MCDump.o): /u/lib/modula/SYM(ASCII.sy) /u/lib/modula/SYM(InOut.sy) \
		SYM(MCBase.sy) SYM(MCDump.sy) SYM(MCTypes.sy) MCDump.m2
		$(M2C) $(MFLAGS) -c MCDump.m2
		ar rv $(LIB) MCDump.o && rm -f MCDump.o
SYM(MCHalfword.sy): MCHalfword.d /u/lib/modula/SYM(StdIO.sy); \
		$(M2C) $(MFLAGS) MCHalfword.d
$(LIB)(MCHalfword.o): SYM(MCStop.sy) /u/lib/modula/SYM(StdIO.sy) \
		SYM(SysPerror.sy) SYM(MCHalfword.sy) MCHalfword.m2
		$(M2C) $(MFLAGS) -c MCHalfword.m2
		ar rv $(LIB) MCHalfword.o && rm -f MCHalfword.o
SYM(MCOperation.sy): MCOperation.d SYM(MCBase.sy); $(M2C) $(MFLAGS) \
		MCOperation.d
$(LIB)(MCOperation.o): SYM(MCBase.sy) SYM(MCBigSet.sy) SYM(MCOperation.sy) \
		SYM(MCTypes.sy) MCOperation.m2
		$(M2C) $(MFLAGS) -c MCOperation.m2
		ar rv $(LIB) MCOperation.o && rm -f MCOperation.o
SYM(MCStop.sy):	MCStop.d; $(M2C) $(MFLAGS) MCStop.d
$(LIB)(MCStop.o): SYM(MCStop.sy) /u/lib/modula/SYM(StdIO.sy) \
		/u/lib/modula/SYM(SysExit.sy) MCStop.m2
		$(M2C) $(MFLAGS) -c MCStop.m2
		ar rv $(LIB) MCStop.o && rm -f MCStop.o
SYM(MCSymFileDe.sy): MCSymFileDe.d; $(M2C) $(MFLAGS) MCSymFileDe.d
$(LIB)(MCSymFileDe.o): SYM(MCSymFileDe.sy) MCSymFileDe.m2
		$(M2C) $(MFLAGS) -c MCSymFileDe.m2
		ar rv $(LIB) MCSymFileDe.o && rm -f MCSymFileDe.o
SYM(MCTypes.sy): MCTypes.d SYM(MCBase.sy); $(M2C) $(MFLAGS) MCTypes.d
$(LIB)(MCTypes.o): SYM(MCBase.sy) SYM(MCTypes.sy) SYM(Storage.sy) MCTypes.m2
		$(M2C) $(MFLAGS) -c MCTypes.m2
		ar rv $(LIB) MCTypes.o && rm -f MCTypes.o
SYM(Storage.sy): Storage.d; $(M2C) $(MFLAGS) Storage.d
$(LIB)(Storage.o): SYM(MCBase.sy) SYM(Storage.sy) \
		/u/lib/modula/SYM(SysBreak.sy) \
		/u/lib/modula/SYM(SysClose.sy) /u/lib/modula/SYM(SysOpen.sy) \
		SYM(SysPanic.sy) /u/lib/modula/SYM(SysRead.sy) \
		/u/lib/modula/SYM(SysWrite.sy) Storage.m2
		$(M2C) $(MFLAGS) -c Storage.m2
		ar rv $(LIB) Storage.o && rm -f Storage.o
SYM(SysPanic.sy): SysPanic.d; $(M2C) $(MFLAGS) SysPanic.d
$(LIB)(SysPanic.o): /u/lib/modula/SYM(SysExit.sy) SYM(SysPanic.sy) \
		/u/lib/modula/SYM(SysWrite.sy) SysPanic.m2
		$(M2C) $(MFLAGS) -c SysPanic.m2
		ar rv $(LIB) SysPanic.o && rm -f SysPanic.o
SYM(SysPerror.sy): SysPerror.d; $(M2C) $(MFLAGS) SysPerror.d
$(LIB)(SysPerror.o): /u/lib/modula/SYM(Errno.sy) /u/lib/modula/SYM(StdIO.sy) \
		SYM(SysPerror.sy) SYM(FtdIO.sy) SysPerror.m2
		$(M2C) $(MFLAGS) -c SysPerror.m2
		ar rv $(LIB) SysPerror.o && rm -f SysPerror.o
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
