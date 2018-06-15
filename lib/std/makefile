# {updated by mmm: Mon Nov 20 14:19:52 1989 MEZ}
# {flags: -a+ -e- -l+ -L+ -m+ -1- -v-}
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
SRC =		ASCII.d ASCII.m2 Archive.d Archive.m2 Arguments.d \
		Arguments.m2 Calendar.d Calendar.m2 CallShell.d CallShell.m2 \
		Clock.d Clock.m2 CoExpressio.d CoExpressio.m2 Conversions.d \
		Conversions.m2 Environment.d Environment.m2 EtcGroup.d \
		EtcGroup.m2 Files.d Files.m2 FtdIO.d FtdIO.m2 Functions.d \
		Functions.m2 GetPass.d GetPass.m2 InOut.d InOut.m2 MathLib.d \
		MathLib.m2 Passwd.d Passwd.m2 PipeIO.d PipeIO.m2 Plot.d \
		Plot.m2 PrioTab.d PrioTab.m2 Processes.d Processes.m2 \
		RandomGener.d RandomGener.m2 ReadIntCard.d ReadIntCard.m2 \
		RealConv.d RealConv.m2 RealInOut.d RealInOut.m2 ScanPwfile.d \
		ScanPwfile.m2 StdFuncs.d StdFuncs.m2 StdIO.d StdIO.m2 \
		Storage.d Storage.m2 StrSpec.d StrSpec.m2 StrToNum.d \
		StrToNum.m2 Strings.d Strings.m2 SysPerror.d SysPerror.m2 \
		SysSema.d SysSema.m2 Terminal.d Terminal.m2 TimeIO.d \
		TimeIO.m2
TAGSRC =	ASCII.m2 Archive.m2 Arguments.m2 Calendar.m2 CallShell.m2 \
		Clock.m2 CoExpressio.m2 Conversions.m2 Environment.m2 \
		EtcGroup.m2 Files.m2 FtdIO.m2 Functions.m2 GetPass.m2 \
		InOut.m2 MathLib.m2 Passwd.m2 PipeIO.m2 Plot.m2 PrioTab.m2 \
		Processes.m2 RandomGener.m2 ReadIntCard.m2 RealConv.m2 \
		RealInOut.m2 ScanPwfile.m2 StdFuncs.m2 StdIO.m2 Storage.m2 \
		StrSpec.m2 StrToNum.m2 Strings.m2 SysPerror.m2 SysSema.m2 \
		Terminal.m2 TimeIO.m2
OBJS =		
SYMS =		
REFS =		ASCII.r Archive.r Arguments.r Calendar.r CallShell.r Clock.r \
		CoExpressio.r Conversions.r Environment.r EtcGroup.r Files.r \
		FtdIO.r Functions.r GetPass.r InOut.r MathLib.r Passwd.r \
		PipeIO.r Plot.r PrioTab.r Processes.r RandomGener.r \
		ReadIntCard.r RealConv.r RealInOut.r ScanPwfile.r StdFuncs.r \
		StdIO.r Storage.r StrSpec.r StrToNum.r Strings.r SysPerror.r \
		SysSema.r Terminal.r TimeIO.r
# {all}
LIB =		libm2.a
PLIB =		plibm2.a
all:		$(LIB) $(PLIB)
# {linkage}
LIBS =		
LDFLAGS =	
# {dependencies}
$(LIB):		$(LIB)(ASCII.o) $(LIB)(Archive.o) $(LIB)(Arguments.o) \
		$(LIB)(Calendar.o) $(LIB)(CallShell.o) $(LIB)(Clock.o) \
		$(LIB)(CoExpressio.o) $(LIB)(Conversions.o) \
		$(LIB)(Environment.o) $(LIB)(EtcGroup.o) $(LIB)(Files.o) \
		$(LIB)(FtdIO.o) $(LIB)(Functions.o) $(LIB)(GetPass.o) \
		$(LIB)(InOut.o) $(LIB)(MathLib.o) $(LIB)(Passwd.o) \
		$(LIB)(PipeIO.o) $(LIB)(Plot.o) $(LIB)(PrioTab.o) \
		$(LIB)(Processes.o) $(LIB)(RandomGener.o) \
		$(LIB)(ReadIntCard.o) $(LIB)(RealConv.o) $(LIB)(RealInOut.o) \
		$(LIB)(ScanPwfile.o) $(LIB)(StdFuncs.o) $(LIB)(StdIO.o) \
		$(LIB)(Storage.o) $(LIB)(StrSpec.o) $(LIB)(StrToNum.o) \
		$(LIB)(Strings.o) $(LIB)(SysPerror.o) $(LIB)(SysSema.o) \
		$(LIB)(Terminal.o) $(LIB)(TimeIO.o)
$(PLIB):	$(PLIB)(ASCII.o) $(PLIB)(Archive.o) $(PLIB)(Arguments.o) \
		$(PLIB)(Calendar.o) $(PLIB)(CallShell.o) $(PLIB)(Clock.o) \
		$(PLIB)(CoExpressio.o) $(PLIB)(Conversions.o) \
		$(PLIB)(Environment.o) $(PLIB)(EtcGroup.o) $(PLIB)(Files.o) \
		$(PLIB)(FtdIO.o) $(PLIB)(Functions.o) $(PLIB)(GetPass.o) \
		$(PLIB)(InOut.o) $(PLIB)(MathLib.o) $(PLIB)(Passwd.o) \
		$(PLIB)(PipeIO.o) $(PLIB)(Plot.o) $(PLIB)(PrioTab.o) \
		$(PLIB)(Processes.o) $(PLIB)(RandomGener.o) \
		$(PLIB)(ReadIntCard.o) $(PLIB)(RealConv.o) \
		$(PLIB)(RealInOut.o) $(PLIB)(ScanPwfile.o) \
		$(PLIB)(StdFuncs.o) $(PLIB)(StdIO.o) $(PLIB)(Storage.o) \
		$(PLIB)(StrSpec.o) $(PLIB)(StrToNum.o) $(PLIB)(Strings.o) \
		$(PLIB)(SysPerror.o) $(PLIB)(SysSema.o) $(PLIB)(Terminal.o) \
		$(PLIB)(TimeIO.o)
SYM(ASCII.sy):	ASCII.d; $(M2C) $(MFLAGS) ASCII.d
$(LIB)(ASCII.o): SYM(ASCII.sy) ASCII.m2
		$(M2C) $(MFLAGS) -c ASCII.m2
		ar rv $(LIB) ASCII.o && rm -f ASCII.o
$(PLIB)(ASCII.o): SYM(ASCII.sy) ASCII.m2
		$(M2C) $(MFLAGS) -c -p ASCII.m2
		ar rv $(PLIB) ASCII.o && rm -f ASCII.o
SYM(Archive.sy): Archive.d /u/lib/modula/SYM(SystemTypes.sy); \
		$(M2C) $(MFLAGS) Archive.d
$(LIB)(Archive.o): SYM(ASCII.sy) SYM(Storage.sy) SYM(StrToNum.sy) \
		/u/lib/modula/SYM(SysPanic.sy) \
		/u/lib/modula/SYM(SystemTypes.sy) SYM(Archive.sy) \
		SYM(StdIO.sy) SYM(FtdIO.sy) Archive.m2
		$(M2C) $(MFLAGS) -c Archive.m2
		ar rv $(LIB) Archive.o && rm -f Archive.o
$(PLIB)(Archive.o): SYM(ASCII.sy) SYM(Storage.sy) SYM(StrToNum.sy) \
		/u/lib/modula/SYM(SysPanic.sy) \
		/u/lib/modula/SYM(SystemTypes.sy) SYM(Archive.sy) \
		SYM(StdIO.sy) SYM(FtdIO.sy) Archive.m2
		$(M2C) $(MFLAGS) -c -p Archive.m2
		ar rv $(PLIB) Archive.o && rm -f Archive.o
SYM(Arguments.sy): Arguments.d; $(M2C) $(MFLAGS) Arguments.d
$(LIB)(Arguments.o): SYM(ASCII.sy) SYM(Arguments.sy) SYM(StrSpec.sy) \
		SYM(StrToNum.sy) SYM(Strings.sy) \
		/u/lib/modula/SYM(SysExit.sy) SYM(StdIO.sy) Arguments.m2
		$(M2C) $(MFLAGS) -c Arguments.m2
		ar rv $(LIB) Arguments.o && rm -f Arguments.o
$(PLIB)(Arguments.o): SYM(ASCII.sy) SYM(Arguments.sy) SYM(StrSpec.sy) \
		SYM(StrToNum.sy) SYM(Strings.sy) \
		/u/lib/modula/SYM(SysExit.sy) SYM(StdIO.sy) Arguments.m2
		$(M2C) $(MFLAGS) -c -p Arguments.m2
		ar rv $(PLIB) Arguments.o && rm -f Arguments.o
SYM(Calendar.sy): Calendar.d /u/lib/modula/SYM(SystemTypes.sy); \
		$(M2C) $(MFLAGS) Calendar.d
$(LIB)(Calendar.o): SYM(Environment.sy) SYM(Strings.sy) \
		/u/lib/modula/SYM(SysTime.sy) SYM(Calendar.sy) Calendar.m2
		$(M2C) $(MFLAGS) -c Calendar.m2
		ar rv $(LIB) Calendar.o && rm -f Calendar.o
$(PLIB)(Calendar.o): SYM(Environment.sy) SYM(Strings.sy) \
		/u/lib/modula/SYM(SysTime.sy) SYM(Calendar.sy) Calendar.m2
		$(M2C) $(MFLAGS) -c -p Calendar.m2
		ar rv $(PLIB) Calendar.o && rm -f Calendar.o
SYM(CallShell.sy): CallShell.d; $(M2C) $(MFLAGS) CallShell.d
$(LIB)(CallShell.o): SYM(CallShell.sy) /u/lib/modula/SYM(SysExec.sy) \
		/u/lib/modula/SYM(SysExit.sy) /u/lib/modula/SYM(SysFork.sy) \
		/u/lib/modula/SYM(SysSignal.sy) \
		/u/lib/modula/SYM(SysWait.sy) \
		/u/lib/modula/SYM(SystemTypes.sy) \
		/u/lib/modula/SYM(UnixString.sy) CallShell.m2
		$(M2C) $(MFLAGS) -c CallShell.m2
		ar rv $(LIB) CallShell.o && rm -f CallShell.o
$(PLIB)(CallShell.o): SYM(CallShell.sy) /u/lib/modula/SYM(SysExec.sy) \
		/u/lib/modula/SYM(SysExit.sy) /u/lib/modula/SYM(SysFork.sy) \
		/u/lib/modula/SYM(SysSignal.sy) \
		/u/lib/modula/SYM(SysWait.sy) \
		/u/lib/modula/SYM(SystemTypes.sy) \
		/u/lib/modula/SYM(UnixString.sy) CallShell.m2
		$(M2C) $(MFLAGS) -c -p CallShell.m2
		ar rv $(PLIB) CallShell.o && rm -f CallShell.o
SYM(Clock.sy):	Clock.d; $(M2C) $(MFLAGS) Clock.d
$(LIB)(Clock.o): SYM(Clock.sy) /u/lib/modula/SYM(Sys.sy) Clock.m2
		$(M2C) $(MFLAGS) -c Clock.m2
		ar rv $(LIB) Clock.o && rm -f Clock.o
$(PLIB)(Clock.o): SYM(Clock.sy) /u/lib/modula/SYM(Sys.sy) Clock.m2
		$(M2C) $(MFLAGS) -c -p Clock.m2
		ar rv $(PLIB) Clock.o && rm -f Clock.o
SYM(CoExpressio.sy): CoExpressio.d; $(M2C) $(MFLAGS) CoExpressio.d
$(LIB)(CoExpressio.o): SYM(CoExpressio.sy) SYM(Storage.sy) CoExpressio.m2
		$(M2C) $(MFLAGS) -c CoExpressio.m2
		ar rv $(LIB) CoExpressio.o && rm -f CoExpressio.o
$(PLIB)(CoExpressio.o): SYM(CoExpressio.sy) SYM(Storage.sy) CoExpressio.m2
		$(M2C) $(MFLAGS) -c -p CoExpressio.m2
		ar rv $(PLIB) CoExpressio.o && rm -f CoExpressio.o
SYM(Conversions.sy): Conversions.d; $(M2C) $(MFLAGS) Conversions.d
$(LIB)(Conversions.o): SYM(Conversions.sy) Conversions.m2
		$(M2C) $(MFLAGS) -c Conversions.m2
		ar rv $(LIB) Conversions.o && rm -f Conversions.o
$(PLIB)(Conversions.o): SYM(Conversions.sy) Conversions.m2
		$(M2C) $(MFLAGS) -c -p Conversions.m2
		ar rv $(PLIB) Conversions.o && rm -f Conversions.o
SYM(Environment.sy): Environment.d; $(M2C) $(MFLAGS) Environment.d
$(LIB)(Environment.o): SYM(Environment.sy) /u/lib/modula/SYM(SysLocation.sy) \
		Environment.m2
		$(M2C) $(MFLAGS) -c Environment.m2
		ar rv $(LIB) Environment.o && rm -f Environment.o
$(PLIB)(Environment.o): SYM(Environment.sy) \
		/u/lib/modula/SYM(SysLocation.sy) Environment.m2
		$(M2C) $(MFLAGS) -c -p Environment.m2
		ar rv $(PLIB) Environment.o && rm -f Environment.o
SYM(EtcGroup.sy): EtcGroup.d; $(M2C) $(MFLAGS) EtcGroup.d
$(LIB)(EtcGroup.o): SYM(EtcGroup.sy) SYM(Strings.sy) SYM(StdIO.sy) \
		SYM(ScanPwfile.sy) EtcGroup.m2
		$(M2C) $(MFLAGS) -c EtcGroup.m2
		ar rv $(LIB) EtcGroup.o && rm -f EtcGroup.o
$(PLIB)(EtcGroup.o): SYM(EtcGroup.sy) SYM(Strings.sy) SYM(StdIO.sy) \
		SYM(ScanPwfile.sy) EtcGroup.m2
		$(M2C) $(MFLAGS) -c -p EtcGroup.m2
		ar rv $(PLIB) EtcGroup.o && rm -f EtcGroup.o
SYM(Files.sy):	Files.d /u/lib/modula/SYM(SystemTypes.sy) SYM(StdIO.sy); \
		$(M2C) $(MFLAGS) Files.d
$(LIB)(Files.o): /u/lib/modula/SYM(SysLink.sy) \
		/u/lib/modula/SYM(SysUnlink.sy) \
		/u/lib/modula/SYM(SystemTypes.sy) SYM(StdIO.sy) \
		SYM(Files.sy) Files.m2
		$(M2C) $(MFLAGS) -c Files.m2
		ar rv $(LIB) Files.o && rm -f Files.o
$(PLIB)(Files.o): /u/lib/modula/SYM(SysLink.sy) \
		/u/lib/modula/SYM(SysUnlink.sy) \
		/u/lib/modula/SYM(SystemTypes.sy) SYM(StdIO.sy) \
		SYM(Files.sy) Files.m2
		$(M2C) $(MFLAGS) -c -p Files.m2
		ar rv $(PLIB) Files.o && rm -f Files.o
SYM(FtdIO.sy):	FtdIO.d SYM(StdIO.sy); $(M2C) $(MFLAGS) FtdIO.d
$(LIB)(FtdIO.o): SYM(Conversions.sy) SYM(ReadIntCard.sy) SYM(StdIO.sy) \
		SYM(FtdIO.sy) FtdIO.m2
		$(M2C) $(MFLAGS) -c FtdIO.m2
		ar rv $(LIB) FtdIO.o && rm -f FtdIO.o
$(PLIB)(FtdIO.o): SYM(Conversions.sy) SYM(ReadIntCard.sy) SYM(StdIO.sy) \
		SYM(FtdIO.sy) FtdIO.m2
		$(M2C) $(MFLAGS) -c -p FtdIO.m2
		ar rv $(PLIB) FtdIO.o && rm -f FtdIO.o
SYM(Functions.sy): Functions.d; $(M2C) $(MFLAGS) Functions.d
$(LIB)(Functions.o): SYM(ASCII.sy) SYM(Functions.sy) SYM(Storage.sy) \
		SYM(Strings.sy) Functions.m2
		$(M2C) $(MFLAGS) -c Functions.m2
		ar rv $(LIB) Functions.o && rm -f Functions.o
$(PLIB)(Functions.o): SYM(ASCII.sy) SYM(Functions.sy) SYM(Storage.sy) \
		SYM(Strings.sy) Functions.m2
		$(M2C) $(MFLAGS) -c -p Functions.m2
		ar rv $(PLIB) Functions.o && rm -f Functions.o
SYM(GetPass.sy): GetPass.d; $(M2C) $(MFLAGS) GetPass.d
$(LIB)(GetPass.o): SYM(ASCII.sy) SYM(GetPass.sy) SYM(RandomGener.sy) \
		/u/lib/modula/SYM(SysOpen.sy) \
		/u/lib/modula/SYM(SysTermIO.sy) SYM(StdIO.sy) SYM(FtdIO.sy) \
		GetPass.m2
		$(M2C) $(MFLAGS) -c GetPass.m2
		ar rv $(LIB) GetPass.o && rm -f GetPass.o
$(PLIB)(GetPass.o): SYM(ASCII.sy) SYM(GetPass.sy) SYM(RandomGener.sy) \
		/u/lib/modula/SYM(SysOpen.sy) \
		/u/lib/modula/SYM(SysTermIO.sy) SYM(StdIO.sy) SYM(FtdIO.sy) \
		GetPass.m2
		$(M2C) $(MFLAGS) -c -p GetPass.m2
		ar rv $(PLIB) GetPass.o && rm -f GetPass.o
SYM(InOut.sy):	InOut.d; $(M2C) $(MFLAGS) InOut.d
$(LIB)(InOut.o): SYM(Conversions.sy) SYM(InOut.sy) SYM(ReadIntCard.sy) \
		SYM(Terminal.sy) InOut.m2
		$(M2C) $(MFLAGS) -c InOut.m2
		ar rv $(LIB) InOut.o && rm -f InOut.o
$(PLIB)(InOut.o): SYM(Conversions.sy) SYM(InOut.sy) SYM(ReadIntCard.sy) \
		SYM(Terminal.sy) InOut.m2
		$(M2C) $(MFLAGS) -c -p InOut.m2
		ar rv $(PLIB) InOut.o && rm -f InOut.o
SYM(MathLib.sy): MathLib.d; $(M2C) $(MFLAGS) MathLib.d
$(LIB)(MathLib.o): SYM(MathLib.sy) MathLib.m2
		$(M2C) $(MFLAGS) -c MathLib.m2
		ar rv $(LIB) MathLib.o && rm -f MathLib.o
$(PLIB)(MathLib.o): SYM(MathLib.sy) MathLib.m2
		$(M2C) $(MFLAGS) -c -p MathLib.m2
		ar rv $(PLIB) MathLib.o && rm -f MathLib.o
SYM(Passwd.sy):	Passwd.d; $(M2C) $(MFLAGS) Passwd.d
$(LIB)(Passwd.o): SYM(Passwd.sy) SYM(Strings.sy) SYM(StdIO.sy) \
		SYM(ScanPwfile.sy) Passwd.m2
		$(M2C) $(MFLAGS) -c Passwd.m2
		ar rv $(LIB) Passwd.o && rm -f Passwd.o
$(PLIB)(Passwd.o): SYM(Passwd.sy) SYM(Strings.sy) SYM(StdIO.sy) \
		SYM(ScanPwfile.sy) Passwd.m2
		$(M2C) $(MFLAGS) -c -p Passwd.m2
		ar rv $(PLIB) Passwd.o && rm -f Passwd.o
SYM(PipeIO.sy):	PipeIO.d SYM(StdIO.sy); $(M2C) $(MFLAGS) PipeIO.d
$(LIB)(PipeIO.o): /u/lib/modula/SYM(SysClose.sy) \
		/u/lib/modula/SYM(SysDup.sy) /u/lib/modula/SYM(SysExec.sy) \
		/u/lib/modula/SYM(SysExit.sy) /u/lib/modula/SYM(SysFork.sy) \
		/u/lib/modula/SYM(SysPipe.sy) /u/lib/modula/SYM(SysWait.sy) \
		/u/lib/modula/SYM(UnixString.sy) SYM(StdIO.sy) \
		SYM(PipeIO.sy) PipeIO.m2
		$(M2C) $(MFLAGS) -c PipeIO.m2
		ar rv $(LIB) PipeIO.o && rm -f PipeIO.o
$(PLIB)(PipeIO.o): /u/lib/modula/SYM(SysClose.sy) \
		/u/lib/modula/SYM(SysDup.sy) /u/lib/modula/SYM(SysExec.sy) \
		/u/lib/modula/SYM(SysExit.sy) /u/lib/modula/SYM(SysFork.sy) \
		/u/lib/modula/SYM(SysPipe.sy) /u/lib/modula/SYM(SysWait.sy) \
		/u/lib/modula/SYM(UnixString.sy) SYM(StdIO.sy) \
		SYM(PipeIO.sy) PipeIO.m2
		$(M2C) $(MFLAGS) -c -p PipeIO.m2
		ar rv $(PLIB) PipeIO.o && rm -f PipeIO.o
SYM(Plot.sy):	Plot.d SYM(StdIO.sy); $(M2C) $(MFLAGS) Plot.d
$(LIB)(Plot.o):	SYM(SysPerror.sy) SYM(StdIO.sy) SYM(FtdIO.sy) SYM(Plot.sy) \
		Plot.m2
		$(M2C) $(MFLAGS) -c Plot.m2
		ar rv $(LIB) Plot.o && rm -f Plot.o
$(PLIB)(Plot.o): SYM(SysPerror.sy) SYM(StdIO.sy) SYM(FtdIO.sy) SYM(Plot.sy) \
		Plot.m2
		$(M2C) $(MFLAGS) -c -p Plot.m2
		ar rv $(PLIB) Plot.o && rm -f Plot.o
SYM(PrioTab.sy): PrioTab.d /u/lib/modula/SYM(SystemTypes.sy); \
		$(M2C) $(MFLAGS) PrioTab.d
$(LIB)(PrioTab.o): /u/lib/modula/SYM(SystemTypes.sy) SYM(PrioTab.sy) \
		PrioTab.m2
		$(M2C) $(MFLAGS) -c PrioTab.m2
		ar rv $(LIB) PrioTab.o && rm -f PrioTab.o
$(PLIB)(PrioTab.o): /u/lib/modula/SYM(SystemTypes.sy) SYM(PrioTab.sy) \
		PrioTab.m2
		$(M2C) $(MFLAGS) -c -p PrioTab.m2
		ar rv $(PLIB) PrioTab.o && rm -f PrioTab.o
SYM(Processes.sy): Processes.d; $(M2C) $(MFLAGS) Processes.d
$(LIB)(Processes.o): SYM(Processes.sy) SYM(Storage.sy) \
		/u/lib/modula/SYM(SysPanic.sy) Processes.m2
		$(M2C) $(MFLAGS) -c Processes.m2
		ar rv $(LIB) Processes.o && rm -f Processes.o
$(PLIB)(Processes.o): SYM(Processes.sy) SYM(Storage.sy) \
		/u/lib/modula/SYM(SysPanic.sy) Processes.m2
		$(M2C) $(MFLAGS) -c -p Processes.m2
		ar rv $(PLIB) Processes.o && rm -f Processes.o
SYM(RandomGener.sy): RandomGener.d; $(M2C) $(MFLAGS) RandomGener.d
$(LIB)(RandomGener.o): SYM(RandomGener.sy) /u/lib/modula/SYM(Sys.sy) \
		/u/lib/modula/SYM(SysTime.sy) \
		/u/lib/modula/SYM(SystemTypes.sy) RandomGener.m2
		$(M2C) $(MFLAGS) -c RandomGener.m2
		ar rv $(LIB) RandomGener.o && rm -f RandomGener.o
$(PLIB)(RandomGener.o): SYM(RandomGener.sy) /u/lib/modula/SYM(Sys.sy) \
		/u/lib/modula/SYM(SysTime.sy) \
		/u/lib/modula/SYM(SystemTypes.sy) RandomGener.m2
		$(M2C) $(MFLAGS) -c -p RandomGener.m2
		ar rv $(PLIB) RandomGener.o && rm -f RandomGener.o
SYM(ReadIntCard.sy): ReadIntCard.d; $(M2C) $(MFLAGS) ReadIntCard.d
$(LIB)(ReadIntCard.o): SYM(ReadIntCard.sy) ReadIntCard.m2
		$(M2C) $(MFLAGS) -c ReadIntCard.m2
		ar rv $(LIB) ReadIntCard.o && rm -f ReadIntCard.o
$(PLIB)(ReadIntCard.o): SYM(ReadIntCard.sy) ReadIntCard.m2
		$(M2C) $(MFLAGS) -c -p ReadIntCard.m2
		ar rv $(PLIB) ReadIntCard.o && rm -f ReadIntCard.o
SYM(RealConv.sy): RealConv.d; $(M2C) $(MFLAGS) RealConv.d
$(LIB)(RealConv.o): SYM(RealConv.sy) RealConv.m2
		$(M2C) $(MFLAGS) -c RealConv.m2
		ar rv $(LIB) RealConv.o && rm -f RealConv.o
$(PLIB)(RealConv.o): SYM(RealConv.sy) RealConv.m2
		$(M2C) $(MFLAGS) -c -p RealConv.m2
		ar rv $(PLIB) RealConv.o && rm -f RealConv.o
SYM(RealInOut.sy): RealInOut.d SYM(StdIO.sy); $(M2C) $(MFLAGS) RealInOut.d
$(LIB)(RealInOut.o): SYM(RealConv.sy) SYM(Strings.sy) SYM(Terminal.sy) \
		SYM(StdIO.sy) SYM(FtdIO.sy) SYM(RealInOut.sy) RealInOut.m2
		$(M2C) $(MFLAGS) -c RealInOut.m2
		ar rv $(LIB) RealInOut.o && rm -f RealInOut.o
$(PLIB)(RealInOut.o): SYM(RealConv.sy) SYM(Strings.sy) SYM(Terminal.sy) \
		SYM(StdIO.sy) SYM(FtdIO.sy) SYM(RealInOut.sy) RealInOut.m2
		$(M2C) $(MFLAGS) -c -p RealInOut.m2
		ar rv $(PLIB) RealInOut.o && rm -f RealInOut.o
SYM(ScanPwfile.sy): ScanPwfile.d SYM(ASCII.sy) SYM(StdIO.sy); \
		$(M2C) $(MFLAGS) ScanPwfile.d
$(LIB)(ScanPwfile.o): SYM(StdIO.sy) SYM(ScanPwfile.sy) ScanPwfile.m2
		$(M2C) $(MFLAGS) -c ScanPwfile.m2
		ar rv $(LIB) ScanPwfile.o && rm -f ScanPwfile.o
$(PLIB)(ScanPwfile.o): SYM(StdIO.sy) SYM(ScanPwfile.sy) ScanPwfile.m2
		$(M2C) $(MFLAGS) -c -p ScanPwfile.m2
		ar rv $(PLIB) ScanPwfile.o && rm -f ScanPwfile.o
SYM(StdFuncs.sy): StdFuncs.d; $(M2C) $(MFLAGS) StdFuncs.d
$(LIB)(StdFuncs.o): SYM(Functions.sy) SYM(MathLib.sy) SYM(StdFuncs.sy) \
		StdFuncs.m2
		$(M2C) $(MFLAGS) -c StdFuncs.m2
		ar rv $(LIB) StdFuncs.o && rm -f StdFuncs.o
$(PLIB)(StdFuncs.o): SYM(Functions.sy) SYM(MathLib.sy) SYM(StdFuncs.sy) \
		StdFuncs.m2
		$(M2C) $(MFLAGS) -c -p StdFuncs.m2
		ar rv $(PLIB) StdFuncs.o && rm -f StdFuncs.o
SYM(StdIO.sy):	StdIO.d /u/lib/modula/SYM(SystemTypes.sy); $(M2C) $(MFLAGS) \
		StdIO.d
$(LIB)(StdIO.o): /u/lib/modula/SYM(Errno.sy) SYM(Storage.sy) \
		/u/lib/modula/SYM(SysClose.sy) \
		/u/lib/modula/SYM(SysCreat.sy) /u/lib/modula/SYM(SysExit.sy) \
		/u/lib/modula/SYM(SysIoctl.sy) \
		/u/lib/modula/SYM(SysLseek.sy) /u/lib/modula/SYM(SysOpen.sy) \
		/u/lib/modula/SYM(SysRead.sy) /u/lib/modula/SYM(SysWrite.sy) \
		/u/lib/modula/SYM(SystemTypes.sy) SYM(StdIO.sy) StdIO.m2
		$(M2C) $(MFLAGS) -c StdIO.m2
		ar rv $(LIB) StdIO.o && rm -f StdIO.o
$(PLIB)(StdIO.o): /u/lib/modula/SYM(Errno.sy) SYM(Storage.sy) \
		/u/lib/modula/SYM(SysClose.sy) \
		/u/lib/modula/SYM(SysCreat.sy) /u/lib/modula/SYM(SysExit.sy) \
		/u/lib/modula/SYM(SysIoctl.sy) \
		/u/lib/modula/SYM(SysLseek.sy) /u/lib/modula/SYM(SysOpen.sy) \
		/u/lib/modula/SYM(SysRead.sy) /u/lib/modula/SYM(SysWrite.sy) \
		/u/lib/modula/SYM(SystemTypes.sy) SYM(StdIO.sy) StdIO.m2
		$(M2C) $(MFLAGS) -c -p StdIO.m2
		ar rv $(PLIB) StdIO.o && rm -f StdIO.o
SYM(Storage.sy): Storage.d; $(M2C) $(MFLAGS) Storage.d
$(LIB)(Storage.o): SYM(Storage.sy) /u/lib/modula/SYM(SysBreak.sy) \
		/u/lib/modula/SYM(SysPanic.sy) Storage.m2
		$(M2C) $(MFLAGS) -c Storage.m2
		ar rv $(LIB) Storage.o && rm -f Storage.o
$(PLIB)(Storage.o): SYM(Storage.sy) /u/lib/modula/SYM(SysBreak.sy) \
		/u/lib/modula/SYM(SysPanic.sy) Storage.m2
		$(M2C) $(MFLAGS) -c -p Storage.m2
		ar rv $(PLIB) Storage.o && rm -f Storage.o
SYM(StrSpec.sy): StrSpec.d; $(M2C) $(MFLAGS) StrSpec.d
$(LIB)(StrSpec.o): SYM(ASCII.sy) SYM(Storage.sy) SYM(StrSpec.sy) \
		SYM(Strings.sy) StrSpec.m2
		$(M2C) $(MFLAGS) -c StrSpec.m2
		ar rv $(LIB) StrSpec.o && rm -f StrSpec.o
$(PLIB)(StrSpec.o): SYM(ASCII.sy) SYM(Storage.sy) SYM(StrSpec.sy) \
		SYM(Strings.sy) StrSpec.m2
		$(M2C) $(MFLAGS) -c -p StrSpec.m2
		ar rv $(PLIB) StrSpec.o && rm -f StrSpec.o
SYM(StrToNum.sy): StrToNum.d; $(M2C) $(MFLAGS) StrToNum.d
$(LIB)(StrToNum.o): SYM(ASCII.sy) SYM(StrToNum.sy) StrToNum.m2
		$(M2C) $(MFLAGS) -c StrToNum.m2
		ar rv $(LIB) StrToNum.o && rm -f StrToNum.o
$(PLIB)(StrToNum.o): SYM(ASCII.sy) SYM(StrToNum.sy) StrToNum.m2
		$(M2C) $(MFLAGS) -c -p StrToNum.m2
		ar rv $(PLIB) StrToNum.o && rm -f StrToNum.o
SYM(Strings.sy): Strings.d; $(M2C) $(MFLAGS) Strings.d
$(LIB)(Strings.o): SYM(Strings.sy) Strings.m2
		$(M2C) $(MFLAGS) -c Strings.m2
		ar rv $(LIB) Strings.o && rm -f Strings.o
$(PLIB)(Strings.o): SYM(Strings.sy) Strings.m2
		$(M2C) $(MFLAGS) -c -p Strings.m2
		ar rv $(PLIB) Strings.o && rm -f Strings.o
SYM(SysPerror.sy): SysPerror.d; $(M2C) $(MFLAGS) SysPerror.d
$(LIB)(SysPerror.o): /u/lib/modula/SYM(Errno.sy) SYM(SysPerror.sy) \
		SYM(StdIO.sy) SYM(FtdIO.sy) SysPerror.m2
		$(M2C) $(MFLAGS) -c SysPerror.m2
		ar rv $(LIB) SysPerror.o && rm -f SysPerror.o
$(PLIB)(SysPerror.o): /u/lib/modula/SYM(Errno.sy) SYM(SysPerror.sy) \
		SYM(StdIO.sy) SYM(FtdIO.sy) SysPerror.m2
		$(M2C) $(MFLAGS) -c -p SysPerror.m2
		ar rv $(PLIB) SysPerror.o && rm -f SysPerror.o
SYM(SysSema.sy): SysSema.d; $(M2C) $(MFLAGS) SysSema.d
$(LIB)(SysSema.o): /u/lib/modula/SYM(Errno.sy) SYM(Storage.sy) \
		/u/lib/modula/SYM(Sys.sy) SYM(SysSema.sy) SysSema.m2
		$(M2C) $(MFLAGS) -c SysSema.m2
		ar rv $(LIB) SysSema.o && rm -f SysSema.o
$(PLIB)(SysSema.o): /u/lib/modula/SYM(Errno.sy) SYM(Storage.sy) \
		/u/lib/modula/SYM(Sys.sy) SYM(SysSema.sy) SysSema.m2
		$(M2C) $(MFLAGS) -c -p SysSema.m2
		ar rv $(PLIB) SysSema.o && rm -f SysSema.o
SYM(Terminal.sy): Terminal.d; $(M2C) $(MFLAGS) Terminal.d
$(LIB)(Terminal.o): SYM(Terminal.sy) SYM(StdIO.sy) Terminal.m2
		$(M2C) $(MFLAGS) -c Terminal.m2
		ar rv $(LIB) Terminal.o && rm -f Terminal.o
$(PLIB)(Terminal.o): SYM(Terminal.sy) SYM(StdIO.sy) Terminal.m2
		$(M2C) $(MFLAGS) -c -p Terminal.m2
		ar rv $(PLIB) Terminal.o && rm -f Terminal.o
SYM(TimeIO.sy):	TimeIO.d SYM(Calendar.sy) SYM(StdIO.sy); $(M2C) $(MFLAGS) \
		TimeIO.d
$(LIB)(TimeIO.o): SYM(ASCII.sy) SYM(Conversions.sy) SYM(Environment.sy) \
		SYM(Storage.sy) SYM(Strings.sy) SYM(Calendar.sy) \
		SYM(StdIO.sy) SYM(TimeIO.sy) SYM(FtdIO.sy) TimeIO.m2
		$(M2C) $(MFLAGS) -c TimeIO.m2
		ar rv $(LIB) TimeIO.o && rm -f TimeIO.o
$(PLIB)(TimeIO.o): SYM(ASCII.sy) SYM(Conversions.sy) SYM(Environment.sy) \
		SYM(Storage.sy) SYM(Strings.sy) SYM(Calendar.sy) \
		SYM(StdIO.sy) SYM(TimeIO.sy) SYM(FtdIO.sy) TimeIO.m2
		$(M2C) $(MFLAGS) -c -p TimeIO.m2
		ar rv $(PLIB) TimeIO.o && rm -f TimeIO.o
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
