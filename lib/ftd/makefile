# {updated by mmm: Fri Apr 28 17:41:36 1989 MEZ}
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
SRC =		Bytes.d Chars.d Chars.m2 E.d E.m2 EditWindow.d EditWindow.m2 \
		F.d F.m2 Keywords.d Keywords.m2 L.d L.m2 LongStrings.d \
		LongStrings.m2 OutLines.d OutLines.m2 P.d P.m2 Printf.d \
		Printf.m2 S.d S.m2 Stack.d Stack.m2 W.d W.m2
TAGSRC =	Chars.m2 E.m2 EditWindow.m2 F.m2 Keywords.m2 L.m2 \
		LongStrings.m2 OutLines.m2 P.m2 Printf.m2 S.m2 Stack.m2 W.m2
OBJS =		
SYMS =		
REFS =		Chars.r E.r EditWindow.r F.r Keywords.r L.r LongStrings.r \
		OutLines.r P.r Printf.r S.r Stack.r W.r
# {all}
LIB =		libm2.a
PLIB =		plibm2.a
all:		SYM(Bytes.sy) $(LIB) $(PLIB)
# {linkage}
LIBS =		
LDFLAGS =	
# {dependencies}
$(LIB):		$(LIB)(Chars.o) $(LIB)(E.o) $(LIB)(EditWindow.o) $(LIB)(F.o) \
		$(LIB)(Keywords.o) $(LIB)(L.o) $(LIB)(LongStrings.o) \
		$(LIB)(OutLines.o) $(LIB)(P.o) $(LIB)(Printf.o) $(LIB)(S.o) \
		$(LIB)(Stack.o) $(LIB)(W.o)
$(PLIB):	$(PLIB)(Chars.o) $(PLIB)(E.o) $(PLIB)(EditWindow.o) \
		$(PLIB)(F.o) $(PLIB)(Keywords.o) $(PLIB)(L.o) \
		$(PLIB)(LongStrings.o) $(PLIB)(OutLines.o) $(PLIB)(P.o) \
		$(PLIB)(Printf.o) $(PLIB)(S.o) $(PLIB)(Stack.o) $(PLIB)(W.o)
SYM(Bytes.sy):	Bytes.d; $(M2C) $(MFLAGS) Bytes.d
SYM(Chars.sy):	Chars.d; $(M2C) $(MFLAGS) Chars.d
$(LIB)(Chars.o): SYM(Chars.sy) Chars.m2
		$(M2C) $(MFLAGS) -c Chars.m2
		ar rv $(LIB) Chars.o && rm -f Chars.o
$(PLIB)(Chars.o): SYM(Chars.sy) Chars.m2
		$(M2C) $(MFLAGS) -c -p Chars.m2
		ar rv $(PLIB) Chars.o && rm -f Chars.o
SYM(E.sy):	E.d /u/lib/modula/SYM(SystemTypes.sy) SYM(Printf.sy); \
		$(M2C) $(MFLAGS) E.d
$(LIB)(E.o):	SYM(Chars.sy) /u/lib/modula/SYM(Errno.sy) \
		/u/lib/modula/SYM(StdIO.sy) /u/lib/modula/SYM(Strings.sy) \
		/u/lib/modula/SYM(SysExit.sy) \
		/u/lib/modula/SYM(SysGetpid.sy) \
		/u/lib/modula/SYM(SysKill.sy) \
		/u/lib/modula/SYM(SysSignal.sy) \
		/u/lib/modula/SYM(SystemTypes.sy) SYM(LongStrings.sy) \
		SYM(Printf.sy) SYM(E.sy) E.m2
		$(M2C) $(MFLAGS) -c E.m2
		ar rv $(LIB) E.o && rm -f E.o
$(PLIB)(E.o):	SYM(Chars.sy) /u/lib/modula/SYM(Errno.sy) \
		/u/lib/modula/SYM(StdIO.sy) /u/lib/modula/SYM(Strings.sy) \
		/u/lib/modula/SYM(SysExit.sy) \
		/u/lib/modula/SYM(SysGetpid.sy) \
		/u/lib/modula/SYM(SysKill.sy) \
		/u/lib/modula/SYM(SysSignal.sy) \
		/u/lib/modula/SYM(SystemTypes.sy) SYM(LongStrings.sy) \
		SYM(Printf.sy) SYM(E.sy) E.m2
		$(M2C) $(MFLAGS) -c -p E.m2
		ar rv $(PLIB) E.o && rm -f E.o
SYM(EditWindow.sy): EditWindow.d /u/lib/modula/SYM(FunctionKey.sy) \
		/u/lib/modula/SYM(Windows.sy); $(M2C) $(MFLAGS) EditWindow.d
$(LIB)(EditWindow.o): SYM(Chars.sy) /u/lib/modula/SYM(FunctionKey.sy) \
		/u/lib/modula/SYM(StdIO.sy) /u/lib/modula/SYM(Windows.sy) \
		SYM(EditWindow.sy) SYM(E.sy) SYM(F.sy) EditWindow.m2
		$(M2C) $(MFLAGS) -c EditWindow.m2
		ar rv $(LIB) EditWindow.o && rm -f EditWindow.o
$(PLIB)(EditWindow.o): SYM(Chars.sy) /u/lib/modula/SYM(FunctionKey.sy) \
		/u/lib/modula/SYM(StdIO.sy) /u/lib/modula/SYM(Windows.sy) \
		SYM(EditWindow.sy) SYM(E.sy) SYM(F.sy) EditWindow.m2
		$(M2C) $(MFLAGS) -c -p EditWindow.m2
		ar rv $(PLIB) EditWindow.o && rm -f EditWindow.o
SYM(F.sy):	F.d /u/lib/modula/SYM(StdIO.sy) SYM(Printf.sy); \
		$(M2C) $(MFLAGS) F.d
$(LIB)(F.o):	/u/lib/modula/SYM(StdIO.sy) SYM(LongStrings.sy) \
		SYM(Printf.sy) SYM(E.sy) SYM(F.sy) F.m2
		$(M2C) $(MFLAGS) -c F.m2
		ar rv $(LIB) F.o && rm -f F.o
$(PLIB)(F.o):	/u/lib/modula/SYM(StdIO.sy) SYM(LongStrings.sy) \
		SYM(Printf.sy) SYM(E.sy) SYM(F.sy) F.m2
		$(M2C) $(MFLAGS) -c -p F.m2
		ar rv $(PLIB) F.o && rm -f F.o
SYM(Keywords.sy): Keywords.d; $(M2C) $(MFLAGS) Keywords.d
$(LIB)(Keywords.o): SYM(Keywords.sy) Keywords.m2
		$(M2C) $(MFLAGS) -c Keywords.m2
		ar rv $(LIB) Keywords.o && rm -f Keywords.o
$(PLIB)(Keywords.o): SYM(Keywords.sy) Keywords.m2
		$(M2C) $(MFLAGS) -c -p Keywords.m2
		ar rv $(PLIB) Keywords.o && rm -f Keywords.o
SYM(L.sy):	L.d SYM(LongStrings.sy) SYM(Printf.sy); $(M2C) $(MFLAGS) L.d
$(LIB)(L.o):	SYM(LongStrings.sy) SYM(Printf.sy) SYM(E.sy) SYM(L.sy) L.m2
		$(M2C) $(MFLAGS) -c L.m2
		ar rv $(LIB) L.o && rm -f L.o
$(PLIB)(L.o):	SYM(LongStrings.sy) SYM(Printf.sy) SYM(E.sy) SYM(L.sy) L.m2
		$(M2C) $(MFLAGS) -c -p L.m2
		ar rv $(PLIB) L.o && rm -f L.o
SYM(LongStrings.sy): LongStrings.d /u/lib/modula/SYM(StdIO.sy); \
		$(M2C) $(MFLAGS) LongStrings.d
$(LIB)(LongStrings.o): SYM(Bytes.sy) SYM(Chars.sy) SYM(Stack.sy) \
		/u/lib/modula/SYM(StdIO.sy) /u/lib/modula/SYM(Storage.sy) \
		/u/lib/modula/SYM(Strings.sy) SYM(LongStrings.sy) \
		LongStrings.m2
		$(M2C) $(MFLAGS) -c LongStrings.m2
		ar rv $(LIB) LongStrings.o && rm -f LongStrings.o
$(PLIB)(LongStrings.o): SYM(Bytes.sy) SYM(Chars.sy) SYM(Stack.sy) \
		/u/lib/modula/SYM(StdIO.sy) /u/lib/modula/SYM(Storage.sy) \
		/u/lib/modula/SYM(Strings.sy) SYM(LongStrings.sy) \
		LongStrings.m2
		$(M2C) $(MFLAGS) -c -p LongStrings.m2
		ar rv $(PLIB) LongStrings.o && rm -f LongStrings.o
SYM(OutLines.sy): OutLines.d /u/lib/modula/SYM(StdIO.sy) \
		SYM(LongStrings.sy); $(M2C) $(MFLAGS) OutLines.d
$(LIB)(OutLines.o): SYM(Chars.sy) /u/lib/modula/SYM(StdIO.sy) \
		SYM(LongStrings.sy) SYM(OutLines.sy) OutLines.m2
		$(M2C) $(MFLAGS) -c OutLines.m2
		ar rv $(LIB) OutLines.o && rm -f OutLines.o
$(PLIB)(OutLines.o): SYM(Chars.sy) /u/lib/modula/SYM(StdIO.sy) \
		SYM(LongStrings.sy) SYM(OutLines.sy) OutLines.m2
		$(M2C) $(MFLAGS) -c -p OutLines.m2
		ar rv $(PLIB) OutLines.o && rm -f OutLines.o
SYM(P.sy):	P.d SYM(Printf.sy); $(M2C) $(MFLAGS) P.d
$(LIB)(P.o):	/u/lib/modula/SYM(StdIO.sy) SYM(LongStrings.sy) \
		SYM(Printf.sy) SYM(E.sy) SYM(P.sy) P.m2
		$(M2C) $(MFLAGS) -c P.m2
		ar rv $(LIB) P.o && rm -f P.o
$(PLIB)(P.o):	/u/lib/modula/SYM(StdIO.sy) SYM(LongStrings.sy) \
		SYM(Printf.sy) SYM(E.sy) SYM(P.sy) P.m2
		$(M2C) $(MFLAGS) -c -p P.m2
		ar rv $(PLIB) P.o && rm -f P.o
SYM(Printf.sy):	Printf.d SYM(LongStrings.sy); $(M2C) $(MFLAGS) Printf.d
$(LIB)(Printf.o): SYM(Bytes.sy) SYM(Chars.sy) \
		/u/lib/modula/SYM(Conversions.sy) \
		/u/lib/modula/SYM(Errno.sy) /u/lib/modula/SYM(RealConv.sy) \
		/u/lib/modula/SYM(Strings.sy) \
		/u/lib/modula/SYM(SysPerror.sy) SYM(LongStrings.sy) \
		SYM(Printf.sy) Printf.m2
		$(M2C) $(MFLAGS) -c Printf.m2
		ar rv $(LIB) Printf.o && rm -f Printf.o
$(PLIB)(Printf.o): SYM(Bytes.sy) SYM(Chars.sy) \
		/u/lib/modula/SYM(Conversions.sy) \
		/u/lib/modula/SYM(Errno.sy) /u/lib/modula/SYM(RealConv.sy) \
		/u/lib/modula/SYM(Strings.sy) \
		/u/lib/modula/SYM(SysPerror.sy) SYM(LongStrings.sy) \
		SYM(Printf.sy) Printf.m2
		$(M2C) $(MFLAGS) -c -p Printf.m2
		ar rv $(PLIB) Printf.o && rm -f Printf.o
SYM(S.sy):	S.d SYM(Printf.sy); $(M2C) $(MFLAGS) S.d
$(LIB)(S.o):	SYM(Bytes.sy) SYM(LongStrings.sy) SYM(Printf.sy) SYM(S.sy) \
		SYM(E.sy) S.m2
		$(M2C) $(MFLAGS) -c S.m2
		ar rv $(LIB) S.o && rm -f S.o
$(PLIB)(S.o):	SYM(Bytes.sy) SYM(LongStrings.sy) SYM(Printf.sy) SYM(S.sy) \
		SYM(E.sy) S.m2
		$(M2C) $(MFLAGS) -c -p S.m2
		ar rv $(PLIB) S.o && rm -f S.o
SYM(Stack.sy):	Stack.d; $(M2C) $(MFLAGS) Stack.d
$(LIB)(Stack.o): SYM(Bytes.sy) SYM(Stack.sy) /u/lib/modula/SYM(Storage.sy) \
		Stack.m2
		$(M2C) $(MFLAGS) -c Stack.m2
		ar rv $(LIB) Stack.o && rm -f Stack.o
$(PLIB)(Stack.o): SYM(Bytes.sy) SYM(Stack.sy) /u/lib/modula/SYM(Storage.sy) \
		Stack.m2
		$(M2C) $(MFLAGS) -c -p Stack.m2
		ar rv $(PLIB) Stack.o && rm -f Stack.o
SYM(W.sy):	W.d /u/lib/modula/SYM(Windows.sy) SYM(Printf.sy); \
		$(M2C) $(MFLAGS) W.d
$(LIB)(W.o):	SYM(Bytes.sy) SYM(Chars.sy) /u/lib/modula/SYM(Windows.sy) \
		SYM(LongStrings.sy) SYM(Printf.sy) SYM(W.sy) SYM(E.sy) W.m2
		$(M2C) $(MFLAGS) -c W.m2
		ar rv $(LIB) W.o && rm -f W.o
$(PLIB)(W.o):	SYM(Bytes.sy) SYM(Chars.sy) /u/lib/modula/SYM(Windows.sy) \
		SYM(LongStrings.sy) SYM(Printf.sy) SYM(W.sy) SYM(E.sy) W.m2
		$(M2C) $(MFLAGS) -c -p W.m2
		ar rv $(PLIB) W.o && rm -f W.o
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
