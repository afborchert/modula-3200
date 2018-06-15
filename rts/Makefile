#
#	Modula-2 runtime library -- XELOS version
#	(c) Andreas Borchert 1986, University of Ulm
#
ASFLAGS =	-Y -m
all:		REF(mcount.r) REF(Bytes.r) \
		cm2rt0.o libm2.a m2rt0.o mm2rt0.o plibm2.a
libm2.a:	libm2.a(SysLocation.o) libm2.a(abort.o) libm2.a(case.o) \
		libm2.a(check.o) libm2.a(fret.o) libm2.a(halt.o) \
		libm2.a(stack.o) libm2.a(transfer.o) libm2.a(newprocess.o) \
		libm2.a(crend.o) libm2.a(prio.o) libm2.a(signal.o) \
		libm2.a(mcount.o) libm2.a(Bytes.o)
plibm2.a:	plibm2.a(SysLocation.o) plibm2.a(abort.o) plibm2.a(case.o) \
		plibm2.a(check.o) plibm2.a(fret.o) plibm2.a(halt.o) \
		plibm2.a(stack.o) plibm2.a(transfer.o) plibm2.a(newprocess.o) \
		plibm2.a(crend.o) plibm2.a(prio.o) plibm2.a(signal.o) \
		plibm2.a(mcount.o) plibm2.a(Bytes.o)

.s.a:
		${AS} ${ASFLAGS} $<
		ar r $@ $*.o
		rm -f $*.o

abort.o:	sys.s equ.s
case.o:		equ.s
check.o:	equ.s
cm2rt0.o:	sys.s equ.s
crend.o:	equ.s
fret.o:		equ.s
halt.o:		equ.s
m2rt0.o:	sys.s equ.s
mcount.o:	equ.s
mm2rt0.o:	sys.s equ.s
newprocess.o:	equ.s process.s
prio.o:		equ.s
signal.o:	equ.s sys.s
stack.o:	equ.s
transfer.o:	equ.s process.s
REF(mcount.r):	mcount.m2
		m2c -c mcount.m2
		rm mcount.[or]
REF(Bytes.r):	Bytes.m2
		m2c -c Bytes.m2
		rm Bytes.[or]
