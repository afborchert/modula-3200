# XELOS makefile
LIBS=	-lPW
OBJS=	mdb_access.o mdb_aout.o mdb_brkpt.o mdb_core.o mdb_corewin.o \
	mdb_lcmd.o mdb_data.o mdb_fast.o mdb_ln.o mdb_main.o mdb_map.o \
	mdb_modules.o mdb_opset.o mdb_path.o mdb_process.o \
	mdb_ref.o mdb_stack.o mdb_text.o mdb_tree.o mdb_dtof.o mdb_error.o \
	mdb_instr.o
D_AOUT=	mdb_aout.o mdb_main.o
CFLAGS= -DCORE_BUG -DPTRACEX_BUG -DLINE

mdb:	${OBJS}
	cc -o mdb ${CFLAGS} ${OBJS} ${LIBS}

debug:	; make 'CFLAGS=-DDEBUG_AOUT' d_aout

d_aout:	${D_AOUT}
	cc -o d_aout ${CFLAGS} ${D_AOUT}

mdb_access.o:	mdb_ptrace.h

mdb_aout.o:	mdb_bp.h mdb_map.h mdb_ref.h mdb_tree.h

mdb_brkpt.o:	mdb_bp.h mdb_ref.h mdb_tree.h

mdb_core.o:	mdb_map.h

mdb_corewin.o:	mdb_bp.h mdb_ref.h mdb_tree.h mdb_view.h

mdb_data.o:	mdb_bp.h mdb_ref.h mdb_tree.h mdb_view.h

mdb_fast.o:	mdb_ref.h mdb_tree.h mdb_view.h

mdb_cmd.o:	mdb_bp.h mdb_ref.h mdb_tree.h mdb_view.h

mdb_error.o:	mdb_error.h

mdb_ref.o:	mdb_ref.h

mdb_map.o:	mdb_map.h

mdb_modules.o:	mdb_bp.h mdb_ref.h mdb_tree.h mdb_view.h

mdb_process.o:	mdb_bp.h mdb_ptrace.h mdb_ref.h mdb_tree.h

mdb_stack.o:	mdb_bp.h mdb_ref.h mdb_tree.h mdb_view.h

mdb_text.o:	mdb_bp.h mdb_ref.h mdb_tree.h mdb_view.h

mdb_tree.o:	mdb_bp.h mdb_ref.h mdb_tree.h

clean:
		rm -f ${OBJS}
