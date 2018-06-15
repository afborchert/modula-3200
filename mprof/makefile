OBJS=	mp_aout.o mp_main.o mp_map.o mp_path.o mp_ref.o mp_refparse.o \
	mp_statistic.o mp_strsave.o
CFLAGS=	-DMPROF

mprof:	${OBJS}
	${CC} -o mprof ${OBJS}

mp_aout.o:	mp_map.h mp.h

mp_map.o:	mp_map.h

mp_ref.o:	mp_ref.h

mp_refparse.o:	mp_ref.h mp.h

mp_statistic.o:	mp.h

clean:
	rm -f ${OBJS}
