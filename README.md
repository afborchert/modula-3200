# Historic Modula-2 compiler for the minicomputers of the Perkin-Elmer 3200 series running XELOS

## Background

Modula-2 and the origins of our family of Modula-2 compilers have been
designed and developed at the Department of Computer Science, ETH
Zürich in Switzerland (see http://www.inf.ethz.ch) by Niklaus Wirth
and his team.

In December 1981, we licensed the sources of the M2M compiler (4-pass
compiler for the famous Lilith architecture) and derived new compilers
from it for the Concurrent 3200 architecture, the m68k processor and
the SPARCv8 architecture. All these compilers conform to PIM3 (Niklaus
Wirth, Programming in Modula-2, 3rd Edition, Springer-Verlag) but not to
the ISO/IEC standard 10514-1:1996.

## Architecture

This implementation was firstly developed on a Perkin-Elmer 3220 (named
ENDLICH) running UNIX Edition VI and later VII. Later, the sources
were ported to a Perkin-Elmer 3280 running XELOS (the Unix variant of
Perkin-Elmer whose computer division had at this time been renamed to
Concurrent).

The architecture of the 3200 series is a successor of the Interdata 8/32
which in turn has similarities to the 370 architecture of IBM.

The Computer History Simulation Project (see https://github.com/simh/simh)
provides support for the Interdata 8/32. Unfortunately, this compiler
does not run on that simulator as it uses instructions which were
added later to the 3200 series.

## License

We have an agreement with the ETH Zürich that the sources which
have been derived from the M2M-compiler may be freely redistributed
provided that

> all derived sources clearly state that Modula-2 has been designed
> and developed at the Department of Computer Science, ETH Zurich in
> Switzerland.

All sources of the compiler of this distribution may be freely
redistributed if you follow the above term for the ETH-derived sources
*and* the terms of the GNU General Public License, Version 2 (as found
in the file COPYING).

The sources in the directories m2c, m2e, mdb, mmm, and mprof
may be freely redistributed under the terms of the GNU General Public
License, Version 2 (as found in the file COPYING).

The sources of the library (subdirectories lib and rts) may be freely
redistributed under the terms of the GNU Library General Public
License, Version 2 (as found in the file COPYING.LIB).

## See also

More about the historic bootstrapping process that led to this
compiler can be found at the following repositories:
 * https://github.com/afborchert/lilith
 * https://github.com/afborchert/lilith-multipass-modula2-compiler
