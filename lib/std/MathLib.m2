IMPLEMENTATION MODULE MathLib;

   CONST
      Huge = 7.237005577332262e+75; (* = MAX(REAL) *)
      LogHuge = 76;
      Zero = 0.0;

   (* local routines *)

   PROCEDURE IntTrunc(x: REAL) : INTEGER;
   BEGIN
      RETURN INTEGER(TRUNC(x));
   END IntTrunc;

   (*
    *	floor and ceil -- greatest integer <= arg
    *   (resp least >=)
    *)

   PROCEDURE Floor(d: REAL) : INTEGER;
      VAR i: INTEGER;
   BEGIN
      i := IntTrunc(d);
      IF (d < Zero) AND (FLOAT(i) <> d) THEN
         DEC(i);
      END;
      RETURN i;
   END Floor;

   PROCEDURE Ceil(d: REAL) : INTEGER;
   BEGIN
      RETURN - Floor(-d);
   END Ceil;

   MODULE Kludges;

      IMPORT Huge, Zero;
      EXPORT frexp, ldexp, modf;

      CONST
         RealWordLen = 8; (* TSIZE(REAL) *)
      TYPE
         Kludge =
            RECORD
               CASE : CARDINAL OF
                 0: d: REAL;
               | 1: byte: ARRAY[0..RealWordLen-1] OF CHAR;
               | 2: word: ARRAY[0..RealWordLen DIV 4 - 1] OF INTEGER;
               END;
            END;

      (*
       * frexp(x, exp)
       *
       * Return a fractional value with		1/16 <= |value| < 1
       * and store exponent in exp with		x = value * 2^exp
       *)

      PROCEDURE frexp(y: REAL; VAR exp: INTEGER) : REAL;
         VAR
            arg: INTEGER;
            x: Kludge;
      BEGIN
         x.d := y;
         IF x.d = Zero THEN		(* very easy if arg is zero *)
            exp := 0;
            RETURN Zero;
         END;
         exp := ORD(x.byte[0]);		(* get the exponent *)
         arg := exp;
         IF exp > 127 THEN		(* remove the sign *)
            exp := exp-128;
         END;
         exp := exp - 64;		(* remove the bias *)
         arg := arg - exp;		(* set argument to 0x40 or 0xc0 *)
         x.byte[0] := CHR(arg);
         exp := exp * 4;
         RETURN x.d;			(* return altered result *)
      END frexp;

      PROCEDURE ldexp(x: REAL; exp: INTEGER) : REAL;
         VAR
            pow2, fract: INTEGER;
            mul: Kludge;
            i: INTEGER;
            f: Kludge;
      BEGIN
         f.d := x;
         IF f.d = Zero THEN
            RETURN Zero;
         END;
         pow2 := INTEGER( ((ORD(f.byte[0]) MOD 128)-64) * 4 ) + exp;
         fract := ORD(f.byte[1]);
         REPEAT
            DEC(pow2);
            fract := fract * 2;
         UNTIL (fract MOD 512) > 255;

         IF pow2 >= 252 THEN
            IF f.d < Zero THEN
               RETURN -Huge;
            ELSE
               RETURN Huge;
            END;
         END;
         IF pow2 <= -260 THEN
            RETURN Zero;
         END;
         mul.d := Zero;
         mul.byte[0] := CHR((exp DIV 4) + 65);
         mul.byte[1] := CHR(16);
         FOR i := 1 TO exp MOD 4 DO
            mul.byte[1] := CHR(ORD(mul.byte[1]) * 2);
         END;

         RETURN f.d * mul.d;
      END ldexp;

      PROCEDURE modf(f: REAL; VAR aint: REAL) : REAL;
         VAR
            fint: Kludge;
            i, bkeep, blose: INTEGER;
      BEGIN
         fint.d := f;
         bkeep := ((ORD(fint.byte[0]) MOD 128)-64) * 4;
         IF bkeep < 0 THEN
            aint := Zero;
            RETURN Zero;
         END;
         blose := 56 - bkeep;
         IF blose < 32 THEN
            (* fint.word[1] &= (~0) << blose; *)
            FOR i := 1 TO blose DO
               fint.word[1] := fint.word[1] DIV 2;
            END;
            FOR i := 1 TO blose DO
               fint.word[1] := fint.word[1] * 2;
            END;
         ELSE
            (* fint.word[0] &= (~0) << (blose-32); *)
            fint.word[1] := 0;
            FOR i := 1 TO blose-32 DO
               fint.word[0] := fint.word[0] DIV 2;
            END;
            FOR i := 1 TO blose-32 DO
               fint.word[0] := fint.word[0] * 2;
            END;
         END;
         aint := fint.d;
         RETURN f - fint.d;
      END modf;

   BEGIN
   END Kludges;

   (* public routines *)

   (*
    *	floating-point arctangent
    *
    *	arctan returns the value of the arctangent of its
    *	argument in the range [-pi/2,pi/2].
    *
    *	there are no error returns.
    *
    *	coefficients are #5077 from Hart & Cheney. (19.56D)
    *)

   PROCEDURE arctan(arg: REAL) : REAL;
      CONST
         sq2p1	= 2.414213562373095048802e0;
         sq2m1	= 0.414213562373095048802e0;
         pio2	= 1.570796326794896619231e0;
         pio4	= 0.785398163397448309615e0;
         p4	= 0.161536412982230228262e2;
         p3	= 0.26842548195503973794141e3;
         p2	= 0.11530293515404850115428136e4;
         p1	= 0.178040631643319697105464587e4;
         p0	= 0.89678597403663861959987488e3;
         q4	= 0.5895697050844462222791e2;
         q3	= 0.536265374031215315104235e3;
         q2	= 0.16667838148816337184521798e4;
         q1	= 0.207933497444540981287275926e4;
         q0	= 0.89678597403663861962481162e3;
   
      PROCEDURE satan(arg: REAL) : REAL;

         PROCEDURE xatan(arg: REAL) : REAL;
            VAR
               argsq, value: REAL;
   	 BEGIN
   	    argsq := arg * arg;
   	    value := ( ( ( ( p4 * argsq + p3 ) * argsq + p2 ) * argsq + p1 ) 
   		       * argsq + p0 );
   	    value := value / ( ( ( ( ( argsq + q4 ) * argsq + q3 ) * argsq + q2)
   			         * argsq + q1 ) * argsq + q0 );
            RETURN value * arg;
   	 END xatan;
   
      BEGIN (* satan *)
   	 IF arg < sq2m1 THEN
            RETURN xatan(arg);
         ELSIF arg > sq2p1 THEN
   	    RETURN pio2 - xatan(1.0 / arg);
   	 ELSE
   	    RETURN pio4 + xatan(( arg - 1.0 ) / ( arg + 1.0 ));
         END;
      END satan;
   
   BEGIN (* arctan *)
      IF arg > Zero THEN
         RETURN satan(arg);
      ELSE
         RETURN -satan(-arg);
      END;
   END arctan;

   (*
   	exp returns the exponential function of its
   	floating-point argument.
   
   	The coefficients are #1069 from Hart and Cheney. (22.35D)
   *)
   
   PROCEDURE exp(arg: REAL) : REAL;
      CONST
         p0	= 0.2080384346694663001443843411e7;
         p1	= 0.3028697169744036299076048876e5;
         p2	= 0.6061485330061080841615584556e2;
         q0	= 0.6002720360238832528230907598e7;
         q1	= 0.3277251518082914423057964422e6;
         q2	= 0.1749287689093076403844945335e4;
         log2e	= 1.4426950408889634073599247;
         sqrt2	= 1.4142135623730950488016887;
         maxf	= 10000.0;
      VAR
         fract: REAL;
         temp1, temp2, xsq: REAL;
         ent: INTEGER;
         minus: BOOLEAN;
   BEGIN
      IF arg = Zero THEN
         RETURN 1.0;
      ELSIF arg < -maxf THEN
         RETURN Zero;
      ELSIF arg > maxf THEN 
         RETURN Huge;
      ELSE
         arg := arg * log2e;
         minus := arg < Zero;
         IF minus THEN
            arg := - arg;
         END;
         ent := Floor(arg);
         fract := (arg-FLOAT(ent)) - 0.5;
         xsq := fract*fract;
         temp1 := ((p2*xsq+p1)*xsq+p0)*fract;
         temp2 := ((xsq+q2)*xsq+q1)*xsq + q0;
         IF minus THEN
            RETURN 1.0 / ldexp(sqrt2*(temp2+temp1)/(temp2-temp1), ent);
         ELSE
            RETURN ldexp(sqrt2*(temp2+temp1)/(temp2-temp1), ent);
         END;
      END;
   END exp;

   (*
    *	log returns the natural logarithm of its floating
    *	point argument.
    *
    *	The coefficients are #2705 from Hart & Cheney. (19.38D)
    *)

   PROCEDURE ln(arg: REAL) : REAL;
      CONST
         log2	=  0.693147180559945309e0;
         sqrto2	=  0.707106781186547524e0;
         p0	= -0.240139179559210510e2;
         p1	=  0.309572928215376501e2;
         p2	= -0.963769093368686593e1;
         p3	=  0.421087371217979714e0;
         q0	= -0.120069589779605255e2;
         q1	=  0.194809660700889731e2;
         q2	= -0.891110902798312337e1;
      VAR
         x, z, zsq, temp: REAL;
         exp: INTEGER;
   BEGIN
      IF arg <= Zero THEN
         RETURN - Huge;
      ELSE
         x := frexp(arg, exp);
         WHILE x < 0.5 DO
            x := x * 2.0;
            exp := exp - 1;
         END;
         IF x < sqrto2 THEN
            x := 2.0 * x;
            exp := exp - 1;
         END;
         z := ( x - 1.0 ) / ( x + 1.0 );
         zsq := z * z;
         temp := ( ( p3 * zsq + p2 ) * zsq + p1 ) * zsq + p0;
         temp := temp / ( ( ( 1.0 * zsq + q2 ) * zsq + q1 ) * zsq + q0 );
         RETURN temp * z + FLOAT(exp) * log2;
      END;
   END ln;

   MODULE Sinus;

      IMPORT Zero, modf, IntTrunc;
      EXPORT cos, sin;

   (*
    *	Coefficients are #3370 from Hart & Cheney (18.80D).
    *)

      PROCEDURE sinus(arg: REAL; quad: INTEGER) : REAL;
         CONST
            twoopi    =  0.63661977236758134308;
            p0        =  0.1357884097877375669092680e8;
            p1        = -0.4942908100902844161158627e7;
            p2        =  0.4401030535375266501944918e6;
            p3        = -0.1384727249982452873054457e5;
            p4        =  0.1459688406665768722226959e3;
            q0        =  0.8644558652922534429915149e7;
            q1        =  0.4081792252343299749395779e6;
            q2        =  0.9463096101538208180571257e4;
            q3        =  0.1326534908786136358911494e3;
         VAR
            e, f, ysq, x, y, temp1, temp2: REAL;
            dummy : REAL;
            k: INTEGER;
      BEGIN
         x := arg;
         IF x < Zero THEN
            x := -x;
            quad := quad + 2;
         END;
         x := x * twoopi;
         IF x > 32764.0 THEN
            y := modf(x, e);
            e := e + FLOAT(quad);
            dummy := modf(0.25 * e, f);
            quad := IntTrunc(e - 4.0 * f);
         ELSE
            k := IntTrunc(x);
            y := x - FLOAT(k);
            quad := (quad + k) MOD 4;
         END;
         IF ODD(quad) THEN
            y := 1.0 - y;
         END;
         IF quad > 1 THEN
            y := - y;
         END;
         ysq := y * y;
         temp1 := ( ( ( ( p4 * ysq + p3 ) * ysq + p2 ) * ysq + p1 ) 
                    * ysq + p0 ) * y;
         temp2 := ( ( ( ysq + q3 ) * ysq + q2 ) * ysq + q1 ) * ysq + q0;
         RETURN temp1 / temp2;
      END sinus;

      PROCEDURE cos(arg: REAL) : REAL;
      BEGIN
         IF arg < Zero THEN
            arg := - arg;
         END;
         RETURN sinus(arg, 1);
      END cos;

      PROCEDURE sin(arg: REAL) : REAL;
      BEGIN
         RETURN sinus(arg, 0);
      END sin;

   BEGIN
   END Sinus;

   (*
    *	sqrt with Newton's method
    *)

   PROCEDURE sqrt(arg: REAL) : REAL;
      CONST
         two30 = 1073741824;	(* 1<<30 *)
      VAR
         x, temp: REAL;
         exp: INTEGER;
         i: INTEGER;
   BEGIN
      IF arg <= Zero THEN
         RETURN Zero;
      END;
      x := frexp(arg, exp);
      WHILE x < 0.5 DO
         x := x * 2.0;
         exp := exp - 1;
      END;
      (*
       * NOTE
       * this wont work on 1's comp
       *)
      IF ODD(exp) THEN
         x := x * 2.0;
         exp := exp - 1;
      END;
      temp := 0.5*(1.0+x);

      WHILE exp > 60 DO
         temp := temp * FLOAT(two30);
         exp := exp - 60;
      END;
      WHILE exp < -60 DO
         temp := temp / FLOAT(two30);
         exp := exp + 60;
      END;
      IF exp >= 0 THEN
         FOR i:= 1 TO exp DIV 2 DO   (* temp *= 1L << (exp/2); *)
            temp := temp * 2.0;
         END;
      ELSE
         FOR i:= 1 TO -exp DIV 2 DO   (* temp /= 1L << (-exp/2); *)
            temp := temp / 2.0;
         END;
      END;
      FOR i := 0 TO 4 DO
         temp := 0.5*(temp + arg/temp);
      END;
      RETURN temp;
   END sqrt;

END MathLib.
