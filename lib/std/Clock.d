DEFINITION MODULE Clock;

   CONST UnitsPerSecond = 100;

   PROCEDURE RealTime(reset: BOOLEAN): CARDINAL;
   PROCEDURE CPUTime (reset: BOOLEAN): CARDINAL;
   (*
    *	These functions return the time in units elapsed since the start
    *	of the current process or since the last call with argument TRUE.
    *)

END Clock.
