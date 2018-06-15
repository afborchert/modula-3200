IMPLEMENTATION MODULE MCP4Stack; (* AFB 1/85 *)

   FROM MCBase IMPORT BitsPerWord, Stptr, adc;
   FROM MCP4AttributSys IMPORT Attribut, AtMode;
   FROM MCP4Global IMPORT Assert, Error;

   CONST
      MaxStackUse = 8192; (* fullwords *)
   TYPE
      UseField = ARRAY [0 .. MaxStackUse DIV BitsPerWord - 1] OF BITSET;
   VAR
      StackUse: UseField;
      Used: [0..MaxStackUse];
      MaxTop: CARDINAL;
      CurrentTop: CARDINAL;
      StartOffset: CARDINAL; (* relative to base *)
      overflow: BOOLEAN; (* of StackUse *)

   (* size always in fullwords; offset always relative to base *)

   PROCEDURE GetStack(size: CARDINAL; VAR offset: CARDINAL);
      VAR index, index2: CARDINAL; found: BOOLEAN;

      PROCEDURE Alloc(index, size: CARDINAL);
         VAR i: CARDINAL;
      BEGIN
         FOR i := index TO index+size-1 DO
            Allocate(i);
         END;
      END Alloc;

   BEGIN
      IF overflow THEN offset := StartOffset; RETURN END;
      IF Used >= size THEN
         FOR index := 0 TO Used-size DO
            found := TRUE;
            FOR index2 := index TO index+size-1 DO
               found := found AND NOT IsUsed(index2);
            END;
            IF found THEN
               offset := StartOffset + index * adc;
               Alloc(index, size);
               RETURN
            END;
         END;
      END;
      WHILE (Used > 0) AND NOT IsUsed(Used-1) DO
         DEC(Used);
      END;
      IF Used+size > MaxStackUse THEN
         Error(213);
         offset := StartOffset;
         overflow := TRUE;
      END;
      Alloc(Used, size);
      offset := StartOffset + Used * adc;
      INC(Used, size);
   END GetStack;

   PROCEDURE GetStackAt(VAR at: Attribut); (* at^.typtr^.size is used *)
   BEGIN
      WITH at DO
         mode := stackMod;
         Assert(typtr <> NIL);
         size := (typtr^.size + adc - 1) DIV adc;
         GetStack(size, offset);
      END;
   END GetStackAt;

   PROCEDURE FreeStack(size: CARDINAL; offset: CARDINAL);
      VAR index: CARDINAL;
   BEGIN
      IF overflow THEN RETURN END;
      index := (offset - StartOffset) DIV adc;
      FOR index := index TO index+size-1 DO
         Assert(IsUsed(index));
         Free(index);
      END;
   END FreeStack;

   PROCEDURE FreeStackAt(VAR at: Attribut); (* at.mode = stackMod ! *)
   BEGIN
      WITH at DO
         Assert(mode = stackMod);
         FreeStack(size, offset);
         mode := illegalMod;
      END;
   END FreeStackAt;

   (* following two routines generates no code; *)
   (* they serves for stack use calculation     *)

   PROCEDURE IncTop(incr: CARDINAL);
   BEGIN
      INC(CurrentTop, incr);
      IF CurrentTop > MaxTop THEN
         MaxTop := CurrentTop;
      END;
   END IncTop;

   PROCEDURE DecTop(dec: CARDINAL);
   BEGIN
      DEC(CurrentTop, dec);
   END DecTop;

   PROCEDURE SetStartOffset(offset: CARDINAL);
   (* called by MCP4Block at the begin of a procedure *)
   BEGIN
      StartOffset := offset;
      Used := 0;
      MaxTop := 0;
      CurrentTop := 0;
      overflow := FALSE;
   END SetStartOffset;

   (* following routines are called at the end of a procedure *)

   PROCEDURE GetStackUse(VAR size: CARDINAL);
   BEGIN
      size := Used;
   END GetStackUse;
   (* return result from GetStack/FreeStack *)

   PROCEDURE GetMaxIncTop(VAR incr: CARDINAL);
   BEGIN
      incr := MaxTop;
   END GetMaxIncTop;
   (* return result from IncTop and DecTop *)

   (* local routines *)

   PROCEDURE Allocate(i: CARDINAL);
   BEGIN
      INCL(StackUse[i DIV BitsPerWord], i MOD BitsPerWord);
   END Allocate;

   PROCEDURE Free(i: CARDINAL);
   BEGIN
      EXCL(StackUse[i DIV BitsPerWord], i MOD BitsPerWord);
   END Free;

   PROCEDURE IsUsed(i: CARDINAL) : BOOLEAN;
   BEGIN
      RETURN (i MOD BitsPerWord) IN StackUse[i DIV BitsPerWord];
   END IsUsed;

END MCP4Stack.
