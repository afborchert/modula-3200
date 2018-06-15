IMPLEMENTATION MODULE MCP4Labels;	(* AFB 8/83 *)

   FROM Conversions IMPORT ConvertInteger;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM MCP4Public IMPORT Sflag;
  
   TYPE
      ListPtr = POINTER TO List;
      List =
         RECORD
            l: LabelPtr;
            lt: LabelType;
            next: ListPtr;
         END;
   VAR
      LabelList: ListPtr;
      unique: CARDINAL; (* unique number *)
  
   PROCEDURE GetLabel(lt: LabelType; VAR l: LabelPtr);
      VAR i: CARDINAL;
          field: ARRAY[0..LabelLength-3] OF CHAR;
   BEGIN
      NEW(l);
      ConvertInteger(unique, 1, field);
      INC(unique);
      IF Sflag THEN
         CASE lt OF
           blockl    : l^[0] := "B";
         | casel     : l^[0] := "C";
         | forl      : l^[0] := "F";
         | ifl       : l^[0] := "I";
         | loopl     : l^[0] := "L";
         | repeatl   : l^[0] := "R";
         | stringl   : l^[0] := "S";
         | whilel    : l^[0] := "W";
         END;
      ELSE
         l^[0] := "L"; (* see ld(1), option -X *)
      END;
      l^[1] := "."; (* avoid conflicts *)
      FOR i := 2 TO LabelLength-1 DO
         l^[i] := field[i-2];
      END;
   END GetLabel;
  
   PROCEDURE PushLabel(lt: LabelType; l: LabelPtr);
      VAR
         new: ListPtr;
   BEGIN
      NEW(new);
      new^.l := l;
      new^.lt := lt;
      new^.next := LabelList;
      LabelList := new;
   END PushLabel;
  
   PROCEDURE PopLabel (lt: LabelType) : LabelPtr;
      VAR result: LabelPtr;
          old: ListPtr;
   BEGIN
      result := LabelList^.l;
      old := LabelList;
      LabelList := LabelList^.next;
      DISPOSE(old);
      RETURN result;
   END PopLabel;

   PROCEDURE TopLabel (ltyp: LabelType) : LabelPtr;
      VAR ptr: ListPtr;
   BEGIN
      ptr := LabelList;
      LOOP
         IF ptr^.lt = ltyp THEN RETURN ptr^.l END;
         ptr := ptr^.next;
      END;
   END TopLabel;

BEGIN
   unique := 0;
   LabelList := NIL;
END MCP4Labels.
