IMPLEMENTATION MODULE MCP4Strings;

   FROM MCBase IMPORT Stringptr, stringroot;
   FROM MCP4CodeSys IMPORT EmitString;
   FROM MCP4Labels IMPORT LabelPtr;

   PROCEDURE PrintStrings;
      VAR ptr: Stringptr;
          slabel: LabelPtr;
   BEGIN
      ptr := stringroot;
      WHILE ptr <> NIL DO
         WITH ptr^ DO
            IF label <> 0 THEN
               slabel := LabelPtr(label);
               EmitString(slabel, valentry);
            END;
            ptr := slink;
         END;
      END;
   END PrintStrings;

END MCP4Strings.
