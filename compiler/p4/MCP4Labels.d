DEFINITION MODULE MCP4Labels;

 (*
 EXPORT QUALIFIED
    LabelLength, Label, LabelPtr, LabelType, GetLabel, PushLabel,
    PopLabel, TopLabel;
 *)

 CONST
    LabelLength = 8;

 TYPE
    Label = ARRAY [ 0..LabelLength-1 ] OF CHAR;
    LabelPtr = POINTER TO Label;
    LabelType = (blockl, casel, forl, ifl, loopl, repeatl, stringl, whilel);

 PROCEDURE GetLabel(lt: LabelType; VAR l: LabelPtr);

 PROCEDURE PushLabel(lt: LabelType; l: LabelPtr);

 PROCEDURE PopLabel(lt: LabelType) : LabelPtr;

 PROCEDURE TopLabel(lt: LabelType) : LabelPtr;

END MCP4Labels.
