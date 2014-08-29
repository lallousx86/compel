unit PaintTypes;

interface

uses
  Messages;

type
  TPaintCmdList =
  (cmdLine, cmdRect, cmdEllipse, cmdLoad, cmdSave, cmdSelectColor,
   cmdSetTitle, cmdClear);
  PPaintCmdRec = ^TPaintCmdRec;
  TPaintCmdRec = record
    Kind: TPaintCmdList;
    case TPaintCmdList of
      cmdLine, cmdRect, cmdEllipse: (X1, Y1, X2, Y2: Integer);
      cmdSetTitle, cmdLoad, cmdSave: (FileName: ShortString);
      cmdSelectColor: (Color: Cardinal);
      cmdClear: ();
  end;

const
  WM_PAINT_CMD = WM_APP+2;
     
implementation

end.
