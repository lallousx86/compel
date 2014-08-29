unit CanvasExtTypes;

interface

uses Windows;

const
  CANVAS_CTL_GETCOUNT   = 1;
  CANVAS_CTL_DELITEM    = 2;
  CANVAS_CTL_SETTITLE   = 3;
  CANVAS_CTL_SETSIZE    = 4;
  CANVAS_CTL_SETPOS     = 5;
  CANVAS_CTL_KEEPONTOP  = 6;
  CANVAS_CTL_SHOWHIDE   = 7;
  CANVAS_CTL_SAVEBMP    = 8;
  CANVAS_CTL_ALLOWCLOSE = 9;
  CANVAS_CTL_DRAWTEXT   = 10;
  CANVAS_CTL_DRAW2DSHAPE= 11;
  CANVAS_CTL_DRAWBMP    = 12;
  CANVAS_CTL_CLEARCANVAS= 13;
  CANVAS_CTL_GETDIMENSIONS = 14;
  
  CANVAS_PIPE_NAME_FMT  = '\\.\pipe\CANVAS_PIPE_%x_%x';
  CANVAS_PIPE_MSG_SIZE  = 1024;

type
  TCanvasClientMsg_Handle = packed record
    Handle: Hwnd;
  end;

  PCanvasClientMsg_XY = ^TCanvasClientMsg_XY;
  TCanvasClientMsg_XY = packed record
    X, Y: Integer;
  end;

implementation

end.