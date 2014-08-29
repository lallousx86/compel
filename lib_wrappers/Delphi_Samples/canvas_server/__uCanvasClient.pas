unit uCanvasClient;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uCanvasExtShapes, StdCtrls;

type

  PCanvasExt = ^TCanvasExt;
  TCanvasExt = class(TObject)
  private
    FInit: TCanvasExtInit;
    FEvent: THandle;
    FThread: THandle;
    FForm: TfrmCanvasExt;

    FBrushColor: TColor;
    FPenColor: TColor;
    FBrushStyle: TBrushStyle;
    FFont: TFont;
  private

    function SendToCanvas(Msg: UINT; AWParam: WPARAM = 0; ALParam: LPARAM = 0): LResult;
    function PostToCanvas(Msg: UINT; AWParam: WPARAM = 0; ALParam: LPARAM = 0): LongBool;
    procedure InsertShape(AShape: TCanvasExtShape;const ANow: Boolean = False);

    procedure SetShapeParameters(AShape: TCanvasExtShape);
    procedure SetFont(AFont: TFont);
    function GetHeight: Integer;
    function GetWidth: Integer;
  public
    procedure BeginPaint(const ABegin: Boolean = True);
    procedure Close(const ANow: Boolean = False);
    constructor Create(Init: TCanvasExtInit);
    procedure Wait;
    destructor Destroy;override;
//    function Rectangle(const AX1, AY1, AX2, AY2: Integer;const AFilled: Boolean = False): Integer;
//    function Ellipse(const AX1, AY1, AX2, AY2: Integer): Integer;
//    function Circle(const AX, AY, ARadius: Integer): Integer;
//    function Square(const AX, AY, ASide: Integer): Integer;
//    function Line(const AX1, AY1, AX2, AY2: Integer): Integer;
//    function DrawText(const AX, AY: Integer;const AMsg: string): Integer;
    function ClearShapes: Integer;
    function ClearCanvas(AColor: TColor): Integer;
    function DeleteShape(Index: Integer): Integer;
    function ShapesCount: Integer;
//    function SetTitle(const ATitle: string): Integer;
//    function SetSize(const AWidth, AHeight: Integer): Integer;
//    function MoveCanvas(const ALeft, ATop: Integer): Integer;
//    function DrawBitmap(const AX1, AY1, AX2, AY2: Integer;const AFileName: string): Integer;
    function CanvasOnTop(const AOnTop: Boolean = True): Integer;
//    function SaveCanvasToBitmap(const AFileName: string): Integer;
    function AllowClose(const AAllow: Boolean = True): Integer;
    function ShowHide(const AShow: Boolean = True): Integer;
  published
    property BrushColor: TColor read FBrushColor write FBrushColor;
    property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle;
    property PenColor: TColor read FPenColor write FPenColor;
    property Font: TFont write SetFont;
//    property Height: Integer read GetHeight;
//    property Width: Integer read GetWidth;
  end;


implementation

{ TCanvasExt }

function TCanvasExt.ClearCanvas(AColor: TColor): Integer;
begin
  ClearShapes;
  BrushColor := AColor;
  Result := Rectangle(0, 0, FForm.Width, FForm.Height, True);
end;

{
function TCanvasExt.ClearShapes: Integer;
begin
  Result := SendToCanvas(WM_CANVAS_CLEAR);
end;
}
procedure TCanvasExt.Close(const ANow: Boolean);
begin
  FForm.FbAllowClose := True;
  PostToCanvas(WM_CLOSE);
  if ANow then
    Wait;
end;

constructor TCanvasExt.Create(Init: TCanvasExtInit);
var
  tid: Cardinal;
begin
  FInit := Init;

  // Create thread
  FEvent := Windows.CreateEvent(nil, False, False, nil);

  // Create the thread
  FThread := Windows.CreateThread(nil, 0, @uCanvasExt.CanvasThreadProc, Self, 0, tid);

  // Wait for thread initialization
  Windows.WaitForSingleObject(FEvent, INFINITE);

  // Dispose the waiting event
  Windows.CloseHandle(FEvent);

  //
  FFont := nil;
end;

function TCanvasExt.DeleteShape(Index: Integer): Integer;
begin
  Result := SendToCanvas(WM_CANVAS_CTL, CANVAS_CTL_DELITEM, Index);
end;

destructor TCanvasExt.Destroy;
begin
  // Signal close NOW to canvas window
  Close(True);

  // Free the initialization structure
  FInit.Free;

  // Clear the font
  if Assigned(FFont) then
    FFont.Free;
  inherited;
end;
{
function TCanvasExt.DrawBitmap(const AX1, AY1, AX2, AY2: Integer;
  const AFileName: string): Integer;
var
  s: TCanvasExt2DImageLoader;
begin
  s := TCanvasExt2DImageLoader.Create(AX1, AY1, AX2, AY2, AFileName);
  SetShapeParameters(s);
  InsertShape(s);
  Result := 1;
end;
}
{
function TCanvasExt.DrawText(const AX, AY: Integer;
  const AMsg: string): Integer;
var
  s: TCanvasExt2DText;
begin
  s := TCanvasExt2DText.Create(AX, AY, AMsg);
  SetShapeParameters(s);
  if Assigned(FFont) then
    s.Font := FFont;
  InsertShape(s);
  Result := 1;
end;
}
procedure TCanvasExt.InsertShape(AShape: TCanvasExtShape;const ANow: Boolean);
begin
  if ANow then
    SendToCanvas(WM_CANVAS_INSERTSHAPE, 0, LPARAM(AShape))
  else
    PostToCanvas(WM_CANVAS_INSERTSHAPE, 0, LPARAM(AShape));
end;

{
function TCanvasExt.Ellipse(const AX1, AY1, AX2, AY2: Integer): Integer;
var
  s: TCanvasExtShape;
begin
  s := TCanvasExtShape4Points.Create(AX1, AY1, AX2, AY2, fptEllipse);
  SetShapeParameters(s);
  InsertShape(s);
  Result := 1;
end;
}

function TCanvasExt.GetHeight: Integer;
begin
  if not Assigned(FForm) then
  begin
    Result := 0;
    Exit;
  end;
  Result := FForm.Height;
end;

function TCanvasExt.GetWidth: Integer;
begin
  if not Assigned(FForm) then
  begin
    Result := 0;
    Exit;
  end;
  Result := FForm.Width;
end;

function TCanvasExt.AllowClose(const AAllow: Boolean): Integer;
begin
  Result := Integer(PostToCanvas(WM_CANVAS_CTL, CANVAS_CTL_ALLOWCLOSE, Integer(AAllow)));
end;

procedure TCanvasExt.BeginPaint(const ABegin: Boolean);
begin
  FForm.PaintBeginning := ABegin;
end;

function TCanvasExt.CanvasOnTop(const AOnTop: Boolean): Integer;
begin
  Result := SendToCanvas(WM_CANVAS_CTL, CANVAS_CTL_KEEPONTOP, Integer(AOnTop));
end;
{
function TCanvasExt.Circle(const AX, AY, ARadius: Integer): Integer;
var
  s: TCanvasExtShape;
begin
  s := TCanvasExtShape4Points.Create(
       AX-ARadius, AY-ARadius,
       AX+ARadius, AY+ARadius, fptEllipse);
  SetShapeParameters(s);
  InsertShape(s);
  Result := 1;
end;
}
{
function TCanvasExt.Line(const AX1, AY1, AX2, AY2: Integer): Integer;
var
  s: TCanvasExtShape;
begin
  s := TCanvasExtShape4Points.Create(AX1, AY1, AX2, AY2, fptLine);
  SetShapeParameters(s);
  InsertShape(s);
  Result := 1;
end;
}

function TCanvasExt.MoveCanvas(const ALeft, ATop: Integer): Integer;
begin
  Result := SendToCanvas(WM_CANVAS_CTL, CANVAS_CTL_SETPOS,
    Windows.MakeLParam(Word(ALeft), Word(ATop)));
end;

function TCanvasExt.PostToCanvas(Msg: UINT; AWParam: WPARAM;
  ALParam: LPARAM): LongBool;
begin
  Result := Windows.PostMessage(FForm.Handle, Msg, AWParam, ALParam);
end;

{
function TCanvasExt.Rectangle(const AX1, AY1, AX2, AY2: Integer;const AFilled: Boolean = False): Integer;
var
  s: TCanvasExtShape4Points;
begin
  s := TCanvasExtShape4Points.Create(AX1, AY1, AX2, AY2);
  s.Filled:= AFilled;
  SetShapeParameters(s);
  InsertShape(s);
  Result := 1;
end;
}

{
function TCanvasExt.Square(const AX, AY, ASide: Integer): Integer;
var
  s: TCanvasExtShape;
begin
  s := TCanvasExtShape4Points.Create(AX, AY, AX+ASide, AY+ASide);
  SetShapeParameters(s);
  InsertShape(s);
  Result := 1;
end;
}
{
function TCanvasExt.SaveCanvasToBitmap(const AFileName: string): Integer;
begin
  Result := SendToCanvas(WM_CANVAS_CTL, CANVAS_CTL_SAVEBMP, LParam(AFileName));
end;
}
function TCanvasExt.SendToCanvas(Msg: UINT; AWParam: WPARAM;
  ALParam: LPARAM): LResult;
begin
  Result := Windows.SendMessage(FForm.Handle, Msg, AWParam, ALParam);
end;

procedure TCanvasExt.SetFont(AFont: TFont);
begin
  if not Assigned(FFont) then
    FFont := TFont.Create;
  FFont.Name := AFont.Name;
  FFont.Size := AFont.Size;
  FFont.Color := AFont.Color;
  FFont.Style := AFont.Style
end;

procedure TCanvasExt.SetShapeParameters(AShape: TCanvasExtShape);
begin
  AShape.BrushColor := BrushColor;
  AShape.PenColor := PenColor;
  AShape.BrushStyle := BrushStyle;
end;

function TCanvasExt.SetSize(const AWidth, AHeight: Integer): Integer;
begin
  Result := Integer(PostToCanvas(WM_CANVAS_CTL, CANVAS_CTL_SETSIZE,
   Windows.MakeLParam(Word(AWidth), Word(AHeight))));
end;

function TCanvasExt.SetTitle(const ATitle: string): Integer;
begin
  Result := SendToCanvas(WM_CANVAS_CTL, CANVAS_CTL_SETTITLE, LParam(ATitle));
end;

function TCanvasExt.ShapesCount: Integer;
begin
  Result := SendToCanvas(WM_CANVAS_CTL, CANVAS_CTL_GETCOUNT);
end;

function TCanvasExt.ShowHide(const AShow: Boolean): Integer;
begin
  Result := Integer(PostToCanvas(WM_CANVAS_CTL, CANVAS_CTL_SHOWHIDE, LParam(AShow)));
end;

procedure TCanvasExt.Wait;
begin
  if FThread = 0 then
    Exit;

  // Wait for thread to GUI terminate
  Windows.WaitForSingleObject(FThread, INFINITE);

  Windows.CloseHandle(FThread);
  FThread := 0;
end;
