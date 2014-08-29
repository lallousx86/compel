unit uCanvasExt;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CompelScript, compel_lib, uCanvasExtShapes;

type
  PCanvasExtInit = ^TCanvasExtInit;
  TCanvasExtInit = class
  public
    Width, Height: Integer;
    X, Y: Integer;
    BrushColor, PenColor: TColor;
    constructor Create;
  end;

  TfrmCanvasExt = class(TForm)
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FInit: TCanvasExtInit;
    FbActivateOnce: Boolean;

    FShapes: TCanvasExtShapeList;
    FOldFormStyle: TFormStyle;
    FbAllowClose: Boolean;
    FPaintBeginning: Boolean;

    procedure OnActivateOnce(Sender: TObject);
    procedure OnMsgCtl(var Msg: TMessage);message WM_CANVAS_CTL;
    procedure OnMsgInsertShape(var Msg: TMessage);message WM_CANVAS_INSERTSHAPE;
    procedure OnMsgClearShapes(var Msg: TMessage);message WM_CANVAS_CLEAR;
    function  DrawToBitmap(const AFileName: string): Boolean;
    procedure SetPaintBeginning(const Value: Boolean);
  public
    procedure SetInitStruct(AInit: TCanvasExtInit);
  published
    property PaintBeginning: Boolean read FPaintBeginning write SetPaintBeginning;
  end;

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
    function Rectangle(const AX1, AY1, AX2, AY2: Integer;const AFilled: Boolean = False): Integer;
    function Ellipse(const AX1, AY1, AX2, AY2: Integer): Integer;
    function Circle(const AX, AY, ARadius: Integer): Integer;
    function Square(const AX, AY, ASide: Integer): Integer;
    function Line(const AX1, AY1, AX2, AY2: Integer): Integer;
    function DrawText(const AX, AY: Integer;const AMsg: string): Integer;
    function ClearShapes: Integer;
    function ClearCanvas(AColor: TColor): Integer;
    function DeleteShape(Index: Integer): Integer;
    function ShapesCount: Integer;
    function SetTitle(const ATitle: string): Integer;
    function SetSize(const AWidth, AHeight: Integer): Integer;
    function MoveCanvas(const ALeft, ATop: Integer): Integer;
    function DrawBitmap(const AX1, AY1, AX2, AY2: Integer;const AFileName: string): Integer;
    function CanvasOnTop(const AOnTop: Boolean = True): Integer;
    function SaveCanvasToBitmap(const AFileName: string): Integer;
    function AllowClose(const AAllow: Boolean = True): Integer;
    function ShowHide(const AShow: Boolean = True): Integer;
  published
    property BrushColor: TColor read FBrushColor write FBrushColor;
    property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle;
    property PenColor: TColor read FPenColor write FPenColor;
    property Font: TFont write SetFont;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
  end;

  
implementation

{$R *.dfm}

// TMultiReadExclusiveWriteSynchronizer
// Finalize

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


function CanvasThreadProc(Me: TCanvasExt): Cardinal;stdcall;
var
  frm: TfrmCanvasExt;
begin
  // Create form
  frm := TfrmCanvasExt.Create(nil);

  // Pass the new form's pointer
  Me.FForm := frm;

  // Pass init structure to the form
  frm.SetInitStruct(Me.FInit);

  // Signal form creation completion
  Windows.SetEvent(Me.FEvent);

  // Run message loop
  Result := Cardinal(frm.ShowModal);
end;

{ TfrmCanvasExt }

function TfrmCanvasExt.DrawToBitmap(const AFileName: string): Boolean;
var
  i: Integer;
  c: TCanvas;
  b: TBitmap;

begin
  b := TBitmap.Create;
  b.Width := Self.Width;
  b.Height := Self.Height;
  
  // Get Canvas
  c := b.Canvas;
  
  // Draw all shapes
  for i := 0 to FShapes.Count - 1 do
    FShapes[i].Draw(c);

  try
    b.SaveToFile(AFileName);
    Result := True;
  except
    Result := False;
  end;
  b.Free;
end;

procedure TfrmCanvasExt.FormActivate(Sender: TObject);
begin
  if FbActivateOnce = False then
  begin
    FbActivateOnce := True;
    OnActivateOnce(Self);
  end;
end;

procedure TfrmCanvasExt.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FbAllowClose = False then
    Action := caNone;
end;

procedure TfrmCanvasExt.FormCreate(Sender: TObject);
begin
  FbActivateOnce := False;
  FShapes := TCanvasExtShapeList.Create;
  FOldFormStyle := Self.FormStyle;
  FbAllowClose := True;

  FPaintBeginning := False;
end;

procedure TfrmCanvasExt.FormDestroy(Sender: TObject);
begin
  FShapes.Free;
end;

procedure TfrmCanvasExt.FormPaint(Sender: TObject);
var
  i: Integer;
  c: TCanvas;
begin
  if FPaintBeginning then
  begin
    inherited;
    Exit;
  end;
  
  // Get Canvas
  c := Self.Canvas;
  // Draw all shapes
  for i := 0 to FShapes.Count - 1 do
    FShapes[i].Draw(c);
end;

procedure TfrmCanvasExt.OnActivateOnce(Sender: TObject);
begin
  if not Assigned(FInit) then
    Exit;
  Self.Width := FInit.Width;
  Self.Height := FInit.Height;
  if FInit.X <> -1 then
    Self.Left := FInit.X;
  if FInit.Y <> -1 then
    Self.Top := FInit.Y;
end;

procedure TfrmCanvasExt.OnMsgClearShapes(var Msg: TMessage);
begin
  Msg.Result := FShapes.Count;
  FShapes.Clear;
  Invalidate;
end;

procedure TfrmCanvasExt.OnMsgCtl(var Msg: TMessage);
begin
  Msg.Result := 0;
  case Msg.WParam of
    CANVAS_CTL_GETCOUNT:
    begin
      Msg.Result := FShapes.Count;
    end;
    CANVAS_CTL_DELITEM:
    begin
      FShapes.Delete(Msg.LParam);
      Msg.Result := FShapes.Count;
      Invalidate;
    end;
    CANVAS_CTL_SETTITLE:
    begin
      Self.Caption := string(Msg.LParam);
    end;
    CANVAS_CTL_SETSIZE:
    begin
      Self.Width := Msg.LParamLo;
      Self.Height := Msg.LParamHi;
    end;
    CANVAS_CTL_SETPOS:
    begin
      Self.Left := Msg.LParamLo;
      Self.Top  := Msg.LParamHi;
    end;
    CANVAS_CTL_KEEPONTOP:
    begin
      if Boolean(Msg.LParam) then
        SetWindowPos(Handle, HWND_TOPMOST, Left, Top, Width, Height, SWP_SHOWWINDOW)
      else
        SetWindowPos(Handle, HWND_NOTOPMOST, Left, Top, Width, Height, SWP_SHOWWINDOW);
    end;
    CANVAS_CTL_SHOWHIDE:
    begin
      if Boolean(Msg.LParam) then
        Self.Show
      else
        Self.Hide;
    end;
    CANVAS_CTL_SAVEBMP:
    begin
      Msg.Result := Integer(DrawToBitmap(string(Msg.LParam)));
    end;
    CANVAS_CTL_ALLOWCLOSE:
    begin
      FbAllowClose := Boolean(Msg.LParam);
    end;
  end;
end;

procedure TfrmCanvasExt.OnMsgInsertShape(var Msg: TMessage);
begin
  FShapes.Add(TCanvasExtShape(Msg.LParam));
  Invalidate;
end;

procedure TfrmCanvasExt.SetInitStruct(AInit: TCanvasExtInit);
begin
  FInit := AInit;
end;

procedure TfrmCanvasExt.SetPaintBeginning(const Value: Boolean);
begin
  // We were in PaintBegin, then now switching to paint end
  // then update screen!
  if (FPaintBeginning = True) and (Value = False) then
  begin
    Invalidate;
  end;
  FPaintBeginning := Value;
end;

{ TCanvasExtInit }

constructor TCanvasExtInit.Create;
begin
  X := -1;
  Y := -1;
  Width := 309;
  Height := 349;
  BrushColor := clBtnFace;
  PenColor := clWindowText;
end;

{ TCanvasExt }

function TCanvasExt.ClearCanvas(AColor: TColor): Integer;
begin
  ClearShapes;
  BrushColor := AColor;
  Result := Rectangle(0, 0, FForm.Width, FForm.Height, True);
end;

function TCanvasExt.ClearShapes: Integer;
begin
  Result := SendToCanvas(WM_CANVAS_CLEAR);
end;

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

procedure TCanvasExt.InsertShape(AShape: TCanvasExtShape;const ANow: Boolean);
begin
  if ANow then
    SendToCanvas(WM_CANVAS_INSERTSHAPE, 0, LPARAM(AShape))
  else
    PostToCanvas(WM_CANVAS_INSERTSHAPE, 0, LPARAM(AShape));
end;

function TCanvasExt.Ellipse(const AX1, AY1, AX2, AY2: Integer): Integer;
var
  s: TCanvasExtShape;
begin
  s := TCanvasExtShape4Points.Create(AX1, AY1, AX2, AY2, fptEllipse);
  SetShapeParameters(s);
  InsertShape(s);
  Result := 1;
end;

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

function TCanvasExt.Line(const AX1, AY1, AX2, AY2: Integer): Integer;
var
  s: TCanvasExtShape;
begin
  s := TCanvasExtShape4Points.Create(AX1, AY1, AX2, AY2, fptLine);
  SetShapeParameters(s);
  InsertShape(s);
  Result := 1;
end;

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

function TCanvasExt.Square(const AX, AY, ASide: Integer): Integer;
var
  s: TCanvasExtShape;
begin
  s := TCanvasExtShape4Points.Create(AX, AY, AX+ASide, AY+ASide);
  SetShapeParameters(s);
  InsertShape(s);
  Result := 1;
end;

function TCanvasExt.SaveCanvasToBitmap(const AFileName: string): Integer;
begin
  Result := SendToCanvas(WM_CANVAS_CTL, CANVAS_CTL_SAVEBMP, LParam(AFileName));
end;

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


end.
