unit uCanvasClient;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Dialogs,
  Classes,
  Graphics,
  CanvasExtTypes,
  uCanvasExtUtil,
  uCanvasExtShapes;

type
  TCanvasExtClient = class
  private
    _pi: TProcessInformation;
    _server_proc_name: string;
    _h_wait_server: THandle;
    _window_handle: Hwnd;
    _b_server_teriminated: Boolean;
    _ms: TMemoryStream;

    FBrushColor: TColor;
    FPenColor: TColor;
    FBrushStyle: TBrushStyle;
    FFont: TFont;

    procedure SetShapeParameters(AShape: TCanvasExtShape);
    procedure SetFont(AFont: TFont);
    function GetHeight: Integer;
    function GetWidth: Integer;

    procedure InstallServerTerminateWaiter;
    function WmCopyDataBuffer(AWnd: Hwnd;const ACtlCode: Integer;Buf: Pointer;const ASize: Integer): Integer;
    function SendMemoryStream(const ACtlCode: Integer): Integer;
    procedure ExitKill;
  public
    constructor Create;
    destructor Destroy;override;
    function Launch: Boolean;
    function CanvasClose: Integer;
    function Wait(ATimeOut: Cardinal = INFINITE): Boolean;
    function GetDimensions(var AWidth, AHeight: Integer): Integer;
    function SetTitle(const ATitle: string): Integer;
    function SetSize(const AWidth, AHeight: Integer): Integer;
    function DrawText(const AX, AY: Integer;const AMsg: string): Integer;
    function ClearShapes: Integer;
    function ClearCanvas(AColor: TColor = clWhite): Integer;
    function Rectangle(const AX1, AY1, AX2, AY2: Integer): Integer;
    function Line(const AX1, AY1, AX2, AY2: Integer): Integer;
    function Circle(const AX, AY, ARadius: Integer): Integer;
    function Square(const AX, AY, ASide: Integer): Integer;
    function Ellipse(const AX1, AY1, AX2, AY2: Integer): Integer;
    function DrawBitmap(const AX1, AY1, AX2, AY2: Integer;const AFileName: string): Integer;
    function SaveCanvasToBitmap(const AFileName: string): Integer;
    function MoveCanvas(const ALeft, ATop: Integer): Integer;
    function CanvasOnTop(const AOnTop: Boolean = True): Integer;
    function AllowClose(const AAllow: Boolean = True): Integer;
    procedure SetServerPath(const AFileName: string);
  published
    property BrushColor: TColor read FBrushColor write FBrushColor;
    property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle;
    property PenColor: TColor read FPenColor write FPenColor;
    property Font: TFont write SetFont;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
  end;

implementation

{ TCanvasExtClient }

function TCanvasExtClient.GetHeight: Integer;
var
  w, h: Integer;
begin
  GetDimensions(w, h);
  Result := h;
end;

function TCanvasExtClient.GetWidth: Integer;
var
  w, h: Integer;
begin
  GetDimensions(w, h);
  Result := w;
end;

constructor TCanvasExtClient.Create;
begin
  _server_proc_name := 'canvas_server.exe';
  _window_handle := 0;
  _ms := TMemoryStream.Create;
  _h_wait_server := 0;

  BrushColor := clBtnFace;
  PenColor := clWindowText;

  FillChar(_pi, SizeOf(_pi), 0);
end;

function CanvasExtClient_ServerWaiter(Self: TCanvasExtClient): Dword;stdcall;
begin
  WaitForSingleObject(Self._pi.hProcess, INFINITE);
  CloseHandle(Self._pi.hProcess);
  CloseHandle(Self._pi.hThread);
  Self._pi.hProcess := 0;
  Self._pi.hThread := 0;
  Self._b_server_teriminated := True;
  Result := 0;
end;

procedure TCanvasExtClient.InstallServerTerminateWaiter;
var
  id: Cardinal;
begin
  _h_wait_server := CreateThread(nil, 0, @CanvasExtClient_ServerWaiter, Self, 0, id);
  if (_h_wait_server <> 0) then
  begin
    _b_server_teriminated := False;
  end
  else begin
    _b_server_teriminated := True;
  end;
end;

function TCanvasExtClient.Launch: Boolean;
  procedure KillProcess;
  begin
    TerminateProcess(_pi.hProcess, Cardinal(-1));
  end;

var
  si: TStartupInfo;
  hPipe: THandle;
  pipe_name: string;
  handle_msg: TCanvasClientMsg_Handle;
  read: Cardinal;
begin
  Result := False;
  FillChar(si, SizeOf(si), 0);
  si.cb := SizeOf(TStartupInfo);
  si.dwX := GetTickCount;
  si.dwY := GetCurrentThreadId;
{$IFDEF dbg}
  pipe_name := '\\.\pipe\canvastest';
{$ELSE}
  // Form the pipe's name
  pipe_name := Format(CANVAS_PIPE_NAME_FMT, [si.dwX, si.dwY]);
{$ENDIF}
  // Create the PIPE
  hPipe := CreateNamedPipe(
    PChar(pipe_name),             // pipe name
    PIPE_ACCESS_DUPLEX,       // read/write access
    PIPE_TYPE_MESSAGE or       // message type pipe
    PIPE_READMODE_MESSAGE or   // message-read mode
    PIPE_WAIT,                // blocking mode
    PIPE_UNLIMITED_INSTANCES, // max. instances
    CANVAS_PIPE_MSG_SIZE,    // output buffer size
    CANVAS_PIPE_MSG_SIZE,   // input buffer size
    NMPWAIT_USE_DEFAULT_WAIT, // client time-out
    nil);                    // default security attribute

  if hPipe = Windows.INVALID_HANDLE_VALUE then
    Exit;
{$IFDEF dbg}

{$ELSE}
  // Run the server
  if not CreateProcess(PChar(_server_proc_name), nil, nil, nil, FALSE, 0, nil, nil, si, _pi) then
    Exit;
{$ENDIF}
  // Wait for client to connect to us
  if not ConnectNamedPipe(hPipe, nil) then
  begin
    KillProcess;
    Exit;
  end;

  if not ReadFile(hPipe, handle_msg, SizeOf(handle_msg), read, nil) then
  begin
    KillProcess;
    Exit;
  end;

  _window_handle := handle_msg.Handle;

{$IFDEF dbg_msg}
  ShowMessage(Format('Retrieve handle: %d', [_window_handle]));
{$ENDIF}

  DisconnectNamedPipe(hPipe);
  CloseHandle(hPipe);

  InstallServerTerminateWaiter;

  Result := True;
end;

function TCanvasExtClient.Wait(ATimeOut: Cardinal): Boolean;
begin
  if _h_wait_server = 0 then
  begin
    Result := True;
    Exit;
  end;
  WaitForSingleObject(_h_wait_server, ATimeOut);
  CloseHandle(_h_wait_server);
  _h_wait_server := 0;
  Result := True;
end;

function TCanvasExtClient.WmCopyDataBuffer(AWnd: Hwnd; const ACtlCode: Integer;
 Buf: Pointer; const ASize: Integer): Integer;
var
  cds: TCopyDataStruct;
begin
{$IFNDEF dbg}
  if _b_server_teriminated then
  begin
    Result := -1;
    Exit;
  end;
{$ENDIF}
  cds.dwData := ACtlCode;
  cds.lpData := Buf;
  cds.cbData := ASize;
  Result := SendMessage(AWnd, WM_COPYDATA, 0, Integer(@cds));
end;

destructor TCanvasExtClient.Destroy;
begin
  ExitKill;
  _ms.Free;
  inherited;
end;

function TCanvasExtClient.CanvasOnTop(const AOnTop: Boolean): Integer;
var
  dum: Integer;
begin
  dum := Integer(AOnTop);
  _ms.Write(dum, SizeOf(dum));
  Result := SendMemoryStream(CANVAS_CTL_KEEPONTOP);
end;

function TCanvasExtClient.AllowClose(const AAllow: Boolean): Integer;
var
  dum: Integer;
begin
  dum := Integer(AAllow);
  _ms.Write(dum, SizeOf(dum));
  Result := SendMemoryStream(CANVAS_CTL_ALLOWCLOSE);
end;

function TCanvasExtClient.GetDimensions(var AWidth, AHeight: Integer): Integer;
var
  dum: Integer;
begin
  _ms.Write(dum, SizeOf(dum));
  Result := SendMemoryStream(CANVAS_CTL_GETDIMENSIONS);
  if Result = -1 then
  begin
    AWidth := -1;
    AHeight := -1;
  end
  else begin
    AWidth  := Word(Result);
    AHeight := Word(Result shr 16);
  end;
end;

function TCanvasExtClient.SetSize(const AWidth, AHeight: Integer): Integer;
var
  msg_sz: TCanvasClientMsg_XY;
begin
  msg_sz.X := AWidth;
  msg_sz.Y := AHeight;
  _ms.Write(msg_sz, SizeOf(msg_sz));
  Result := SendMemoryStream(CANVAS_CTL_SETSIZE);
end;

function TCanvasExtClient.ClearShapes: Integer;
var
  dum: Integer;
begin
  _ms.Write(dum, SizeOf(dum));
  Result := SendMemoryStream(CANVAS_CTL_CLEARCANVAS);
end;

function TCanvasExtClient.MoveCanvas(const ALeft, ATop: Integer): Integer;
var
  msg_sz: TCanvasClientMsg_XY;
begin
  msg_sz.X := ALeft;
  msg_sz.Y := ATop;
  _ms.Write(msg_sz, SizeOf(msg_sz));
  Result := SendMemoryStream(CANVAS_CTL_SETPOS);
end;

function TCanvasExtClient.SetTitle(const ATitle: string): Integer;
begin
  SerializeString(_ms, ATitle);
  Result := SendMemoryStream(CANVAS_CTL_SETTITLE);
end;

function TCanvasExtClient.DrawText(const AX, AY: Integer;
  const AMsg: string): Integer;
var
  s: TCanvasExt2DText;
begin
  s := TCanvasExt2DText.Create(AX, AY, AMsg);
  SetShapeParameters(s);
  if Assigned(FFont) then
    s.Font := FFont;

  s.Serialize(_ms);
  s.Free;
  Result := SendMemoryStream(CANVAS_CTL_DRAWTEXT);
end;

function TCanvasExtClient.DrawBitmap(const AX1, AY1, AX2, AY2: Integer;
  const AFileName: string): Integer;
var
  s: TCanvasExt2DImageLoader;
begin
  s := TCanvasExt2DImageLoader.Create(AX1, AY1, AX2, AY2, AFileName);
  SetShapeParameters(s);
  s.Serialize(_ms);
  s.Free;
  Result := SendMemoryStream(CANVAS_CTL_DRAWBMP);
end;

function TCanvasExtClient.Circle(const AX, AY, ARadius: Integer): Integer;
var
  s: TCanvasExtShape;
begin
  s := TCanvasExtShape4Points.Create(
       AX-ARadius, AY-ARadius,
       AX+ARadius, AY+ARadius, fptEllipse);
  SetShapeParameters(s);
  s.Serialize(_ms);
  s.Free;
  Result := SendMemoryStream(CANVAS_CTL_DRAW2DSHAPE);
end;

function TCanvasExtClient.Square(const AX, AY, ASide: Integer): Integer;
var
  s: TCanvasExtShape;
begin
  s := TCanvasExtShape4Points.Create(AX, AY, AX+ASide, AY+ASide);
  SetShapeParameters(s);
  s.Serialize(_ms);
  s.Free;
  Result := SendMemoryStream(CANVAS_CTL_DRAW2DSHAPE);
end;

function TCanvasExtClient.SaveCanvasToBitmap(
  const AFileName: string): Integer;
begin
  SerializeString(_ms, AFileName);
  Result := SendMemoryStream(CANVAS_CTL_SAVEBMP);
end;

function TCanvasExtClient.Rectangle(const AX1, AY1, AX2,
  AY2: Integer): Integer;
var
  s: TCanvasExtShape4Points;
begin
  s := TCanvasExtShape4Points.Create(AX1, AY1, AX2, AY2);
  SetShapeParameters(s);
  s.Serialize(_ms);
  s.Free;
  Result := SendMemoryStream(CANVAS_CTL_DRAW2DSHAPE);
end;

function TCanvasExtClient.Ellipse(const AX1, AY1, AX2,
  AY2: Integer): Integer;
var
  s: TCanvasExtShape4Points;
begin
  s := TCanvasExtShape4Points.Create(AX1, AY1, AX2, AY2, fptEllipse);
  SetShapeParameters(s);
  s.Serialize(_ms);
  s.Free;
  Result := SendMemoryStream(CANVAS_CTL_DRAW2DSHAPE);
end;

function TCanvasExtClient.Line(const AX1, AY1, AX2, AY2: Integer): Integer;
var
  s: TCanvasExtShape;
begin
  s := TCanvasExtShape4Points.Create(AX1, AY1, AX2, AY2, fptLine);
  SetShapeParameters(s);
  s.Serialize(_ms);
  s.Free;
  Result := SendMemoryStream(CANVAS_CTL_DRAW2DSHAPE);
end;

procedure TCanvasExtClient.SetFont(AFont: TFont);
begin
  if not Assigned(FFont) then
    FFont := TFont.Create;
  FFont.Name := AFont.Name;
  FFont.Size := AFont.Size;
  FFont.Color := AFont.Color;
  FFont.Style := AFont.Style
end;

function TCanvasExtClient.SendMemoryStream(const ACtlCode: Integer): Integer;
begin
  Result := WmCopyDataBuffer(_window_handle, ACtlCode, _ms.Memory, _ms.Size);
  _ms.Clear;
end;

procedure TCanvasExtClient.SetServerPath(const AFileName: string);
begin
  _server_proc_name := AFileName;
end;

procedure TCanvasExtClient.SetShapeParameters(AShape: TCanvasExtShape);
begin
  AShape.BrushColor := BrushColor;
  AShape.PenColor := PenColor;
  AShape.BrushStyle := BrushStyle;
end;

procedure TCanvasExtClient.ExitKill;
begin
  if _pi.hProcess<> 0 then
  begin
    // Signal CLOSE
    Self.CanvasClose;

    // Wait a little bit
    WaitForSingleObject(_pi.hProcess, 2000);

    // Forcefully terminate
    TerminateProcess(_pi.hProcess, 0);

    CloseHandle(Self._pi.hProcess);
    CloseHandle(Self._pi.hThread);
    
    Self._pi.hProcess := 0;
    Self._pi.hThread := 0;
  end;

  if _h_wait_server = 0 then
    Exit;
  TerminateThread(_h_wait_server, 0);
  CloseHandle(_h_wait_server);
  _h_wait_server := 0;
end;

function TCanvasExtClient.ClearCanvas(AColor: TColor): Integer;
begin
  ClearShapes;
  BrushColor := AColor;
  Result := Rectangle(0, 0, Width, Height);
end;

function TCanvasExtClient.CanvasClose: Integer;
begin
  Result := Integer(PostMessage(_window_handle, WM_CLOSE, 0, 0));
end;

end.
