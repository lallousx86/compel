unit compel_canvas;

interface

uses
 uCanvasClient,
 CompelScript, compel_lib, Classes, SysUtils, Windows, Graphics, Forms;

type
  TCompelCanvasExt = class
    private
      FScript: TCompelScript;
      FCanvasList: TList;
      FSelCanvas: TCanvasExtClient;

      function FindCanvasById(Idx: Integer): TCanvasExtClient;
      function FindCanvas(ACanvas: TCanvasExtClient): Integer;
    public
      function AttachToScript(script: compel_script_t): Boolean;
      function RegisterFunctions: Boolean;
      constructor Create;
      procedure Clear;
      destructor Destroy;override;

      class function RGBtoTColor(r, g, b: Byte): TColor;overload;
      class function RGBtoTColor(n: Integer): TColor;overload;

//      function ext_CanvasBeginPaint(Script: TCompelScript; Args: array of string): Integer;
      function ext_CanvasOnTop(Script: TCompelScript; Args: array of string): Integer;
      function ext_CanvasCreate(Script: TCompelScript; Args: array of string): Integer;
      function ext_CanvasClose(Script: TCompelScript; Args: array of string): Integer;
      function ext_CanvasWait(Script: TCompelScript; Args: array of string): Integer;
      function ext_CanvasSetTitle(Script: TCompelScript; Args: array of string): Integer;
      function ext_ResizeCanvas(Script: TCompelScript; Args: array of string): Integer;
      function ext_SelectBkColor(Script: TCompelScript; Args: array of string): Integer;
      function ext_SelectFgColor(Script: TCompelScript; Args: array of string): Integer;
      function ext_ClearCanvas(Script: TCompelScript; Args: array of string): Integer;
      function ext_DrawText(Script: TCompelScript; Args: array of string): Integer;
      function ext_DrawRect(Script: TCompelScript; Args: array of string): Integer;
      function ext_DrawLine(Script: TCompelScript; Args: array of string): Integer;
      function ext_Dimensions(Script: TCompelScript; Args: array of string): Integer;
      function ext_SelectFont(Script: TCompelScript; Args: array of string): Integer;
      function ext_DrawEllipse(Script: TCompelScript; Args: array of string): Integer;
      function ext_DrawCircle(Script: TCompelScript; Args: array of string): Integer;
      function ext_LoadImage(Script: TCompelScript; Args: array of string): Integer;
      function ext_CanvasMove(Script: TCompelScript; Args: array of string): Integer;
      function ext_SaveCanvas(Script: TCompelScript; Args: array of string): Integer;
      function ext_SelectCanvas(Script: TCompelScript; Args: array of string): Integer;
  end;

implementation

const
  canvas_compel_client = '{482ADB62-1D66-4578-A125-E888C81987DB}';

type
  TClsFnc = class
  public
    Name: string;
    cb: TCompelScriptLUCallback;
    Min, Max: Integer;
    Desc: string;
    constructor Create(
      AName: string;
      ACallback:TCompelScriptLUCallback;
      AMin, AMax: Integer;
      ADesc: string = '');
  end;

{ TClsFnc }

constructor TClsFnc.Create(AName: string;
  ACallback: TCompelScriptLUCallback; AMin, AMax: Integer; ADesc: string);
begin
  Name := AName;
  cb := ACallBack;
  Min := AMin;
  Max := AMax;
  Desc := ADesc;
end;

// Extension functions

//    function ext_ShowCanvas(Script: TCompelScript; Args: array of string): Integer;
//    function ext_ShowHideCanvas(Script: TCompelScript; Args: array of string): Integer;

{ TCompelCanvasExt }

class function TCompelCanvasExt.RGBtoTColor(r, g, b: Byte): TColor;
begin
  Result := RGB(b, g, r);
end;

class function TCompelCanvasExt.RGBtoTColor(n: Integer): TColor;
begin
  Result := RGBtoTColor(n and $FF, n shr 8, n shr 16);
end;

function TCompelCanvasExt.FindCanvas(ACanvas: TCanvasExtClient): Integer;
var
  i: Integer;
begin
  for i := 0 to FCanvasList.Count - 1 do
  begin
    if FCanvasList[i] = ACanvas then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

function TCompelCanvasExt.AttachToScript(script: compel_script_t): Boolean;
begin
  Fscript := TCompelScript.FromScript(script, canvas_compel_client);
  FScript.Tag := Pointer(Self);
  Result := True;
end;

procedure TCompelCanvasExt.Clear;
var
  i: Integer;
begin
  for i := 0 to FCanvasList.Count - 1 do
    TCanvasExtClient(FCanvasList[i]).Free;
  FCanvasList.Free;
end;

constructor TCompelCanvasExt.Create;
begin
  FScript := nil;

  FCanvasList := TList.Create;
  FSelCanvas  := nil;
end;

destructor TCompelCanvasExt.Destroy;
begin
  if Assigned(FScript) then
    FScript.Free;
  Clear;
end;

function TCompelCanvasExt.FindCanvasById(Idx: Integer): TCanvasExtClient;
begin
  Result := nil;

  if Idx >= FCanvasList.Count then
    Exit;

  if not Assigned(FCanvasList[Idx]) then
    Exit;

  Result := FCanvasList[Idx];
end;

function TCompelCanvasExt.RegisterFunctions: Boolean;
const
  ns = 'Canvas.';

var
  L: TList;
  F: TClsFnc;
begin
  Result := True;
  if not Assigned(FScript) then
    Exit;

  L := TList.Create;
  L.Add(TClsFnc.Create('Create', ext_CanvasCreate, 0, 4));
  L.Add(TClsFnc.Create('Close', ext_CanvasClose, 0, 0));
  L.Add(TClsFnc.Create('Wait', ext_CanvasWait, 0, 0));
  L.Add(TClsFnc.Create('SetTitle', ext_CanvasSetTitle, 1, 1));
  L.Add(TClsFnc.Create('Select', ext_SelectCanvas, 1, 1));
  L.Add(TClsFnc.Create('SaveImage', ext_SaveCanvas, 1, 1));
  L.Add(TClsFnc.Create('Resize', ext_ResizeCanvas, 2, 2));
  L.Add(TClsFnc.Create('Clear', ext_ClearCanvas, 0, 2));
  L.Add(TClsFnc.Create('SelectBkColor', ext_SelectBkColor, 1, 1));
  L.Add(TClsFnc.Create('SelectFgColor', ext_SelectFgColor, 1, 1));
  L.Add(TClsFnc.Create('DrawText', ext_DrawText, 3, 3));
  L.Add(TClsFnc.Create('Rectangle', ext_DrawRect, 4, 4));
  L.Add(TClsFnc.Create('Ellipse', ext_DrawEllipse, 4, 4));
  L.Add(TClsFnc.Create('Circle', ext_DrawCircle, 3, 3));
  L.Add(TClsFnc.Create('Line', ext_DrawLine, 4, 4));
  L.Add(TClsFnc.Create('SelectFont', ext_SelectFont, 3, 4));
  L.Add(TClsFnc.Create('Dimensions', ext_Dimensions, 4, 4));
//  L.Add(TClsFnc.Create('BeginPaint', ext_CanvasBeginPaint, 1, 1));
  L.Add(TClsFnc.Create('SetOnTop', ext_CanvasOnTop, 1, 1));
  L.Add(TClsFnc.Create('Move', ext_CanvasMove, 2, 2));
  L.Add(TClsFnc.Create('LoadImage', ext_LoadImage, 3, 5));

  repeat
    F := TClsFnc(L.First);
    FScript.LU_Register(ns + F.Name, F.cb, F.Min, F.Max);
    F.Free;
    L.Delete(0);
  until L.Count = 0;
  L.Free;
end;

function TCompelCanvasExt.ext_CanvasClose(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.Close
// Closes active canvas or selected canvas
// Returns: 0 or 1
var
  Canvas: TCanvasExtClient;
  Idx: Integer;
begin
  Self := TCompelCanvasExt(Script.Tag);
  Canvas := FSelCanvas;
  if not Assigned(Canvas) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;

  Canvas.Free;

  // Was is the default?
  if Length(Args) = 0 then
    FSelCanvas := nil;

  Idx := FindCanvas(Canvas);

  if (Idx <> -1) then
    FCanvasList[Idx] := nil;

  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_CanvasCreate(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.Create [Width] [Height] [X] [Y]
// Returns ID
var
  Canvas: TCanvasExtClient;
  Len: Integer;
  Width, Height, X, Y: Integer;
  server_name: string;
  path: array[0..MAX_PATH] of char;
begin
  Self := TCompelCanvasExt(Script.Tag);

  server_name := '';
  if GetModuleFileName(GetModuleHandle('compel_canvas_ext'), path, MAX_PATH)<>0 then
  begin
//    server_name := path;
    server_name := ExtractFilePath(path) + '\canvas_server.exe';
    if not FileExists(server_name) then
      server_name := '';
  end;

  X := -1;
  Y := -1;
  Height := -1;
  Width := -1;

  Len := Length(Args);
  if Len >= 1 then
    Width := TCompelScript.ParseNumber(Args[0]);

  if Len >= 2 then
    Height := TCompelScript.ParseNumber(Args[1]);

  if Len >= 3 then
    X := TCompelScript.ParseNumber(Args[2]);

  if Len >= 4 then
    Y := TCompelScript.ParseNumber(Args[3]);

  // Create new canvas
  Canvas := TCanvasExtClient.Create;

  if Length(server_name) <> 0 then
    Canvas.SetServerPath(server_name);

  if not Canvas.Launch then
  begin
    Result := compel_error_wrong_param_value;
    Exit;
  end;

  if Height <> -1 then
    Canvas.SetSize(Width, Height);

  if Y <> -1 then
    Canvas.MoveCanvas(X, Y);
      
  Canvas.BrushColor := clWhite;

  // Pass return value
  Script.LU_SetRetVal(IntToStr(FCanvasList.Count));

  // Insert it into canvas list
  FCanvasList.Add(Canvas);

  // Select this new canvas
  FSelCanvas := Canvas;

  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_CanvasWait(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.Wait
var
  Canvas: TCanvasExtClient;
begin
  Self := TCompelCanvasExt(Script.Tag);
  Canvas := FSelCanvas;
  if not Assigned(Canvas) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;
  Canvas.Wait;
  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_CanvasSetTitle(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.SetTitle Title
var
  Canvas: TCanvasExtClient;
begin
  Self := TCompelCanvasExt(Script.Tag);
  Canvas := FSelCanvas;
  if not Assigned(Canvas) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;
  Canvas.SetTitle(Args[0]);
  Result := compel_error_success;
end;


function TCompelCanvasExt.ext_ResizeCanvas(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.Resize NewWidth NewHeight
var
  Canvas: TCanvasExtClient;
begin
  Self := TCompelCanvasExt(Script.Tag);
  Canvas := FSelCanvas;
  if not Assigned(Canvas) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;
  Canvas.SetSize(TCompelScript.ParseNumber(Args[0]), TCompelScript.ParseNumber(Args[1]));
  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_SelectBkColor(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.SelectBkColor RGB
var
  Canvas: TCanvasExtClient;
begin
  Self := TCompelCanvasExt(Script.Tag);
  Canvas := FSelCanvas;
  if not Assigned(Canvas) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;

  Canvas.BrushColor := RGBtoTColor(TCompelScript.ParseNumber(Args[0]));

  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_SelectFgColor(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.SelectFgColor RGB
var
  Canvas: TCanvasExtClient;
begin
  Self := TCompelCanvasExt(Script.Tag);
  Canvas := FSelCanvas;
  if not Assigned(Canvas) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;

  Canvas.PenColor := RGBtoTColor(TCompelScript.ParseNumber(Args[0]));

  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_ClearCanvas(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.Clear [BkColor_RGB]
var
  Canvas: TCanvasExtClient;
  cl: TColor;
begin
  Self := TCompelCanvasExt(Script.Tag);
  Canvas := FSelCanvas;
  if not Assigned(Canvas) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;

  if Length(Args) >= 1 then
  begin
    cl := RGBtoTColor(TCompelScript.ParseNumber(Args[0]));
  end
  else
    cl := clWhite;
  Canvas.ClearCanvas(cl);
  Result := compel_error_success;
end;


function TCompelCanvasExt.ext_DrawText(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.DrawText X Y "text"
var
  Canvas: TCanvasExtClient;
begin
  Self := TCompelCanvasExt(Script.Tag);
  Canvas := FSelCanvas;
  if not Assigned(Canvas) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;

  Canvas.DrawText(
   TCompelScript.ParseNumber(Args[0]),
   TCompelScript.ParseNumber(Args[1]),
   Args[2]);

  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_DrawRect(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.Rectangle X1 Y1 X2 Y2
var
  Canvas: TCanvasExtClient;
begin
  Self := TCompelCanvasExt(Script.Tag);
  Canvas := FSelCanvas;
  if not Assigned(Canvas) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;

  Canvas.Rectangle(
   TCompelScript.ParseNumber(Args[0]),
   TCompelScript.ParseNumber(Args[1]),
   TCompelScript.ParseNumber(Args[2]),
   TCompelScript.ParseNumber(Args[3]));

  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_DrawLine(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.Line X1 Y1 X2 Y2
var
  Canvas: TCanvasExtClient;
begin
  Self := TCompelCanvasExt(Script.Tag);
  Canvas := FSelCanvas;
  if not Assigned(Canvas) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;

  Canvas.Line(
   TCompelScript.ParseNumber(Args[0]),
   TCompelScript.ParseNumber(Args[1]),
   TCompelScript.ParseNumber(Args[2]),
   TCompelScript.ParseNumber(Args[3]));

  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_Dimensions(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.MaxDimensions &$ScreenX &$ScreenY &$CanvasX &$CanvasY
var
  Canvas: TCanvasExtClient;
  sx, sy, cx, cy: TCompelValue;
begin
  Self := TCompelCanvasExt(Script.Tag);
  Canvas := FSelCanvas;
  if not Assigned(Canvas) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;
  
  sx := TCompelValue.FindValue(script, Args[0]);
  sy := TCompelValue.FindValue(script, Args[1]);
  cx := TCompelValue.FindValue(script, Args[2]);
  cy := TCompelValue.FindValue(script, Args[3]);

  if (not Assigned(sx)) or
     (not Assigned(sy)) or
     (not Assigned(cx)) or
     (not Assigned(cy)) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;

  sx.IntValue := Screen.Width;
  sy.IntValue := Screen.Height;
  cx.IntValue := Canvas.Width;
  cy.IntValue := Canvas.Height;
   
  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_SelectFont(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.SelectFont "FontName" FontSize Fontcolor [fontstyle]
var
  Canvas: TCanvasExtClient;
  Font: TFont;
  s: string;
begin
  Result := compel_error_success;

  Self := TCompelCanvasExt(Script.Tag);
  Canvas := FSelCanvas;
  if not Assigned(Canvas) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;

  try
    Font := TFont.Create;
    Font.Name := Args[0];
    Font.Size := TCompelScript.ParseNumber(Args[1]);
    Font.Color:= RGBtoTColor(TCompelScript.ParseNumber(Args[2]));

    if Length(Args) >= 4 then
    begin
      s := LowerCase(Args[3]);

      if Pos('bold', s)<>0 then
        Font.Style := Font.Style + [fsBold];

      if Pos('italic', s)<>0 then
        Font.Style := Font.Style + [fsItalic];
        
      if Pos('underline', s)<>0 then
        Font.Style := Font.Style + [fsUnderline];

      if Pos('strikeout', s)<>0 then
        Font.Style := Font.Style + [fsStrikeOut];
    end;

    Canvas.Font := Font;
  except
    // Invalid font name
    Result := compel_error_symbol_expected;
  end;

  if Assigned(Font) then
    FreeAndNil(Font);
end;

{
function TCompelCanvasExt.ext_CanvasBeginPaint(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.BeginPaint 0|1
var
  Canvas: TCanvasExtClient;
begin
  Self := TCompelCanvasExt(Script.Tag);
  Canvas := FSelCanvas;
  if not Assigned(Canvas) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;
  Canvas.BeginPaint(Boolean(TCompelScript.ParseNumber(Args[0])));
  Result := compel_error_success;
end;
}

function TCompelCanvasExt.ext_CanvasOnTop(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.SetOnTop 0|1
var
  Canvas: TCanvasExtClient;
begin
  Self := TCompelCanvasExt(Script.Tag);
  Canvas := FSelCanvas;
  if not Assigned(Canvas) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;
  Canvas.CanvasOnTop(Boolean(TCompelScript.ParseNumber(Args[0])));
  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_DrawEllipse(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.Ellipse X1 Y1 X2 Y2
var
  Canvas: TCanvasExtClient;
begin
  Self := TCompelCanvasExt(Script.Tag);
  Canvas := FSelCanvas;
  if not Assigned(Canvas) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;

  Canvas.Ellipse(
   TCompelScript.ParseNumber(Args[0]),
   TCompelScript.ParseNumber(Args[1]),
   TCompelScript.ParseNumber(Args[2]),
   TCompelScript.ParseNumber(Args[3]));

  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_DrawCircle(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.Circle X Y Radius
var
  Canvas: TCanvasExtClient;
begin
  Self := TCompelCanvasExt(Script.Tag);
  Canvas := FSelCanvas;
  if not Assigned(Canvas) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;

  Canvas.Circle(
   TCompelScript.ParseNumber(Args[0]),
   TCompelScript.ParseNumber(Args[1]),
   TCompelScript.ParseNumber(Args[2]));

  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_CanvasMove(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.Move X Y
var
  Canvas: TCanvasExtClient;
begin
  Canvas := TCompelCanvasExt(Script.Tag).FSelCanvas;
  if not Assigned(Canvas) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;

  Canvas.MoveCanvas(
   TCompelScript.ParseNumber(Args[0]),
   TCompelScript.ParseNumber(Args[1]));

  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_LoadImage(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.LoadImage FileName X1 Y1 [X2 Y2]
var
  Canvas: TCanvasExtClient;
  X2, Y2: Integer;
begin
  Canvas := TCompelCanvasExt(Script.Tag).FSelCanvas;
  if not Assigned(Canvas) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;

  if Length(Args) >= 5 then
  begin
    X2 := TCompelScript.ParseNumber(Args[3]);
    Y2 := TCompelScript.ParseNumber(Args[4]);    
  end
  else begin
    X2 := -1;
    Y2 := -1;
  end;
  Canvas.DrawBitmap(
   TCompelScript.ParseNumber(Args[1]),
   TCompelScript.ParseNumber(Args[2]),
   X2,
   Y2,
   Args[0]);

  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_SaveCanvas(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.SaveCanvas FileName
var
  Canvas: TCanvasExtClient;
begin
  Canvas := TCompelCanvasExt(Script.Tag).FSelCanvas;
  if not Assigned(Canvas) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;

  Canvas.SaveCanvasToBitmap(Args[0]);

  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_SelectCanvas(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.SelectCanvas ID
var
  Canvas: TCanvasExtClient;
begin
  Self   := TCompelCanvasExt(Script.Tag);

  Canvas := FindCanvasById(TCompelScript.ParseNumber(Args[0]));
  if not Assigned(Canvas) then
  begin
    Result := compel_error_symbol_expected;
    Exit;
  end;
  FSelCanvas := Canvas;
  Result := 0;
end;

end.
