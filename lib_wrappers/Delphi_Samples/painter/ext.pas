unit ext;

interface

uses CompelScript, compel_lib, Classes, Graphics, Windows, PaintTypes;

type
  TCompelCanvasExt = class
    private
      FScript: TCompelScript;
      FWindow: Hwnd;

      function SendCommand(var ACmd: TPaintCmdRec): Integer;
    public
      function RegisterFunctions: Boolean;
      constructor Create(AHwnd: Hwnd);
      destructor Destroy;override;

      class function RGBtoTColor(r, g, b: Byte): TColor;overload;
      class function RGBtoTColor(n: Integer): TColor;overload;

      function GetScriptError(const ACode: Integer): string;
      function RunCommand(const ACmd: string): Integer;
      function RunFile(const AFileName: string): Integer;
      function ext_CanvasSetTitle(Script: TCompelScript; Args: array of string): Integer;
      function ext_SelectColor(Script: TCompelScript; Args: array of string): Integer;
      function ext_ClearCanvas(Script: TCompelScript; Args: array of string): Integer;
      function ext_DrawRect(Script: TCompelScript; Args: array of string): Integer;
      function ext_DrawLine(Script: TCompelScript; Args: array of string): Integer;
      function ext_DrawEllipse(Script: TCompelScript; Args: array of string): Integer;
      function ext_LoadImage(Script: TCompelScript; Args: array of string): Integer;
      function ext_SelectRGBColor(Script: TCompelScript; Args: array of string): Integer;
      function ext_SaveCanvas(Script: TCompelScript; Args: array of string): Integer;
  end;

implementation

const
  canvas_compel_client = '{ED9AE0D5-FB63-4034-A21E-9BAE7AB4F9E8}';

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

{ TCompelCanvasExt }

class function TCompelCanvasExt.RGBtoTColor(n: Integer): TColor;
begin
  Result := RGBtoTColor(n and $FF, n shr 8, n shr 16);
end;

class function TCompelCanvasExt.RGBtoTColor(r, g, b: Byte): TColor;
begin
  Result := RGB(b, g, r);
end;

constructor TCompelCanvasExt.Create(AHwnd: Hwnd);
begin
  FScript := TCompelScript.Create(nil, canvas_compel_client);
  FScript.Tag := Self;
  FWindow := AHwnd;
  Self.RegisterFunctions;
end;

destructor TCompelCanvasExt.Destroy;
begin
  FScript.Free;
  inherited;
end;

function TCompelCanvasExt.ext_CanvasSetTitle(Script: TCompelScript;
  Args: array of string): Integer;
var
  cmd: TPaintCmdRec;
begin
  Self := TCompelCanvasExt(Script.Tag);

  cmd.FileName := Args[0];
  cmd.Kind := cmdSetTitle;

  Self.SendCommand(cmd);

  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_ClearCanvas(Script: TCompelScript;
  Args: array of string): Integer;
var
  cmd: TPaintCmdRec;
begin
  Self := TCompelCanvasExt(Script.Tag);

  cmd.Kind := cmdClear;
  Self.SendCommand(cmd);
  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_DrawEllipse(Script: TCompelScript;
  Args: array of string): Integer;
var
  cmd: TPaintCmdRec;
begin
  Self := TCompelCanvasExt(Script.Tag);

  cmd.X1 := TCompelScript.ParseNumber(Args[0]);
  cmd.Y1 := TCompelScript.ParseNumber(Args[1]);
  cmd.X2 := TCompelScript.ParseNumber(Args[2]);
  cmd.Y2 := TCompelScript.ParseNumber(Args[3]);
  cmd.Kind := cmdEllipse;

  Self.SendCommand(cmd);

  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_DrawLine(Script: TCompelScript;
  Args: array of string): Integer;
var
  cmd: TPaintCmdRec;
begin
  Self := TCompelCanvasExt(Script.Tag);

  cmd.X1 := TCompelScript.ParseNumber(Args[0]);
  cmd.Y1 := TCompelScript.ParseNumber(Args[1]);
  cmd.X2 := TCompelScript.ParseNumber(Args[2]);
  cmd.Y2 := TCompelScript.ParseNumber(Args[3]);
  cmd.Kind := cmdLine;

  Self.SendCommand(cmd);

  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_DrawRect(Script: TCompelScript;
  Args: array of string): Integer;
// Syntax:
// Canvas.Rectangle X1 Y1 X2 Y2
var
  cmd: TPaintCmdRec;
begin
  Self := TCompelCanvasExt(Script.Tag);

  cmd.X1 := TCompelScript.ParseNumber(Args[0]);
  cmd.Y1 := TCompelScript.ParseNumber(Args[1]);
  cmd.X2 := TCompelScript.ParseNumber(Args[2]);
  cmd.Y2 := TCompelScript.ParseNumber(Args[3]);
  cmd.Kind := cmdRect;

  Self.SendCommand(cmd);

  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_LoadImage(Script: TCompelScript;
  Args: array of string): Integer;
var
  cmd: TPaintCmdRec;
begin
  Self := TCompelCanvasExt(Script.Tag);

  cmd.FileName := Args[0];
  cmd.Kind := cmdLoad;
  Self.SendCommand(cmd);
  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_SaveCanvas(Script: TCompelScript;
  Args: array of string): Integer;
var
  cmd: TPaintCmdRec;
begin
  Self := TCompelCanvasExt(Script.Tag);

  cmd.FileName := Args[0];
  cmd.Kind := cmdSave;
  Self.SendCommand(cmd);
  Result := compel_error_success;
end;

function TCompelCanvasExt.ext_SelectColor(Script: TCompelScript;
  Args: array of string): Integer;
var
  cmd: TPaintCmdRec;
begin
  Self := TCompelCanvasExt(Script.Tag);

  cmd.X1 := TCompelScript.ParseNumber(Args[0]);
  cmd.Y1 := TCompelScript.ParseNumber(Args[1]);
  cmd.X2 := TCompelScript.ParseNumber(Args[2]);

  cmd.Color := TColor(RGBtoTColor(cmd.X2, cmd.Y1, cmd.X1));
  cmd.Kind := cmdSelectColor;

  Self.SendCommand(cmd);

  Result := compel_error_success;
end;

function TCompelCanvasExt.RegisterFunctions: Boolean;
const
  prefix = 'Cv';

var
  L: TList;
  F: TClsFnc;
begin
  Result := True;
  if not Assigned(FScript) then
    Exit;

  L := TList.Create;
  L.Add(TClsFnc.Create('Title', ext_CanvasSetTitle, 1, 1));
  L.Add(TClsFnc.Create('Save', ext_SaveCanvas, 1, 1));
  L.Add(TClsFnc.Create('Load', ext_LoadImage, 1, 1));
  L.Add(TClsFnc.Create('RgbColor', ext_SelectRGBColor, 1, 1));

  L.Add(TClsFnc.Create('Clear', ext_ClearCanvas, 0, 0));

  L.Add(TClsFnc.Create('Color', ext_SelectColor, 3, 3));

  L.Add(TClsFnc.Create('Rect', ext_DrawRect, 4, 4));
  L.Add(TClsFnc.Create('Ellipse', ext_DrawEllipse, 4, 4));
  L.Add(TClsFnc.Create('Line', ext_DrawLine, 4, 4));

  repeat
    F := TClsFnc(L.First);
    FScript.LU_Register(prefix + F.Name, F.cb, F.Min, F.Max);
    F.Free;
    L.Delete(0);
  until L.Count = 0;
  L.Free;
end;

function TCompelCanvasExt.RunCommand(const ACmd: string): Integer;
begin
  Result := FScript.InterpretLine(ACmd);
  FScript.ScriptRunNow;
end;

function TCompelCanvasExt.SendCommand(var ACmd: TPaintCmdRec): Integer;
begin
  if FWindow = 0 then
  begin
    Result := -1;
    Exit;
  end;
  Result := SendMessage(FWindow, WM_PAINT_CMD, 1, Integer(@ACmd));
end;

function TCompelCanvasExt.RunFile(const AFileName: string): Integer;
begin
  Fscript.ScriptLoad(AFileName);
  Result := Fscript.ScriptRunNow;
end;

function TCompelCanvasExt.GetScriptError(const ACode: Integer): string;
begin
  Result := Fscript.GetErrorString(ACode);
end;

function TCompelCanvasExt.ext_SelectRGBColor(Script: TCompelScript;
  Args: array of string): Integer;
var
  cmd: TPaintCmdRec;
begin
  Self := TCompelCanvasExt(Script.Tag);

  cmd.Color := TCompelScript.ParseNumber(Args[0]);
  cmd.Kind := cmdSelectColor;

  Self.SendCommand(cmd);

  Result := compel_error_success;
end;

end.
