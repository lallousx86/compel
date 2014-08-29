{$IFDEF DLL}
library compel_canvas_ext;
{$ELSE}
unit compel_canvas_ext;
{$ENDIF}

interface

uses uCanvasExt, CompelScript, compel_lib, Classes;

type
  TCompelCanvasExt = class
    private
      FScript: TCompelScript;
      FCanvasList: TList;
      FSelCanvas: TCanvasExt;
    public
      function FindCanvasById(Id: Pointer): TCanvasExt;
      constructor Create;
      procedure Clear;
      destructor Destroy;override;
  end;

implementation

// Extension functions

//    function ext_CanvasCreate(Script: TCompelScript; Args: array of string): Integer;
//    function ext_CanvasMove(Script: TCompelScript; Args: array of string): Integer;
//    function ext_CanvasTitle(Script: TCompelScript; Args: array of string): Integer;
//    function ext_ClearCanvas(Script: TCompelScript; Args: array of string): Integer; with a fill color
//    function ext_LoadBitmap(Script: TCompelScript; Args: array of string): Integer;
//    function ext_SelectCanvas(Script: TCompelScript; Args: array of string): Integer;
//    function ext_ShowCanvas(Script: TCompelScript; Args: array of string): Integer;
//    function ext_ShowHideCanvas(Script: TCompelScript; Args: array of string): Integer;
//    function ext_DrawLine(Script: TCompelScript; Args: array of string): Integer;
//    function ext_DrawRect(Script: TCompelScript; Args: array of string): Integer;
//    function ext_DrawEllipse(Script: TCompelScript; Args: array of string): Integer;
//    function ext_SaveCanvas(Script: TCompelScript; Args: array of string): Integer;
//    function ext_CloseCanvas(Script: TCompelScript; Args: array of string): Integer;
//    function ext_WriteText(Script: TCompelScript; Args: array of string): Integer;
//    function ext_SelectFont(Script: TCompelScript; Args: array of string): Integer;
//    function ext_WaitCanvas(Script: TCompelScript; Args: array of string): Integer;
//    function ext_ResizeCanvas(Script: TCompelScript; Args: array of string): Integer;
//    function ext_LoadBitmap(Script: TCompelScript; Args: array of string): Integer;
//    function ext_SelectBkColor(Script: TCompelScript; Args: array of string): Integer;
//    function ext_SelectFgColor(Script: TCompelScript; Args: array of string): Integer;

{ TCompelCanvasExt }

procedure TCompelCanvasExt.Clear;
var
  i: Integer;
begin
  for i := 0 to FCanvasList.Count - 1 do
    TCanvasExt(FCanvasList[i]).Free;
  FCanvasList.Free;
end;

constructor TCompelCanvasExt.Create;
begin
  FScript := nil;
  FScript.Tag := Pointer(Self);

  FCanvasList := TList.Create;
  FSelCanvas  := nil;
end;

destructor TCompelCanvasExt.Destroy;
begin
  if Assigned(FScript) then
    FScript.Free;
  FCanvasList.Free;
end;

function TCompelCanvasExt.FindCanvasById(Id: Pointer): TCanvasExt;
var
  i: Integer;
begin
  for i := 0 to FCanvasList.Count - 1 do
  begin
    if Pointer(FCanvasList[i]) = Id then
    begin
      Result := FCanvasList[i];
      Exit;
    end;
  end;
  Result := nil;
end;

end.
