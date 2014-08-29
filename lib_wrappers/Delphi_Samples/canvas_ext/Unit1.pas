unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uCanvasExt, ExtCtrls, CompelScript;

type
  TForm1 = class(TForm)
    cmdCreate: TButton;
    cmdCircle: TButton;
    Button1: TButton;
    lbledtParameters: TLabeledEdit;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    edtBrushColor: TLabeledEdit;
    ColorDialog1: TColorDialog;
    edtPenColor: TLabeledEdit;
    Button5: TButton;
    Button6: TButton;
    edtText: TLabeledEdit;
    FontDialog1: TFontDialog;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);

    procedure FormActivate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cmdCircleClick(Sender: TObject);
    procedure cmdCreateClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button4Click(Sender: TObject);
    procedure edtBrushColorDblClick(Sender: TObject);
    procedure edtBrushColorChange(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure edtTextDblClick(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);

  private
    FCanvas: TCanvasExt;
    tok: TCompelTokenizer;
    function ParamAsTRect: TRect;
    function ParseParams: Integer;
  public
    procedure CreateCanvas(const ACreate: Boolean = True);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button10Click(Sender: TObject);
var
  r: TRect;
begin
  r := ParamAsTRect;
  FCanvas.SetSize(r.Left, r.Top);
end;

procedure TForm1.Button11Click(Sender: TObject);
var
  r: TRect;
begin
  r := ParamAsTRect;
  FCanvas.DrawBitmap(r.Left, r.Top, -1, -1, edtText.Text);
end;

procedure TForm1.Button12Click(Sender: TObject);
var
  r: TRect;
begin
  r := ParamAsTRect;
  FCanvas.CanvasOnTop(Boolean(r.Left));
end;

procedure TForm1.Button13Click(Sender: TObject);
var
  r: TRect;
begin
  r := ParamAsTRect;
  FCanvas.ShowHide(Boolean(r.Left));
end;

procedure TForm1.Button14Click(Sender: TObject);
begin
  FCanvas.SaveCanvasToBitmap(edtText.Text);
end;

procedure TForm1.Button15Click(Sender: TObject);
var
  r: TRect;
begin
  r := ParamAsTRect;
  FCanvas.AllowClose(Boolean(r.Left));
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  r: TRect;
begin
  r := ParamAsTRect;
  FCanvas.Rectangle(r.Left, r.Top, r.Right, r.Bottom);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  r: TRect;
begin
  r := ParamAsTRect;
  FCanvas.Ellipse(r.Left, r.Top, r.Right, r.Bottom);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  r: TRect;
begin
  r := ParamAsTRect;
  FCanvas.Square(r.Left, r.Top, r.Right);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  r: TRect;
begin
  r := ParamAsTRect;
  FCanvas.Line(r.Left, r.Top, r.Right, r.Bottom);
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  r: TRect;
begin
  r := ParamAsTRect;
  FCanvas.Rectangle(r.Left, r.Top, r.Right, r.Bottom, True);
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  r: TRect;
begin
  r := ParamAsTRect;
  FCanvas.DrawText(r.Left, r.Top, edtText.Text);
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  FCanvas.ClearCanvas(FCanvas.BrushColor);
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  r: TRect;
begin
  r := ParamAsTRect;
  FCanvas.DeleteShape(r.Left);
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  FCanvas.SetTitle(edtText.Text);
end;

procedure TForm1.cmdCreateClick(Sender: TObject);
begin
  FCanvas.ClearShapes;
end;

procedure TForm1.cmdCircleClick(Sender: TObject);
var
  r: TRect;
begin
  r := ParamAsTRect;
  FCanvas.Circle(r.Left, r.Top, r.Right);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCanvas := nil;
  tok := TCompelTokenizer.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  tok.Free;
end;

procedure TForm1.edtBrushColorChange(Sender: TObject);
var
  Ctrl: TLabeledEdit;
  Color: TColor;
begin
  Ctrl := Sender as TLabeledEdit;
  Color := StrToIntDef(Ctrl.Text, $FFFFFF);
  if Ctrl.Tag = 1 then
    FCanvas.BrushColor := Color
  else if Ctrl.Tag = 2 then
    FCanvas.PenColor := Color;
end;

procedure TForm1.edtBrushColorDblClick(Sender: TObject);
var
  Ctrl: TLabeledEdit;
begin
  Ctrl := Sender as TLabeledEdit;
  if not ColorDialog1.Execute then
    Exit;
  Ctrl.Text := Format('$%0.6X', [ColorDialog1.Color]);
end;

procedure TForm1.edtTextDblClick(Sender: TObject);
begin
  if not FontDialog1.Execute then
    Exit;
  FCanvas.Font := FontDialog1.Font;
end;

function TForm1.ParamAsTRect: TRect;
begin
  tok.Parse(lbledtParameters.Text, ', ');
  Result.Left  := tok.GetInt(0);
  Result.Top   := tok.GetInt(1);
  Result.Right := tok.GetInt(2);
  Result.Bottom:= tok.GetInt(3);
end;

function TForm1.ParseParams: Integer;
begin
  Result := tok.Parse(lbledtParameters.Text, ', ');
end;

procedure TForm1.FormActivate(Sender: TObject);
const
  bOnce: Boolean = False;
begin
  if bOnce = False then
  begin
    CreateCanvas;
    bOnce := True;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CreateCanvas(False);
end;

procedure TForm1.CreateCanvas(const ACreate: Boolean);
var
  Init: TCanvasExtInit;
begin
  if ACreate then
  begin
    if Assigned(FCanvas) then
      Exit;

    Init := TCanvasExtInit.Create;

    Init.Width := 309;
    Init.Height := 349;
    Init.X := Self.Left + Self.Width;
    Init.Y := Self.Top;
    FCanvas := TCanvasExt.Create(Init);
    edtBrushColorChange(edtBrushColor);
    edtBrushColorChange(edtPenColor);
//    FCanvas.BrushStyle := bsSolid;
  end
  else begin
    if not Assigned(FCanvas) then
      Exit;
    FCanvas.Free;
  end;
end;

end.
