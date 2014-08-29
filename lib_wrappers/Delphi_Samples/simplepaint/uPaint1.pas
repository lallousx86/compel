unit uPaint1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ComCtrls, ExtCtrls, ActnList, ImgList, ToolWin,
  Buttons, StdActns, compel_lib, CompelScript, PaintTypes, ext;

type

  TfrmSimplePaint = class(TForm)
    StatusBar1: TStatusBar;
    grpCmd: TGroupBox;
    ColorDialog1: TColorDialog;
    Image1: TImage;
    PopupMenu1: TPopupMenu;
    ClearAll: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    tbLine: TToolButton;
    tbRect: TToolButton;
    ImageList1: TImageList;
    ActionList1: TActionList;
    actColor: TAction;
    actLineTool: TAction;
    actRectTool: TAction;
    actFreehandTool: TAction;
    tbPen: TToolButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    ColorLabel: TLabel;
    RectLabel: TLabel;
    freehandlabel: TLabel;
    LineLabel: TLabel;
    About1: TMenuItem;
    OpenBitmap1: TMenuItem;
    Savebitmap1: TMenuItem;
    N1: TMenuItem;
    Loadscript1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Edit2: TMenuItem;
    Runcommand1: TMenuItem;
    BitBtn1: TBitBtn;
    actEditRunCommand: TAction;
    actFileExit: TFileExit;
    actFileOpen: TFileOpen;
    actFileSave: TFileSaveAs;
    actEditClearImage: TAction;
    N3: TMenuItem;
    Clearimage1: TMenuItem;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    actLoadScript: TFileOpen;
    cbCommand: TComboBox;
    Help1: TMenuItem;
    N4: TMenuItem;
    CommandReference1: TMenuItem;
    actHelpReference: TAction;
    actEditLastScript: TAction;
    Editlastscript1: TMenuItem;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ColorLabelClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure actColorExecute(Sender: TObject);
    procedure actLineToolExecute(Sender: TObject);
    procedure actRectToolExecute(Sender: TObject);
    procedure actFreehandToolExecute(Sender: TObject);
    procedure actEditRunCommandExecute(Sender: TObject);
    procedure actEditClearImageExecute(Sender: TObject);
    procedure actFileOpenAccept(Sender: TObject);
    procedure actFileSaveAccept(Sender: TObject);
    procedure actLoadScriptAccept(Sender: TObject);
    procedure cbCommandKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure actHelpReferenceExecute(Sender: TObject);
    procedure actEditLastScriptExecute(Sender: TObject);
  private
   lastx,lasty:integer;
   startx,starty:integer;
   state:integer;
   DrawColor: TColor;

   Fscript: TCompelCanvasExt;

   procedure OnWmCommand(var Msg: TMessage);message WM_PAINT_CMD;

   function SaveToFile(const AFileName: string): Boolean;
   function LoadFromFile(const AFileName: string): Boolean;
   procedure CreateScript;
   procedure ClearCanvas;
  public
    { Public declarations }
  end;

var
  frmSimplePaint: TfrmSimplePaint;

implementation

{$R *.dfm}

procedure TfrmSimplePaint.FormCreate(Sender: TObject);
begin
 state:=0;
 image1.canvas.pen.Color:=clwhite;
 image1.Canvas.Brush.Color:=clwhite;
 image1.Canvas.Rectangle(0,0,image1.width-1,image1.height-1);
 image1.canvas.pen.Color:=clblack;
 Fscript := nil;
 CreateScript;
end;

procedure TfrmSimplePaint.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 //
end;

procedure TfrmSimplePaint.FormResize(Sender: TObject);
begin
{
 groupbox1.width:=clientwidth;
} 
 image1.width:=clientwidth;
 image1.Height:=clientheight-60;
 image1.picture.bitmap.width:=image1.width;
 image1.picture.bitmap.height:=image1.Height;
end;

procedure TfrmSimplePaint.ColorLabelClick(Sender: TObject);  // caption:='Color'
begin
 if colordialog1.Execute then
 begin
   DrawColor := colordialog1.Color;
   colorlabel.Color := DrawColor;
 end;
end;


// starts selected drawing action
//
procedure TfrmSimplePaint.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 // linemode - start line
 if state=1 then
 begin
  startx:=x; starty:=y;
  lastx:=x;lasty:=y;
  state:=2;
  image1.Canvas.Pen.Color:=clwhite;
 end;

 // rect mode - start rectangle
 if state=3 then
 begin
  startx:=x; starty:=y;
  lastx:=x;lasty:=y;
  state:=4;
  image1.Canvas.Pen.Color:=clwhite;
  image1.Canvas.Brush.Color:=clwhite;
 end;

 // freehand mode - start
 if state=5 then
 begin
  image1.Canvas.Pen.Color:=drawcolor;
  state:=6;
  end;
end;

procedure TfrmSimplePaint.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
  {
  var
  p:TGraphicBase;
}
begin
 if state=2 then begin   // linemode - draw
  image1.Canvas.Pen.Mode:=pmXOR;
  image1.Canvas.MoveTo(startx,starty);
  image1.Canvas.LineTo(lastx,lasty);
  image1.Canvas.Pen.Mode:=pmXOR;
  image1.Canvas.MoveTo(startx,starty);
  image1.Canvas.LineTo(x,y);
  lastx:=x; lasty:=y;
  end;
 if state=4 then begin   // rectmode - draw
  image1.Canvas.Pen.Mode:=pmXOR;
  image1.Canvas.MoveTo(startx,starty);
  image1.Canvas.LineTo(startx,lasty);
  image1.Canvas.LineTo(lastx,lasty);
  image1.Canvas.LineTo(lastx,starty);
  image1.Canvas.LineTo(startx,starty);
  image1.Canvas.Pen.Mode:=pmXOR;
  image1.Canvas.MoveTo(startx,starty);
  image1.Canvas.LineTo(startx,y);
  image1.Canvas.LineTo(x,y);
  image1.Canvas.LineTo(x,starty);
  image1.Canvas.LineTo(startx,starty);
  lastx:=x; lasty:=y;
  end;

 if state=6 then begin // freehand mode - draw
  image1.canvas.Pixels[x,y]:=drawcolor;
 end;

 statusbar1.Panels[0].Text:='mouse '+inttostr(x)+' '+inttostr(y);
end;

// ends current drawing action
//
procedure TfrmSimplePaint.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
{
var
  Line:TGraphicLine;
}
begin
 if state=2 then
 begin  // linemode - end line
  image1.Canvas.Pen.Mode:=pmXOR;
  image1.Canvas.MoveTo(startx,starty);
  image1.Canvas.LineTo(lastx,lasty);
  image1.Canvas.Pen.Color:=drawcolor;
  image1.Canvas.Pen.Mode:=pmCopy;
  image1.Canvas.MoveTo(startx,starty);
  image1.Canvas.LineTo(lastx,lasty);
  state:=1;
  end;
 if state=4 then
 begin  // rectmode - end line
  image1.Canvas.Pen.Mode:=pmXOR;
  image1.Canvas.MoveTo(startx,starty);
  image1.Canvas.LineTo(startx,lasty);
  image1.Canvas.LineTo(lastx,lasty);
  image1.Canvas.LineTo(lastx,starty);
  image1.Canvas.LineTo(startx,starty);

  image1.Canvas.Pen.Color:=drawcolor;
  image1.Canvas.Brush.Color:=drawcolor;
  image1.Canvas.Pen.Mode:=pmCopy;

  image1.Canvas.Rectangle(startx, starty, lastx, lasty);
{
  image1.Canvas.MoveTo(startx,starty);
  image1.Canvas.LineTo(startx,lasty);
  image1.Canvas.LineTo(lastx,lasty);
  image1.Canvas.LineTo(lastx,starty);
  image1.Canvas.LineTo(startx,starty);
}
  state:=3;
  end;
 if state=6 then
 begin // freehand mode - end
  state:=5;
  end;
end;


// Since the image cannot receive keys, set form.keypreview:=true
//
procedure TfrmSimplePaint.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if key=$1B then begin // ESC
  if state=1 then linelabel.Color:=clbtnface;
  if state=3 then rectlabel.Color:=clbtnface;
  if state=5 then freehandlabel.Color:=clbtnface;
  state:=0;
  end;
end;

procedure TfrmSimplePaint.actColorExecute(Sender: TObject);
begin
 if colordialog1.Execute then
 begin
   DrawColor := colordialog1.Color;
   colorlabel.Color := DrawColor;
 end;
end;

procedure TfrmSimplePaint.actLineToolExecute(Sender: TObject);
begin
 state:=1;
// tbPen.
 rectlabel.Color:=clbtnface;
 linelabel.Color:=clwhite;
 freehandlabel.color:=clbtnface;
end;

procedure TfrmSimplePaint.actRectToolExecute(Sender: TObject);
begin
 state:=3;
 linelabel.Color:=clbtnface;
 rectlabel.Color:=clwhite;
 freehandlabel.color:=clbtnface;
end;

procedure TfrmSimplePaint.actFreehandToolExecute(Sender: TObject);
begin
 state:=5;
 linelabel.Color:=clbtnface;
 rectlabel.Color:=clbtnface;
 freehandlabel.color:=clwhite;
end;

procedure TfrmSimplePaint.actEditRunCommandExecute(Sender: TObject);
var
  i: Integer;
begin
  i := Fscript.RunCommand(cbCommand.Text);
  if (i <> compel_error_success) then
  begin
    ShowMessage(Fscript.GetScriptError(i));
  end;
  cbCommand.Items.Insert(0, cbCommand.Text);
  cbCommand.Text := '';
end;

procedure TfrmSimplePaint.actEditClearImageExecute(Sender: TObject);
begin
  ClearCanvas;
end;

procedure TfrmSimplePaint.actFileOpenAccept(Sender: TObject);
begin
  if not LoadFromFile(actFileOpen.Dialog.FileName) then
    ShowMessage('Cannot load!');
end;

procedure TfrmSimplePaint.actFileSaveAccept(Sender: TObject);
begin
  SaveToFile(actFileSave.Dialog.FileName);
end;

procedure TfrmSimplePaint.actLoadScriptAccept(Sender: TObject);
begin
  // Create new script object
  CreateScript;
  Fscript.RunFile(actLoadScript.Dialog.FileName);
end;

procedure TfrmSimplePaint.OnWmCommand(var Msg: TMessage);
var
  cmd: PPaintCmdRec;
  c: TCanvas;
begin
  cmd := PPaintCmdRec(Msg.LParam);

  c := Image1.Canvas;
  c.Brush.Color := DrawColor;
  c.Pen.Color := DrawColor;

  case cmd.Kind of
    cmdLine:
    begin
      c.MoveTo(cmd.X1, cmd.Y1);
      c.LineTo(cmd.X2, cmd.Y2);
    end;
    cmdLoad:
    begin
      LoadFromFile(cmd.FileName);
    end;
    cmdSetTitle:
    begin
      Caption := Application.Title + ' - ' + cmd.FileName;
    end;
    cmdRect:
    begin
      c.Rectangle(cmd.X1, cmd.Y1, cmd.X2, cmd.Y2);
    end;
    cmdEllipse:
    begin
      c.Ellipse(cmd.X1, cmd.Y1, cmd.X2, cmd.Y2);
    end;
    cmdSelectColor:
    begin
      DrawColor := TColor(cmd.Color);
    end;
    cmdSave:
    begin
      SaveToFile(cmd.FileName);
    end;
    cmdClear:
    begin
      ClearCanvas;
    end;
  end;
end;

function TfrmSimplePaint.SaveToFile(const AFileName: string): Boolean;
var
  b: TBitmap;
  r: TRect;
begin
  r.Right := Image1.Width;
  r.Bottom := Image1.Height;
  r.Left := 0;
  r.Top := 0;

  b := TBitmap.Create;

  b.Width := r.Right;
  b.Height := r.Bottom;

  try
    b.Canvas.CopyRect(r, Image1.Canvas, r);
    b.SaveToFile(AFileName);
    Result := True;
  except
    Result := False;
  end;
  b.Free;
end;

function TfrmSimplePaint.LoadFromFile(const AFileName: string): Boolean;
var
  b: TBitmap;
begin
  b := TBitmap.Create;
  try
    b.LoadFromFile(AFileName);
    image1.Canvas.Draw(0, 0, b);
    Result := True;
  except
    Result := False;
  end;
  b.Free;
end;

procedure TfrmSimplePaint.CreateScript;
begin
  // Delete old script and create a new one
  if Assigned(Fscript) then
    Fscript.Free;
  Fscript := TCompelCanvasExt.Create(Handle);
end;

procedure TfrmSimplePaint.cbCommandKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    actEditRunCommandExecute(Sender);
    Key := 0;
    Exit;
  end;
end;

{!!
---------------------------
Command Reference
---------------------------
CvCreate - creates a new canvas
CvSelect _titles_ - Switchs to a canvas by title

CvTitle _title_ - sets a new title
CvClear - clears the canvas

CvLine _x1_ _y1_ _x2_ _y2_ - Draws a line
CvRect _x1_ _y1_ _x2_ _y2_ - Draws a rectangle
CvEllipse _x1_ _y1_ _x2_ _y2_ - Draws an ellipse
CvColor R G B - Selects the color
CvClose - Closes a canvas
CvSave _filename_ - Saves canvas to an image file
CvOpen _filename_ - Saves canvas to an image file

---------------------------
OK
---------------------------

}

procedure TfrmSimplePaint.ClearCanvas;
begin
  image1.canvas.pen.Color:=clwhite;
  image1.Canvas.Brush.Color:=clwhite;
  image1.Canvas.Rectangle(0,0,image1.width-1,image1.height-1);
end;

procedure TfrmSimplePaint.FormDestroy(Sender: TObject);
begin
  if Assigned(Fscript) then
    FreeAndNil(Fscript);
end;

procedure TfrmSimplePaint.actHelpReferenceExecute(Sender: TObject);
begin
  //
end;

procedure TfrmSimplePaint.actEditLastScriptExecute(Sender: TObject);
var
  s: string;
begin
  s := actLoadScript.Dialog.FileName;
  if not FileExists(s) then
    Exit;
  WinExec(PChar('notepad ' + s), SW_SHOW);
end;

end.
