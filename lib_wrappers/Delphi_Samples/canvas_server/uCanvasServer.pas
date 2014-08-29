unit uCanvasServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uCanvasExtShapes;

type
  TfrmCanvasExt = class(TForm)
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FbActivateOnce: Boolean;

    FShapes: TCanvasExtShapeList;
    FOldFormStyle: TFormStyle;
    FbAllowClose: Boolean;
    FPaintBeginning: Boolean;

    procedure OnActivateOnce(Sender: TObject);
    procedure OnMsgCtl(var Msg: TMessage);message WM_CANVAS_CTL;
    procedure OnMsgClearShapes(var Msg: TMessage);message WM_CANVAS_CLEAR;
    function  DrawToBitmap(const AFileName: string): Boolean;
    procedure SetPaintBeginning(const Value: Boolean);

    procedure OnWMCopyData(var Msg: TMessage);message WM_COPYDATA;
    procedure InsertShape(AShape: TCanvasExtShape);
  public
  published
    property PaintBeginning: Boolean read FPaintBeginning write SetPaintBeginning;
  end;

var
  frmCanvasExt: TfrmCanvasExt;

implementation

{$R *.dfm}

uses CanvasExtTypes, uCanvasExtUtil;

// TMultiReadExclusiveWriteSynchronizer
// Finalize


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
{
  if not Assigned(FInit) then
    Exit;
  Self.Width := FInit.Width;
  Self.Height := FInit.Height;
  if FInit.X <> -1 then
    Self.Left := FInit.X;
  if FInit.Y <> -1 then
    Self.Top := FInit.Y;
}
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
    CANVAS_CTL_SHOWHIDE:
    begin
      if Boolean(Msg.LParam) then
        Self.Show
      else
        Self.Hide;
    end;
  end;
end;

procedure TfrmCanvasExt.InsertShape(AShape: TCanvasExtShape);
begin
  FShapes.Add(AShape);
  Invalidate;
end;

procedure TfrmCanvasExt.OnWMCopyData(var Msg: TMessage);
var
  pcds: PCopyDataStruct;
  ms: TMemoryStream;
  s: string;
  msg_sz: PCanvasClientMsg_XY;
  shp_2dtxt: TCanvasExt2DText;
  shp_2dimg: TCanvasExt2DImageLoader;
  shp_2d4pt: TCanvasExtShape4Points;
  shp: TCanvasExtShape;
  i: Integer;
begin
  ms := TMemoryStream.Create;

  // Get the buffer
  pcds := PCopyDataStruct(Msg.LParam);

  // Write the buffer to the memory stream
  ms.Write(pcds.lpData^, pcds.cbData);

  // Reset buffer posisiton
  ms.Seek(0, soBeginning);

  shp := nil;

  case pcds.dwData of
  CANVAS_CTL_SETTITLE:
    begin
      DeSerializeString(ms, s);
      Caption := s;
      Application.Title := s;
    end;
  CANVAS_CTL_SETSIZE:
    begin
      msg_sz := PCanvasClientMsg_XY(ms.Memory);
      Self.Width := msg_sz.X;
      Self.Height := msg_sz.Y;
    end;
  CANVAS_CTL_DRAWTEXT:
    begin
      shp_2dtxt := TCanvasExt2DText.Create;
      shp_2dtxt.DeSerialize(ms);
      shp := shp_2dtxt;
    end;
  CANVAS_CTL_CLEARCANVAS:
    begin
      Msg.Result := FShapes.Count;
      FShapes.Clear;
      Invalidate;
    end;
  CANVAS_CTL_GETDIMENSIONS:
    begin
      Msg.Result := Windows.MakeLong(Word(Self.Width), Word(Self.Height));
    end;
  CANVAS_CTL_DRAWBMP:
    begin
      shp_2dimg := TCanvasExt2DImageLoader.Create;
      shp_2dimg.DeSerialize(ms);
      shp := shp_2dimg;
    end;
  CANVAS_CTL_DRAW2DSHAPE:
    begin
      shp_2d4pt := TCanvasExtShape4Points.Create;
      shp_2d4pt.DeSerialize(ms);
      shp := shp_2d4pt;
    end;
  CANVAS_CTL_SAVEBMP:
    begin
      DeSerializeString(ms, s);
      Msg.Result := Integer(DrawToBitmap(s));
   end;
  CANVAS_CTL_SETPOS:
    begin
      msg_sz := PCanvasClientMsg_XY(ms.Memory);
      Self.Left := msg_sz.X;
      Self.Top  := msg_sz.Y;
    end;
  CANVAS_CTL_KEEPONTOP:
    begin
      ms.Read(i, SizeOf(i));
      if Boolean(i) then
        SetWindowPos(Handle, HWND_TOPMOST, Left, Top, Width, Height, SWP_SHOWWINDOW)
      else
        SetWindowPos(Handle, HWND_NOTOPMOST, Left, Top, Width, Height, SWP_SHOWWINDOW);
    end;
  CANVAS_CTL_ALLOWCLOSE:
    begin
      ms.Read(i, SizeOf(i));
      FbAllowClose := Boolean(i);
    end;
  end;

  if Assigned(shp) then
    InsertShape(shp);
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

end.
