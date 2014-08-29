unit uClientTestForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uCanvasExtShapes, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  s: TCanvasExt2DText;
  f: TFileStream;
  fnt: TFont;
begin
  s := TCanvasExt2DText.Create(0, 0, 'Hello world');
  fnt := TFont.Create;
  fnt.Size := 15;
  fnt.Name := 'Courier New';
  fnt.Color := clRed;

  s.Font := fnt;

  f := TFileStream.Create('a.bin', fmCreate);
  s.Serialize(f);
  f.Free;

  s.Draw(Canvas);
  s.Free;
  fnt.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  s: TCanvasExt2DText;
  f: TFileStream;
begin
  f := TFileStream.Create('a.bin', fmOpenRead);
  s := TCanvasExt2DText.Create;
  s.DeSerialize(f);
  f.Free;
  s.Draw(Canvas);
  s.Free;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  s: TCanvasExtShape4Points;
  f: TFileStream;
begin
  s := TCanvasExtShape4Points.Create(0, 0, 100, 100, fptEllipse);
  s.BrushColor := clGreen;
  s.PenColor := clBlue;
  s.BrushStyle := bsSolid;

  f := TFileStream.Create('a.bin', fmCreate);
  s.Serialize(f);
  f.Free;

  s.Draw(Canvas);
  s.Free;
end;

procedure TForm1.FormPaint(Sender: TObject);
const
  s: TCanvasExtShape4Points = nil;
begin
  inherited;
  Exit;
  if not Assigned(s) then
  begin
    s := TCanvasExtShape4Points.Create(0, 0, 100, 100, fptEllipse);
    s.BrushColor := clGreen;
  s.BrushStyle := bsSolid;
    s.PenColor := clBlue;
//    s.Filled := True;
  end;
  s.Draw(Canvas);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  s: TCanvasExtShape4Points;
  f: TFileStream;
begin
  f := TFileStream.Create('a.bin', fmOpenRead);

  s := TCanvasExtShape4Points.Create;
  s.DeSerialize(f);
  f.Free;

  s.Draw(Canvas);
  s.Free;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  s: TCanvasExt2DImageLoader;
  f: TFileStream;
begin
  s := TCanvasExt2DImageLoader.Create(0, 0, -1, -1, 'c:\windows\winnt.bmp');
  s.BrushColor := clRed;
  s.PenColor := clBlue;

  f := TFileStream.Create('a.bin', fmCreate);
  s.Serialize(f);
  f.Free;

  s.Draw(Canvas);
  s.Free;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  s: TCanvasExt2DImageLoader;
  f: TFileStream;
begin
  s := TCanvasExt2DImageLoader.Create;

  f := TFileStream.Create('a.bin', fmOpenRead);
  s.DeSerialize(f);
  f.Free;

  s.Draw(Canvas);
  s.Free;
end;

end.
