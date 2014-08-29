program canvasclient;

{$APPTYPE CONSOLE}

{$DEFINE db g}

uses
  Windows,
  Messages,
  SysUtils,
  Dialogs,
  Classes,
  Graphics,
  uCanvasClient;

procedure main;
var
  c: TCanvasExtClient;
begin
  c := TCanvasExtClient.Create;
  repeat
    if not c.Launch then
      Break;
    c.SetTitle('Xss');
    c.SetSize(400, 400);
    c.PenColor := clRed;
    c.BrushColor := clRed;
    c.DrawText(0, 0, 'Hello world!');
    c.Rectangle(10, 10, 100, 100);
    c.PenColor := clGreen;
    c.Line(20, 20, 200, 100);
    c.BrushColor := clWhite;
    c.Circle(50, 50, 20);
    c.Square(250, 250, 60);
    c.BrushColor := clYellow;
    c.PenColor := clBlack;
    c.Ellipse(40, 40, 60, 60);
    c.DrawBitmap(70, 70, -1, -1, 'c:\windows\winnt.bmp');
    c.MoveCanvas(0, 0);
    c.SetSize(c.Width*2, c.Height*2);
    c.CanvasOnTop(True);
    c.CanvasOnTop(False);
    c.Wait;
  until True;
  c.Free;
end;

begin
  main;
end.