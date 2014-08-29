program client_test;

uses
  Forms,
  uClientTestForm in 'uClientTestForm.pas' {Form1},
  uCanvasExtShapes in 'uCanvasExtShapes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
