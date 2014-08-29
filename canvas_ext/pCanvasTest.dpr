program pCanvasTest;

{%TogetherDiagram 'ModelSupport_pCanvasTest\default.txaPackage'}

uses
  Forms,
  uCanvasTestForm in 'uCanvasTestForm.pas' {frmCanvasTest},
  uCanvasExt in 'uCanvasExt.pas' {frmCanvasExt},
  uCanvasExtShapes in 'uCanvasExtShapes.pas',
  compel_canvas in 'compel_canvas.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmCanvasTest, frmCanvasTest);
  Application.Run;
end.
