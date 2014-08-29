program Paint1;

uses
  Forms,
  uPaint1 in 'uPaint1.pas' {frmSimplePaint},
  compel_lib in 'compel_lib.pas',
  CompelScript in 'CompelScript.pas',
  CompelUtil in 'CompelUtil.pas',
  ext in 'ext.pas',
  PaintTypes in 'PaintTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Simple Paint';
  Application.CreateForm(TfrmSimplePaint, frmSimplePaint);
  Application.Run;
end.
