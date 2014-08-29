program CompelIDE;

uses
  Forms,
  uCompelIDE in 'uCompelIDE.pas' {frmCompelIDE},
  uAbout in 'uAbout.PAS' {AboutBox},
  uEditorWindow in 'uEditorWindow.pas' {frmEditorWindow},
  uBreakpointsWindow in 'uBreakpointsWindow.pas' {frmBreakpointsWindow},
  uDebuggerWindow in 'uDebuggerWindow.pas' {frmDebuggerWindow},
  uWatchWindow in 'uWatchWindow.pas' {frmWatchWindow},
  CompelScript in 'CompelScript.pas',
  CompelUtil in 'CompelUtil.pas',
  CompelWatches in 'CompelWatches.pas',
  CompelWorkspace in 'CompelWorkspace.pas',
  CompelIDEConsts in 'CompelIDEConsts.pas',
  CompelIDEUtils in 'CompelIDEUtils.pas',
  uRunExternally in 'uRunExternally.pas' {frmRunExternally},
  dlgSearchText in 'dlgSearchText.pas' {TextSearchDialog},
  dlgConfirmReplace in 'dlgConfirmReplace.pas' {ConfirmReplaceDialog},
  dlgReplaceText in 'dlgReplaceText.pas' {TextReplaceDialog};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'COMPEL IDE';
  Application.CreateForm(TfrmCompelIDE, frmCompelIDE);
  Application.CreateForm(TConfirmReplaceDialog, ConfirmReplaceDialog);
  Application.Run;
end.

