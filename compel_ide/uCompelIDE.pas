unit uCompelIDE;

{$define auto1}

interface

uses Windows, Classes, Graphics, Forms, Controls, Menus,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, ImgList, StdActns,
  ActnList, ToolWin, uWatchWindow, Messages,
  uAbout, SysUtils,
  uEditorWindow, uDebuggerWindow, uBreakpointsWindow,
  CompelScript, CompelWorkspace,
  CompelIDEConsts, compel_lib, CompelIDEUtils, SynEdit, SynMemo;

type
  TfrmCompelIDE = class(TForm)
    dlgOpenDialog: TOpenDialog;
    dlgSaveDialog: TSaveDialog;
    ToolBar1: TToolBar;
    tbbFileNew: TToolButton;
    tbbFileOpen: TToolButton;
    tbbFileSave: TToolButton;
    tbbFileClose: TToolButton;
    ActionListAll: TActionList;
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFileExit: TAction;
    nactEditCut: TEditCut;
    nactEditCopy: TEditCopy;
    nactEditPaste: TEditPaste;
    actHelpAbout: TAction;
    StatusBar: TStatusBar;
    ImageListAll: TImageList;
    mnuMainMenu: TMainMenu;
    File1: TMenuItem;
    mnuFileNewItem: TMenuItem;
    mnuFileOpenItem: TMenuItem;
    mnuFileSaveItem: TMenuItem;
    mnuFileSaveAsItem: TMenuItem;
    N1: TMenuItem;
    mnuFileExitItem: TMenuItem;
    Edit1: TMenuItem;
    CutItem: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    Help1: TMenuItem;
    HelpAboutItem: TMenuItem;
    ToolButton7: TToolButton;
    mnuDebug: TMenuItem;
    Breakpointswindow1: TMenuItem;
    Watches1: TMenuItem;
    Debugger1: TMenuItem;
    N2: TMenuItem;
    ActionListDbg: TActionList;
    actDbgStepNext: TAction;
    actDbgSingleStep: TAction;
    actDbgRunToCursor: TAction;
    actDbgRun: TAction;
    actDbgSetScriptParameters: TAction;
    Run1: TMenuItem;
    mnuDbgStepInto: TMenuItem;
    mnuDbgStepOver: TMenuItem;
    actDbgLocateExecPoint: TAction;
    mnuWindows: TMenuItem;
    mnuDbgSetScriptParameters: TMenuItem;
    actEditGotoLine: TAction;
    Gotoline1: TMenuItem;
    actDbgStop: TAction;
    actDbgPause: TAction;
    mnuDbgStop: TMenuItem;
    mnuDbgPause: TMenuItem;
    actDbgBptToggle: TAction;
    actDbgRestartExec: TAction;
    actWindowWatchs: TAction;
    actWindowBreakpoints: TAction;
    actWindowEditor: TAction;
    actWindowDebugger: TAction;
    Editorwindow1: TMenuItem;
    N3: TMenuItem;
    mnuOptions: TMenuItem;
    N4: TMenuItem;
    Editoroptions1: TMenuItem;
    mnuDbgRestartExecution: TMenuItem;
    actFileClose: TAction;
    mnuFileClose: TMenuItem;
    actDbgSetNewExecPoint: TAction;
    mnuDbgSetNewExecPoint: TMenuItem;
    actDbgCycleView: TAction;
    actOptionsEditOptions: TAction;
    nactWindowMinimizeAll: TWindowMinimizeAll;
    N5: TMenuItem;
    MinimizeAll1: TMenuItem;
    nactWindowTileVertical: TWindowTileVertical;
    nactWindowTileHorizontal: TWindowTileHorizontal;
    nactWindowCascade: TWindowCascade;
    Cascade1: TMenuItem;
    ileHorizontally1: TMenuItem;
    ileVertically1: TMenuItem;
    mnuDbgCycleDebugLinesView: TMenuItem;
    LocateExecutionPoint1: TMenuItem;
    N6: TMenuItem;
    ogglebreakpoint1: TMenuItem;
    nactFileRunExternally: TAction;
    N7: TMenuItem;
    Runscriptfromconsole1: TMenuItem;
    mnuTest1: TMenuItem;
    mnuResetpositions: TMenuItem;
    actWindowResetPositions: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    actEditSearch: TAction;
    Gotoline2: TMenuItem;
    actEditSearchNext: TAction;
    N8: TMenuItem;
    mnuEditFindNext: TMenuItem;
    actEditReplace: TAction;
    Replace1: TMenuItem;

    procedure actFileNewExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actDbgStepNextExecute(Sender: TObject);
    procedure actDbgSingleStepExecute(Sender: TObject);
    procedure actDbgRunToCursorExecute(Sender: TObject);
    procedure actDbgRunExecute(Sender: TObject);
    procedure actWindowBreakpointsExecute(Sender: TObject);
    procedure actWindowWatchsExecute(Sender: TObject);
    procedure actDbgSetScriptParametersExecute(Sender: TObject);
    procedure actDbgLocateExecPointExecute(Sender: TObject);
    procedure actDbgStopExecute(Sender: TObject);
    procedure actDbgPauseExecute(Sender: TObject);
    procedure actDbgBptToggleExecute(Sender: TObject);
    procedure actDbgRestartExecExecute(Sender: TObject);
    procedure actEditGotoLineExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure actFileCloseExecute(Sender: TObject);
    procedure actWindowDebuggerExecute(Sender: TObject);
    procedure Editorwindow1Click(Sender: TObject);
    procedure actWindowEditorExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actDbgSetNewExecPointExecute(Sender: TObject);
    procedure actOptionsEditOptionsExecute(Sender: TObject);
    procedure actDbgCycleViewExecute(Sender: TObject);
    procedure nactFileRunExternallyExecute(Sender: TObject);
    procedure actWindowResetPositionsExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actEditSearchExecute(Sender: TObject);
    procedure nactEditCutUpdate(Sender: TObject);
    procedure nactEditCutExecute(Sender: TObject);
    procedure nactEditCopyUpdate(Sender: TObject);
    procedure nactEditCopyExecute(Sender: TObject);
    procedure nactEditPasteUpdate(Sender: TObject);
    procedure nactEditPasteExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actEditSearchNextExecute(Sender: TObject);
    procedure actEditReplaceExecute(Sender: TObject);
  private
    FCurrentState, FLastState: TIDEStates;
    FActivatedOnce: Boolean;

    FEditorWin: TfrmEditorWindow;
    FWatchesWin: TfrmWatchWindow;
    FBreakpointsWin: TfrmBreakpointsWindow;
    FDbgWin: TfrmDebuggerWindow;
    FScriptFileName: string;

    FCompelScript: TCompelScript;
    FIDEWorkspace, FWorkspace: TCompelWorkspace;
    FActionsArray: array of TAction;

    procedure ChangeActionsState(AState: TIDEStates);
    function  InitializeDebugger: Integer;
    procedure OnActivateOnce(Sender: TObject);

    procedure OnIDERefresh(var Msg: TMessage);message WM_IDE_REFRESH;
    procedure OnIDEShowScriptError(var Msg: TMessage); message WM_IDE_SHOWSCRIPTERROR;
    procedure OnScriptSuspended(var Msg: TMessage);message WM_COMPEL_ONSCRIPTSUSPENDED;
    procedure OnScriptFinished(var Msg: TMessage);message WM_COMPEL_ONSCRIPTFINISHED;
    function  InitializeCompelScript(const ACreate: Boolean = True): Boolean;
    function  InitializeWorkspace(const ACreate: Boolean = True): Boolean;
{$IFDEF dev}
    function  TempOnDeveloperMachine: Boolean;
    function  TempAutoExec(const AFunctionNumber: Integer): Boolean;
    function  IDEStateToString(const AState: TIDEStates): string;
{$ENDIF}    
    function  ConfirmFileSave: Boolean;

    procedure IDE_SaveLoadDimensions(const ASave: Boolean; const AState: Integer;AWks: TCompelWorkspace);

  public
    function ChangeState(AFrom, ATo: TIDEStates): Boolean;
    function CreateDebuggerWindow(const aCreate: Boolean = True) : Boolean;
    function CreateWatchesWindow(const aCreate: Boolean = True) : Boolean;
    function CreateBreakpointsWindow(const aCreate: Boolean = True) : Boolean;
    function CreateEditorWindow(const aCreate: Boolean = True) : Boolean;
    function LoadIntoEditor: Boolean;
    procedure SaveLoadDimensions(const ASave: Boolean;const AState:TIDEStates);
    function GetCCompelToolPath: string;
  end;

var
  frmCompelIDE: TfrmCompelIDE;

implementation

uses Math, uRunExternally;


{$R *.dfm}

const
 gIDEStates: array[0..28] of array[TIDEStates] of Boolean = (
	(False,True,False,True), // actDbgStepOver
	(False,True,False,True), // actDbgStepInto
	(False,True,False,True), // actDbgRunToCursor
	(False,True,False,True), // actDbgRun
	(False,False,False,True), // actDbgSetNewExecPoint
	(False,False,True,True), // actDbgCycleView
	(False,False,False,True), // actDbgLocateExecPoint
	(False,False,True,True), // actDbgStop
	(False,False,False,True), // actDbgRestartExec
	(False,False,True,False), // actDbgPause
	(False,False,True,True), // actDbgBptToggle
	(False,True,False,False), // actDbgSetScriptParameters
	(True,False,False,False), // actFileNew
	(True,False,False,False), // actFileOpen
	(False,True,False,False), // actFileClose
	(False,True,False,False), // actFileSave
	(False,True,False,False), // actFileSaveAs
	(True,True,False,False), // actFileExit
	(True,True,True,True), // actHelpAbout
	(False,True,True,True), // actEditGotoLine
	(True,True,True,True), // actOptionsEditOptions
	(False,True,True,True), // actWindowBreakpoints
	(False,False,True,True), // actWindowDebugger
	(False,True,True,True), // actWindowEditor
	(False,True,True,True),// actWindowWatchs
	(False,True,True,True),// actWindowResetPositions
	(False,True,True,True),// actEditSearch
	(False,True,True,True),// actEditSearchNext
	(False,True,True,True)// actEditReplace
);

// FActionsArray: array of TAction;
// procedure ChangeActionsState(ANewState: TIDEStates);
procedure TfrmCompelIDE.ChangeActionsState(AState: TIDEStates);
var
  i: Integer;
begin
  if Length(FActionsArray) = 0 then
  begin
    SetLength(FActionsArray, 29);
    FActionsArray[0] := actDbgStepNext;
    FActionsArray[1] := actDbgSingleStep;
    FActionsArray[2] := actDbgRunToCursor;
    FActionsArray[3] := actDbgRun;
    FActionsArray[4] := actDbgSetNewExecPoint;
    FActionsArray[5] := actDbgCycleView;
    FActionsArray[6] := actDbgLocateExecPoint;
    FActionsArray[7] := actDbgStop;
    FActionsArray[8] := actDbgRestartExec;
    FActionsArray[9] := actDbgPause;
    FActionsArray[10] := actDbgBptToggle;
    FActionsArray[11] := actDbgSetScriptParameters;
    FActionsArray[12] := actFileNew;
    FActionsArray[13] := actFileOpen;
    FActionsArray[14] := actFileClose;
    FActionsArray[15] := actFileSave;
    FActionsArray[16] := actFileSaveAs;
    FActionsArray[17] := actFileExit;
    FActionsArray[18] := actHelpAbout;
    FActionsArray[19] := actEditGotoLine;
    FActionsArray[20] := actOptionsEditOptions;
    FActionsArray[21] := actWindowBreakpoints;
    FActionsArray[22] := actWindowDebugger;
    FActionsArray[23] := actWindowEditor;
    FActionsArray[24] := actWindowWatchs;
    FActionsArray[25] := actWindowResetPositions;
    FActionsArray[26] := actEditSearch;
    FActionsArray[27] := actEditSearchNext;
    FActionsArray[28] := actEditReplace;
  end;
  for i := 0 to 28 do
    FActionsArray[i].Enabled := gIDEStates[i][AState];

{$IFDEF dev}
  if TempOnDeveloperMachine then
    Caption := IDEStateToString(AState);
{$ENDIF}    
end;

procedure TfrmCompelIDE.actFileNewExecute(Sender: TObject);
begin
  // Clear file name (make sure)
  FScriptFileName := '';
  
  // Goto into editing state
  ChangeState(FCurrentState, stateEditing);
end;

procedure TfrmCompelIDE.IDE_SaveLoadDimensions(const ASave: Boolean;
  const AState: Integer;AWks: TCompelWorkspace);
var
  dim: TCompelWorkspaceDimensions;
begin
  // Load dimensions
  if ASave = False then
  begin
    if not AWks.LoadSaveDimensions(True, strIdeTag, AState, dim) then
      Exit;
    CompelIDEUtils.FormToWksDimension(Self, dim, True);
  end
  else begin
    CompelIDEUtils.FormToWksDimension(Self, dim, False);
    AWks.LoadSaveDimensions(False, strIdeTag, AState, dim);
  end;
end;

procedure TfrmCompelIDE.actFileOpenExecute(Sender: TObject);
begin
{$IFDEF dev}
  if TempAutoExec(1) then
  begin
    //
  end else
{$ENDIF}  
  begin
    // Select the file name to open
    if not dlgOpenDialog.Execute then
      Exit;
    FScriptFileName := dlgOpenDialog.FileName;
  end;

  // Goto into editing state
  ChangeState(FCurrentState, stateEditing);
end;

procedure TfrmCompelIDE.actDbgSetNewExecPointExecute(Sender: TObject);
var
  s: string;
  Line: Integer;
begin
  if not Assigned(FDbgWin) then
    Exit;

  s := IntToStr(FDbgWin.GetSelectedLine+1);
  if not InputQuery('Change execution line', 'Set new script execution line:', s) then
    Exit;

  Line := StrToIntDef(s, FDbgWin.GetMaxEPLine + 2);
  if Line >= FDbgWin.GetMaxEPLine then
    Exit;

  FCompelScript.ScriptSetLineNo(Line-1);
  FDbgWin.UpdateView;
end;

procedure TfrmCompelIDE.actFileSaveExecute(Sender: TObject);
begin
  if Length(FScriptFileName) = 0 then
  begin
    ConfirmFileSave;
  end
  else begin
    FEditorWin.SaveFile;
  end;
end;

procedure TfrmCompelIDE.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmCompelIDE.actHelpAboutExecute(Sender: TObject);
var
  AboutBox: TAboutBox;
begin
  AboutBox := TAboutBox.Create(Self);
  AboutBox.ShowModal;
  AboutBox.Free;
end;

procedure TfrmCompelIDE.actEditSearchExecute(Sender: TObject);
begin
  if Assigned(FEditorWin) then
    FEditorWin.ShowSearchReplaceDialog(False);
end;

procedure TfrmCompelIDE.FormCreate(Sender: TObject);
begin
  // Set same filter for open and save
  dlgSaveDialog.Filter := dlgOpenDialog.Filter;

  FCurrentState := stateEmpty;
  FLastState := stateEmpty;

  FEditorWin := nil;
  FWatchesWin := nil;
  FBreakpointsWin := nil;
  FDbgWin := nil;
  FCompelScript := nil;

  FActivatedOnce := False;
  FIDEWorkspace := TCompelWorkspace.Create('compel_ide.cwks');

  // Disable windows animation
  CompelIDEUtils.DisableWindowsAnimation(True);
end;

procedure TfrmCompelIDE.OnIDERefresh(var Msg: TMessage);
begin
  if (Msg.WParam = ideconstsRedrawBreakpoints) and Assigned(FDbgWin) then
    FDbgWin.DrawBPTs;
end;

procedure TfrmCompelIDE.actDbgStepNextExecute(Sender: TObject);
var
  err: Integer;
begin
  err := InitializeDebugger;

  if err = ideconstsCannotInitialize then
  begin
    Exit;
  end;  

  // First run? we don't want to trace first instruction
  // We just initialize debugger
  if err = ideconstsFirstInitialization then
  begin
    ChangeState(FCurrentState, stateSuspended);
    FDbgWin.LocateEP;
    Exit;
  end;

  // Request script run
  if FCompelScript.ScriptStepNext then
  begin
    // Change to running state
    ChangeState(FCurrentState, stateRunning);
  end;
end;

procedure TfrmCompelIDE.actDbgSingleStepExecute(Sender: TObject);
var
  err: Integer;
begin
  err := InitializeDebugger;
  if err = ideconstsCannotInitialize then
    Exit;

  if err = ideconstsFirstInitialization then
  begin
    ChangeState(FCurrentState, stateSuspended);
    FDbgWin.LocateEP;
    Exit;
  end;

  // Request script run
  if FCompelScript.ScriptSingleStep then
  begin
    // Change to running state
    ChangeState(FCurrentState, stateRunning);
  end;
end;

procedure TfrmCompelIDE.actDbgRunToCursorExecute(Sender: TObject);
var
  Line: Integer;
  ErrInit: Integer;
begin
  ErrInit := InitializeDebugger;

  if ErrInit = ideconstsCannotInitialize then
    Exit;

  if ErrInit = ideconstsFirstInitialization then
    Line := FEditorWin.GetLineNo
  else
    Line := FDbgWin.GetSelectedLine;

  // Request
  if FCompelScript.ScriptRunToLine(Line) then
  begin
    // Change to running state
    ChangeState(FCurrentState, stateRunning);
  end;

end;

procedure TfrmCompelIDE.actDbgRunExecute(Sender: TObject);
begin
  if InitializeDebugger = ideconstsCannotInitialize then
    Exit;

  // Request script run
  if FCompelScript.ScriptRun then
  begin
    // Change to running state
    ChangeState(FCurrentState, stateRunning);
  end;
end;

procedure TfrmCompelIDE.actWindowBreakpointsExecute(Sender: TObject);
begin
  FBreakpointsWin.UpdateView(True);
end;

procedure TfrmCompelIDE.actWindowWatchsExecute(Sender: TObject);
begin
  FWatchesWin.UpdateView(True);
end;

procedure TfrmCompelIDE.actDbgSetScriptParametersExecute(Sender: TObject);
begin
  //
end;

procedure TfrmCompelIDE.actDbgLocateExecPointExecute(Sender: TObject);
begin
  FDbgWin.LocateEP;
end;

procedure TfrmCompelIDE.actDbgStopExecute(Sender: TObject);
begin
  FCompelScript.ScriptStopNow;
  ChangeState(FCurrentState, stateEditing);
end;

procedure TfrmCompelIDE.actDbgPauseExecute(Sender: TObject);
begin
  FCompelScript.ScriptPause;
end;

procedure TfrmCompelIDE.actDbgCycleViewExecute(Sender: TObject);
begin
  FDbgWin.CycleLines;
end;

procedure TfrmCompelIDE.actDbgBptToggleExecute(Sender: TObject);
begin
  FDbgWin.ToggleBP;
  FBreakpointsWin.UpdateBreakpoints;
end;

procedure TfrmCompelIDE.actDbgRestartExecExecute(Sender: TObject);
begin
  // Go to first line
  FCompelScript.ScriptSetLineNo(0);

  // Update EP display
  FDbgWin.UpdateView;
  FDbgWin.LocateEP;
end;

procedure TfrmCompelIDE.actEditGotoLineExecute(Sender: TObject);
var
  s: string;
  Line: Integer;
  bInDbgWin: Boolean;
begin
  Line := -1;
  bInDbgWin := False;
  
  if Assigned(FDbgWin) then
  begin
    Line := FDbgWin.GetEPLine;
    bInDbgWin := True;
  end;

  if (Line = -1) then
  begin
    Line := FEditorWin.GetLineNo;
    bInDbgWin := False;
  end;

  s := IntToStr(Line+1);
  if not InputQuery('Go to line', 'New line number:', s) then
    Exit;

  Line := StrToIntDef(s, -1);
  if (Line = -1) then
    Exit;

  if bInDbgWin then
    FDbgWin.SelectLine(Line-1)
  else
    FEditorWin.GotoLineNo(Line-1);
end;

function TfrmCompelIDE.CreateBreakpointsWindow(
  const aCreate: Boolean): Boolean;
var
  bCreateMe: Boolean;
begin
  Result := False;

  if aCreate then
  begin
    bCreateMe := not Assigned(FBreakpointsWin);

    if bCreateMe then
    begin
      FBreakpointsWin := TfrmBreakpointsWindow.Create(Self);
      
      // Assign workspace
      FBreakpointsWin.SetWorkspace(FWorkspace);

      // Load workspace
      FBreakpointsWin.SaveLoadWorkspace(False);
    end;

    // Assign script object
    FBreakpointsWin.SetScript(FCompelScript);

    // Update the view
    FBreakpointsWin.UpdateView(not bCreateMe);

    Result := True;
  end
  else begin
    // Save workspace
    FBreakpointsWin.SaveLoadWorkspace(True);

    // Close window
    FBreakpointsWin.Close;
    
    FBreakpointsWin.Free;
    FBreakpointsWin := nil;
  end;
end;

function TfrmCompelIDE.CreateDebuggerWindow(
  const aCreate: Boolean): Boolean;
var
  bCreateMe: Boolean;
begin
  Result := False;

  if aCreate then
  begin
    bCreateMe := not Assigned(FDbgWin);

    if not ConfirmFileSave then
    begin
      ShowMessage('You must save the file before continuing!');
      Exit;
    end;

    if bCreateMe then
    begin
      FDbgWin := TfrmDebuggerWindow.Create(Self);

      // Hide "cycle view" if it is not supported
      actDbgCycleView.Visible := FCompelScript.OnlyRawLines;

      FDbgWin.SetWorkspace(FWorkspace);

      // Bind actions
      FDbgWin.BindActions(
        actDbgBptToggle,
        actDbgLocateExecPoint,
        actDbgRunToCursor,
        actDbgPause,
        actDbgCycleView,
        actDbgSingleStep,
        actDbgStepNext,
        actDbgSetNewExecPoint);
    end;
    
    if not FDbgWin.InitializeScripting(FCompelScript, FScriptFileName) then
    begin
      ShowMessage('Could not initialize / begin debugging');

      // Destroy debugger window
      CreateDebuggerWindow(False);
      Exit;
    end;

    FDbgWin.UpdateView(not bCreateMe);

    Result := True;
  end
  else begin
    // Save debugger dimensions
//    FDbgWin.SaveLoadDimensions(True, Integer(stateRunning));

    // Save dimensions when returning from debug state
    SaveLoadDimensions(True, stateRunning);

    // Restore the editing dimensions
    SaveLoadDimensions(False, stateEditing);

    FDbgWin.Close;
    FDbgWin.Free;
    FDbgWin := nil;
    Result := True;
  end;
end;

function TfrmCompelIDE.CreateEditorWindow(const aCreate: Boolean): Boolean;
var
  bCreateMe: Boolean;
begin
  Result := False;

  if aCreate then
  begin
    bCreateMe := not Assigned(FEditorWin);

    if bCreateMe then
    begin
      FEditorWin := TfrmEditorWindow.Create(Self);

      FEditorWin.SetWorkspace(FWorkspace);

      FEditorWin.SetScript(FCompelScript);
    end;

    FEditorWin.UpdateView(not bCreateMe);

    Result := True;
  end else
  begin
    FEditorWin.Close;
    FEditorWin.Free;
    FEditorWin := nil;
  end;
end;

function TfrmCompelIDE.CreateWatchesWindow(
  const aCreate: Boolean): Boolean;
var
  bCreateMe: Boolean;
begin
  Result := False;

  if aCreate then
  begin
    bCreateMe := not Assigned(FWatchesWin);

    if bCreateMe then
    begin
      // Create window
      FWatchesWin := TfrmWatchWindow.Create(Self);
      
      // Assign workspace
      FWatchesWin.SetWorkspace(FWorkspace);

      // Load workspace
      FWatchesWin.SaveLoadWorkspace(False);
    end;

    // Assign script object
    FWatchesWin.SetScript(FCompelScript);

    FWatchesWin.UpdateView(not bCreateMe);

    Result := True;
  end
  else begin
    if not Assigned(FWatchesWin) then
      Exit;

    FWatchesWin.SaveLoadWorkspace(True);

    FWatchesWin.Close;

    FWatchesWin.Free;
    
    FWatchesWin := nil;
  end;
end;

procedure TfrmCompelIDE.Editorwindow1Click(Sender: TObject);
begin
  FEditorWin.UpdateView(True);
end;

procedure TfrmCompelIDE.FormActivate(Sender: TObject);
begin
  if not FActivatedOnce then
  begin
    FActivatedOnce := True;
    OnActivateOnce(Sender);
  end;
end;

procedure TfrmCompelIDE.OnActivateOnce(Sender: TObject);
begin

  // Load IDE dimensions
  IDE_SaveLoadDimensions(False, integer(stateEmpty), FIDEWorkspace);

  // Reset state machine
  ChangeState(stateEmpty, stateEmpty);

  // Check command line parameters
  if FileExists(ParamStr(1)) then
  begin
    FScriptFileName := ParamStr(1);
    ChangeState(FCurrentState, stateEditing);
  end;

{$IFDEF dev}
  if TempAutoExec(2) then
    Exit;
{$ENDIF}
end;

procedure TfrmCompelIDE.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if (actFileExit.Enabled) then
  begin
    // Exit program by going to empty state
    ChangeState(FCurrentState, stateEmpty);

    // Continue close action
  end else
  begin
    // Cancel close action
    Action := caNone;
  end;
end;

procedure TfrmCompelIDE.OnScriptFinished(var Msg: TMessage);
begin
  ChangeState(FCurrentState, stateEditing);
  FreeConsole;
end;

procedure TfrmCompelIDE.OnScriptSuspended(var Msg: TMessage);
begin
  FWatchesWin.UpdateView(False);
  FBreakpointsWin.UpdateView(False);
  ChangeState(FCurrentState, stateSuspended);

  FDbgWin.UpdateView(True);
  FDbgWin.LocateEP;
  FDbgWin.BringToFront;
end;

function TfrmCompelIDE.LoadIntoEditor: Boolean;
begin
  //
  CreateEditorWindow(True);
  if Length(FScriptFileName) = 0 then
    FEditorWin.OpenNewFile
  else
    FEditorWin.OpenFile(FScriptFileName);
  Result := True;
end;

procedure TfrmCompelIDE.actFileCloseExecute(Sender: TObject);
begin
  //
  ChangeState(FCurrentState, stateEmpty);
end;

function TfrmCompelIDE.InitializeDebugger: Integer;
begin
  Result := ideconstsCannotInitialize;

  // Check script object
  if not Assigned(FCompelScript) then
  begin
    ShowMessage('Script object not initialized!');
    Exit;
  end;

  // Debugger already initialized?
  if Assigned(FDbgWin) then
  begin
    FDbgWin.UpdateView(True);
    Result := ideconstsAlreadyInitialized;
    Exit;
  end;

  // Create the debugger window
  if not CreateDebuggerWindow then
  begin
    Result := ideconstsCannotInitialize;
    Exit;
  end;

  FDbgWin.Show;

  // Save editing dimensions before going to debug state
  SaveLoadDimensions(True, stateEditing);

  // Load debugging dimensions
  SaveLoadDimensions(False, stateRunning);

  // Load debugger window dimensions
//  FDbgWin.SaveLoadDimensions(False, Integer(stateRunning));

  FBreakpointsWin.ResetHitCount;

  Result := ideconstsFirstInitialization;
end;

function TfrmCompelIDE.InitializeWorkspace(
  const ACreate: Boolean): Boolean;
begin
  if ACreate then
  begin
    if Assigned(FWorkspace) then
    begin
      Result := True;
      Exit;
    end;
    // Create workspace object
    FWorkspace := TCompelWorkspace.Create(FScriptFileName + '.cwks');

    if Assigned(FEditorWin) then
      FEditorWin.SetWorkspace(FWorkspace);

    if Assigned(FBreakpointsWin) then
      FBreakpointsWin.SetWorkspace(FWorkspace);

    if Assigned(FWatchesWin) then
      FWatchesWin.SetWorkspace(FWorkspace);

    if Assigned(FDbgWin) then
      FDbgWin.SetWorkspace(FWorkspace);
  end else
  begin
    if not Assigned(FWorkspace) then
    begin
      Result := True;
      Exit;
    end;
    FWorkspace.Free;
    FWorkspace := nil;
  end;
  Result := True;
end;

procedure TfrmCompelIDE.actWindowDebuggerExecute(Sender: TObject);
begin
  FDbgWin.UpdateView(True);
end;

procedure TfrmCompelIDE.actWindowEditorExecute(Sender: TObject);
begin
  FEditorWin.UpdateView(True);
end;

function CompelScriptError(
  script: compel_script_t;
  lineno: Integer;
  err: Integer): Integer;stdcall;
var
  error_packet: compel_script_error_handler_cb_params_t;
  CompelScript: TCompelScript;
begin
  CompelScript := TCompelScript.FromScriptContext(script, strDBG_CLIENT);

  error_packet.script := script;
  error_packet.lineno := lineno;
  error_packet.err := err;

  // We need to send a message and wait for the response!
  SendMessage(
    TfrmCompelIDE(CompelScript.Tag).Handle,
    WM_IDE_SHOWSCRIPTERROR,
    Integer(@error_packet), 0);
  Result := error_packet.Return;
end;

function TfrmCompelIDE.InitializeCompelScript(
  const ACreate: Boolean): Boolean;
begin
  if ACreate then
  begin
    if Assigned(FCompelScript) then
    begin
      Result := True;
      Exit;
    end;
    // Create script object
    FCompelScript := TCompelScript.Create(Self, strDBG_CLIENT);

    // Set script callbacks
    FCompelScript.SetNotifyWindow(Self.Handle);

    FCompelScript.Tag := Pointer(Self);

    // Set error handler
    FCompelScript.SetErrorHandler(CompelScriptError);
  end else
  begin
    if not Assigned(FCompelScript) then
    begin
      Result := True;
      Exit;
    end;
    FCompelScript.Free;
    FCompelScript := nil;
  end;
  Result := True;
end;

{$IFDEF dev}

function TfrmCompelIDE.IDEStateToString(const AState: TIDEStates): string;
begin
  case AState of
    stateEmpty: Result := 'empty';
    stateEditing: Result := 'editing';
    stateRunning: Result := 'running';
    stateSuspended: Result := 'suspended';
  end;
end;

function TfrmCompelIDE.TempAutoExec(const AFunctionNumber: Integer): Boolean;
const
  bOnce: Boolean = False;
begin
  Result := False;
  if not TempOnDeveloperMachine then
    Exit;

  if bOnce = True then
    Exit;
    
  case AFunctionNumber of
  1:
    begin
      FScriptFileName := ExtractFilePath(ParamStr(0)) + 'fcd_prog1.compel';
    end;
  2:
    begin
      actFileOpenExecute(Self);
    end;
  end;
  bOnce := True;
  Result := True;
end;

function TfrmCompelIDE.TempOnDeveloperMachine: Boolean;
var
  bEnv: Boolean;
  bParam: Boolean;
begin
  Result := False;

  bEnv := Pos('Elias', SysUtils.GetEnvironmentVariable('USERNAME')) <> 0;
  bParam := Pos('/-dbg', Windows.GetCommandLine) <> 0;

  if (not (bEnv or bParam)) then
    Exit;

  Result := True;
end;
{$ENDIF}

procedure TfrmCompelIDE.SaveLoadDimensions(
  const ASave: Boolean;
  const AState: TIDEStates);
begin
  // Load dimensions?
  if ASave = False then
  begin
    IDE_SaveLoadDimensions(False, Integer(AState), FWorkspace);
    FEditorWin.SaveLoadDimensions(False, Integer(AState));
    FWatchesWin.SaveLoadDimensions(False, Integer(AState));
    FBreakpointsWin.SaveLoadDimensions(False, Integer(AState));
    if Assigned(FDbgWin) then
      FDbgWin.SaveLoadDimensions(False, Integer(AState));
  end
  // Save dimensions
  else begin
    IDE_SaveLoadDimensions(True, Integer(AState), FWorkspace);
    FEditorWin.SaveLoadDimensions(True, Integer(AState));
    FWatchesWin.SaveLoadDimensions(True, Integer(AState));
    FBreakpointsWin.SaveLoadDimensions(True, Integer(AState));
    if Assigned(FDbgWin) then
      FDbgWin.SaveLoadDimensions(True, Integer(AState));
  end;
end;

procedure TfrmCompelIDE.OnIDEShowScriptError(var Msg: TMessage);
var
  CompelScript: TCompelScript;
  s: string;
  error_packet: pcompel_script_error_handler_cb_params_t;
begin
  error_packet := pcompel_script_error_handler_cb_params_t(Msg.wParam);
  CompelScript := TCompelScript.FromScriptContext(error_packet.script, strDBG_CLIENT);

  FDbgWin.DrawEP(True);
  
  with error_packet^ do
  begin
    s := Format('Script runtime error occured:'#13 +
                'Error number: %d'#13+
                'Error description: %s'#13 +
                'Faulty line number: %d'#13 +
                'Faulty line code:'#13 + '%s'#13#13+
                'Do you want to ignore the error and continue?',
             [err,
             CompelScript.GetErrorString(err),
             lineno+1,
             CompelScript.ScriptGetLineCode(lineno)
             ]);
  end;

  if MessageDlg(s, mtError, [mbYes, mbNo], 0) = mrYes then
    error_packet.Return := compel_error_success
  else
    error_packet.Return := compel_error_script_stopped;
end;

procedure TfrmCompelIDE.nactEditCopyExecute(Sender: TObject);
begin
  FEditorWin.memoLines.CopyToClipboard;
end;

procedure TfrmCompelIDE.nactEditCopyUpdate(Sender: TObject);
begin
  nactEditCopy.Enabled := Assigned(FEditorWin);
end;

procedure TfrmCompelIDE.nactEditCutExecute(Sender: TObject);
begin
  FEditorWin.memoLines.CutToClipboard;
end;

procedure TfrmCompelIDE.nactEditCutUpdate(Sender: TObject);
begin
  nactEditCut.Enabled := Assigned(FEditorWin);
end;

procedure TfrmCompelIDE.nactEditPasteExecute(Sender: TObject);
begin
  FEditorWin.memoLines.PasteFromClipboard;
end;

procedure TfrmCompelIDE.nactEditPasteUpdate(Sender: TObject);
begin
  nactEditPaste.Enabled := Assigned(FEditorWin);
end;

procedure TfrmCompelIDE.nactFileRunExternallyExecute(Sender: TObject);
begin
  TfrmRunExternally.Go(Self);
end;

procedure TfrmCompelIDE.actWindowResetPositionsExecute(Sender: TObject);
var
  F: TForm;
  bDbgPosition: Boolean;
begin

  if Assigned(FDbgWin) then
  begin
    F := FDbgWin;
    bDbgPosition := True;
  end
  else begin
    F := FEditorWin;
    bDbgPosition := False;
  end;

  FBreakpointsWin.Height := 180;
  FBreakpointsWin.Width := 400;

  FWatchesWin.Height := 180;

  F.Left := 0;
  F.Top := 0;
  F.Width := Self.Width - 15;

  if bDbgPosition then
  begin
    FWatchesWin.Width := Self.Width - FBreakpointsWin.Width - 20;
    FBreakpointsWin.Left := FWatchesWin.Width;
    FDbgWin.BringToFront;
    FBreakpointsWin.BringToFront;
  end
  else begin
    FBreakpointsWin.WindowState := wsMinimized;
    FBreakpointsWin.Left := 0;
    FWatchesWin.Width := F.Width;
  end;

  FWatchesWin.BringToFront;

  FWatchesWin.Left := 0;

  FWatchesWin.Top := Self.Height - FWatchesWin.Height - 100;
  FBreakpointsWin.Top := FWatchesWin.Top;

  F.Height := FWatchesWin.Top;
end;

procedure TfrmCompelIDE.FormDestroy(Sender: TObject);
begin
  // Save IDE dimensions
  IDE_SaveLoadDimensions(True, Integer(stateEmpty), FIDEWorkspace);

  FIDEWorkspace.Free;

  if Assigned(FActionsArray) then
    Finalize(FActionsArray);

end;

function TfrmCompelIDE.GetCCompelToolPath: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + '\ccompel.exe';
end;

procedure TfrmCompelIDE.actOptionsEditOptionsExecute(Sender: TObject);
begin
  //
end;

// The state machine
function TfrmCompelIDE.ChangeState(AFrom, ATo: TIDEStates): Boolean;
var
  bUpdateStates: Boolean;
begin
  bUpdateStates := True;
  Result := True;

  repeat

  case ATo of
    // ===========================================================
    // Suspended state
    // ===========================================================
    stateSuspended:
    begin
      ChangeActionsState(ATo);
    end;

    // ===========================================================
    // Running
    // ===========================================================
    stateRunning:
    begin
      ChangeActionsState(ATo);
    end;

    // ===========================================================
    // Empty state
    // ===========================================================
    stateEmpty:
    begin
      // Is this a valid state switch?
      if (AFrom <> stateEditing) and (AFrom <> stateEmpty) then
      begin
        ShowMessage('Cannot switch from this state!');
        Exit;
      end;

      // Closing an open document?
      if AFrom = stateEditing then
      begin

        // Save modified file
        ConfirmFileSave;

        // Save dimensions before closing a document
        SaveLoadDimensions(True, stateEditing);

        // Load empty IDE dimensions
        IDE_SaveLoadDimensions(False, Integer(stateEmpty), FIDEWorkspace);

        // Remove Watches window
        CreateWatchesWindow(False);

        // remove Breakpoints window
        CreateBreakpointsWindow(False);

        // remove Editor window
        CreateEditorWindow(False);

        // Free scripting object
        InitializeCompelScript(False);

        // De-Initialize workspace
        InitializeWorkspace(False);
      end;

      // Clear script file
      FScriptFileName := '';

      ChangeActionsState(ATo);
      
    end;

    // ===========================================================
    // Editing state
    // ===========================================================
    stateEditing:
    begin
      // Switching from Debugger states to Editing state
      if (AFrom = stateSuspended) or (AFrom = stateRunning) then
      begin
        // Delete debugger window
        CreateDebuggerWindow(False);

        // Delete scripting object
        InitializeCompelScript(False);
      end;

      // Switching from Empty state to editing a file state
      if AFrom = stateEmpty then
      begin
        // Create workspace once
        InitializeWorkspace;

        // Load the file into the editor
        if not LoadIntoEditor then
          Exit;
      end;

      // Change actions state
      ChangeActionsState(ATo);

      // Re-create a new script object
      InitializeCompelScript;

      // Create/Show watches (minimized)
      CreateWatchesWindow(True);
//      FWatchesWin.Close;

      // Create/Show breakpoints window
      CreateBreakpointsWindow(True);
//      FBreakpointsWin.Close;

      // Show Editor window
      FEditorWin.Show;

      // Load dimensions when in editing mode
      if AFrom = stateEmpty then
        SaveLoadDimensions(False, stateEditing);
    end;
  end;
  until True;

  if bUpdateStates then
  begin
    FCurrentState := ATo;
    FLastState    := AFrom;
  end;

  Result := True;
end;


function TfrmCompelIDE.ConfirmFileSave: Boolean;
  function DoYouWantToSave: Boolean;
  begin
    Result := MessageDlg('File not saved, Save it now?', mtConfirmation, [mbYes, mbNo], 0) = mrYes;
  end;
begin
  // A new file?
  if (Length(FScriptFileName) = 0) and DoYouWantToSave then
  begin
    dlgSaveDialog.FileName := Format('compel_%d.compel', [FEditorWin.NewNumber]);
    if not dlgSaveDialog.Execute then
    begin
      Result := False;
      Exit;
    end;
    FScriptFileName := dlgSaveDialog.FileName;
    FEditorWin.FileName := FScriptFileName;
    actFileSaveExecute(Self);
    Result := True;
    Exit;
  end;

  if FEditorWin.IsModified then
  begin
    if DoYouWantToSave then
    begin
      actFileSaveExecute(Self);
    end
    else begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

procedure TfrmCompelIDE.actFileSaveAsExecute(Sender: TObject);
begin
  if Length(FScriptFileName) = 0 then
  begin
    ConfirmFileSave;
    Exit;
  end;

  dlgSaveDialog.FileName := FScriptFileName;
  if not dlgSaveDialog.Execute then
    Exit;
    
  // Update file names
  FScriptFileName := dlgSaveDialog.FileName;
  FEditorWin.FileName := FScriptFileName;
  FEditorWin.SaveFile;
  
  // Re-Initialize workspace
  InitializeWorkspace(False);
  InitializeWorkspace(True);
end;

procedure TfrmCompelIDE.actEditSearchNextExecute(Sender: TObject);
begin
  if Assigned(FEditorWin) then
    FEditorWin.DoSearchReplaceText(False, False);
end;

procedure TfrmCompelIDE.actEditReplaceExecute(Sender: TObject);
begin
  if Assigned(FEditorWin) then
    FEditorWin.ShowSearchReplaceDialog(TRUE);
end;

end.
