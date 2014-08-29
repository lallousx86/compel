unit uDebuggerWindow;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ToolWin, ComCtrls, Buttons, ActnList,
  CompelScript, CompelWorkspace, Menus,
  CompelIDEUtils, CompelIDEConsts, compel_lib;

type
  TfrmDebuggerWindow = class(TForm)
    lbScript: TListBox;
    ToolBar2: TToolBar;
    cmdToggleBreakpoint: TBitBtn;
    cmdCycle: TBitBtn;
    cmdRunToCursor: TBitBtn;
    cmdLocateExecPoint: TBitBtn;
    popmenuDbg: TPopupMenu;
    mnuSetNewEP: TMenuItem;
    mnuToggleBP: TMenuItem;
    actlistDebugWindow: TActionList;
    actPatchCode: TAction;
    Patchline1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbScriptDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure actPatchCodeExecute(Sender: TObject);
  private
    FScript: TCompelScript;
    FWorkspace: TCompelWorkspace;
    FScriptFileName: string;
    FLines: TStringList;
    FRawLines: Boolean;
    FLastEP: Integer;
    FLastBPTs: TList;

    procedure DrawLinePrefix(const APrefix: Char;
       const APrefixIndex, ALineNo: Integer;
       const ABeginUpdate: Boolean = True);
    function GetBreakpoints: TCompelBreakpointList;

    procedure PatchCodeLine(const AIndex: Integer;const ACode: string);
  public
    procedure BindActions(
      ActToggleBreakpoint,
      ActLocateExecPoint,
      ActRunToCursor,
      ActPause,
      ActCycle,
      ActSingleStep,
      ActStepNext,
      ActSetNewExecPoint
      : TAction);

    procedure DrawEP(const ASetEP: Boolean = False);
    procedure UpdateView(const AShow: Boolean = False);
    procedure SetWorkspace(AWks: TCompelWorkspace);
    procedure SaveLoadWorkspace(const ASave: Boolean = True);

    function InitializeScripting(const AScript: TCompelScript;
          const AScriptFileName: string): Boolean;

    function  DrawLines(const ARawLines: Boolean = False): Boolean;
    procedure CycleLines;
    function  GetEPLine: Integer;
    function  GetMaxEPLine: Integer;
    function  GetSelectedLine: Integer;
    function  LocateEP: Boolean;
    function  ToggleBP: Boolean;
    function  SelectLine(const ALineNo: Integer): Boolean;
    procedure DrawBPTs;
    procedure SaveLoadDimensions(const ASave: Boolean;const AState: Integer);
  end;

implementation

{$R *.dfm}

const
  prefixNormal : Char = ' ';
  prefixHERE : Char = '>';
  prefixBP_Enabled = '+';
  prefixBP_Disabled = '-';
  prefixPosEP = 1;
  prefixPosBP = 2;

procedure TfrmDebuggerWindow.BindActions(ActToggleBreakpoint,
  ActLocateExecPoint, ActRunToCursor, ActPause, ActCycle, ActSingleStep,
  ActStepNext, ActSetNewExecPoint: TAction);
begin
  cmdToggleBreakpoint.Action := ActToggleBreakpoint;
  mnuToggleBP.Action := ActToggleBreakpoint;
  cmdLocateExecPoint.Action := ActLocateExecPoint;
  cmdRunToCursor.Action := ActRunToCursor;
//  cmdPause.Action := ActPause;
  cmdCycle.Action := ActCycle;
//  cmdSingleStep.Action := ActSingleStep;
//  cmdStepNext.Action := ActStepNext;
  mnuSetNewEP.Action := ActSetNewExecPoint;


  cmdToggleBreakpoint.Caption := '';
  cmdLocateExecPoint.Caption := '';
  cmdRunToCursor.Caption := '';
//  cmdPause.Caption := '';
  cmdCycle.Caption := '';

  cmdCycle.Visible := ActCycle.Visible;

//  cmdSingleStep.Caption := '';
//  cmdStepNext.Caption := '';
end;


procedure TfrmDebuggerWindow.CycleLines;
begin
  FRawLines := not FRawLines;
  DrawLines(FRawLines);
  UpdateView(True);
end;

function TfrmDebuggerWindow.DrawLines(const ARawLines: Boolean): Boolean;
var
  i, curIdx: Integer;
begin
  if not Assigned(FScript) then
  begin
    Result := False;
    Exit;
  end;

  if ARawLines then
    FLines := FScript.ScriptGetRawLines
  else
    FLines := FScript.ScriptGetLines;

  lbScript.Items.BeginUpdate;

  curIdx := lbScript.ItemIndex;

  lbScript.Items.Clear;

  // Draw basic lines
  for i := 0 to FLines.Count-1 do
  begin
    lbScript.Items.Add(Format('  %05d: %s', [i+1, FLines.Strings[i]]));
  end;

  lbScript.Items.EndUpdate;

  lbScript.ItemIndex := curIdx;

  Result := True;
end;

procedure TfrmDebuggerWindow.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //
end;

procedure TfrmDebuggerWindow.FormCreate(Sender: TObject);
begin
  FScript := nil;
  FWorkspace := nil;

  FLastBPTs := TList.Create;

  FLastEP := 0;

  // Normal lines
  FRawLines := False;
end;

function TfrmDebuggerWindow.InitializeScripting(
  const AScript: TCompelScript;const AScriptFileName: string): Boolean;

var
  err: Integer;
begin
  // Assume failure
  Result := False;

  if (not Assigned(AScript)) or (Length(AScriptFileName) = 0) then
    Exit;

  // Save script file name
  FScriptFileName := AScriptFileName;

  // Save script object
  FScript := AScript;

  // Load the script
  err := FScript.ScriptLoad(FScriptFileName);
  if err <> compel_error_success  then
  begin
    MessageDlg(Format('Failed to load script!'#13#13+
    'ErrorNo: %d'#13+
    'Error line: %d'#13+
    'Error description: %s'#13,
    [err, FScript.ScriptGetPreparseFailingLine+1,
    FScript.GetErrorString(err)]), mtError, [mbOk], 0);
    Exit;
  end;

  // Initialize debugger
  if not FScript.ScriptInitRuntime then
  begin
    Exit;
  end;

  Result := True;
end;

procedure TfrmDebuggerWindow.SaveLoadWorkspace(const ASave: Boolean);
begin
  if not Assigned(FWorkspace) then
    Exit;

  if ASave then
  begin

  end else
  begin

  end;
end;

procedure TfrmDebuggerWindow.SetWorkspace(AWks: TCompelWorkspace);
begin
  FWorkspace := AWks;
end;

procedure TfrmDebuggerWindow.UpdateView(const AShow: Boolean);
begin
  // If lines were never drawn
  if (lbScript.Items.Count = 0) then
    DrawLines(FRawLines);

  DrawEP;
  DrawBPTs;
  if AShow then
  begin
    if (WindowState = wsMinimized ) then
      WindowState := wsNormal;

    Self.BringToFront;
  end;
end;

procedure TfrmDebuggerWindow.FormDestroy(Sender: TObject);
begin
  FLastBPTs.Free;
end;

procedure TfrmDebuggerWindow.DrawBPTs;
var
  i: Integer;
  bps: TCompelBreakpointList;
  bp: TCompelBreakpointItem;
begin
  lbScript.Items.BeginUpdate;

  // Clear all breakpoints indicators
  for i := 0 to FLastBPTs.Count-1 do
    DrawLinePrefix(prefixNormal, prefixPosBP, Integer(FLastBPTs[i]), False);

  // Get latest breakpoints list from COMPELscript
  bps := GetBreakpoints;
  if Assigned(bps) then
  begin
    bps.GetBptsLines(FLastBPTs);
  end;

  // Draw new breakpoints
  for i := 0 to bps.Count-1  do
  begin
    bp := bps[i];
    if (bp.Enabled) then
      DrawLinePrefix(prefixBP_Enabled, prefixPosBP, Integer(FLastBPTs[i]), False)
    else
      DrawLinePrefix(prefixBP_Disabled, prefixPosBP, Integer(FLastBPTs[i]), False)
  end;
  
  lbScript.Items.EndUpdate;
end;

procedure TfrmDebuggerWindow.DrawEP(const ASetEP: Boolean = False);
var
  ep: Integer;
begin
  // Get executing line no
  ep := FScript.ScriptGetLineNo;

  if ep <> FLastEP then
    DrawLinePrefix(prefixNormal, prefixPosEP, FLastEP);

  DrawLinePrefix(prefixHERE, prefixPosEP, ep);

  FLastEP := ep;

  if ASetEP then
    LocateEP;
end;

procedure TfrmDebuggerWindow.DrawLinePrefix(const APrefix: Char;
  const APrefixIndex, ALineNo: Integer;
  const ABeginUpdate: Boolean);
var
  s: string;
begin
  if lbScript.Items.Count <= ALineNo then
    Exit;

  with lbScript do
  begin
    if ABeginUpdate then
      Items.BeginUpdate;
    s := Items.Strings[ALineNo];
    s[APrefixIndex] := APrefix;
    Items.Strings[ALineNo] := s;
    if ABeginUpdate then
      Items.EndUpdate;
  end;
end;

function TfrmDebuggerWindow.GetEPLine: Integer;
begin
  if not Assigned(FScript) then
  begin
    Result := -1;
    Exit;
  end;
  Result := FScript.ScriptGetLineNo;
end;

function TfrmDebuggerWindow.GetSelectedLine: Integer;
begin
  Result := lbScript.ItemIndex;
end;

function TfrmDebuggerWindow.LocateEP: Boolean;
begin
  if not Assigned(FScript) then
  begin
    Result := False;
    Exit;
  end;
  lbScript.ItemIndex := GetEPLine;
  Result := True;
end;

function TfrmDebuggerWindow.GetMaxEPLine: Integer;
begin
  Result := lbScript.Count;
end;

function TfrmDebuggerWindow.ToggleBP: Boolean;
var
  bps: TCompelBreakpointList;
  Idx: Integer;
begin
  Result := False;
  bps := GetBreakpoints;
  if not Assigned(bps) then
    Exit;

  bps.ToggleBP(GetSelectedLine, Idx);

  Result := True;
  DrawBPTs;
end;

function TfrmDebuggerWindow.GetBreakpoints: TCompelBreakpointList;
begin
  if not Assigned(FScript) then
  begin
    Result := nil;
    Exit;
  end;
  Result := FScript.Breakpoints;
  if not Assigned(Result) then
    Exit;
end;

function TfrmDebuggerWindow.SelectLine(const ALineNo: Integer): Boolean;
begin
  if ALineNo >= lbScript.Count then
  begin
    Result := False;
    Exit;
  end;
  lbScript.ItemIndex := ALineNo;
  Result := True;
end;

procedure TfrmDebuggerWindow.lbScriptDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  frameIt : boolean;
  bIsHere, bIsBPT_Enabled, bIsBPT_Disabled: Boolean;
  s: string;
  lb: TListBox;
begin
  lb := (Control as TListBox);

  s := lb.Items[Index];

  bIsHere := s[1] = prefixHERE;
  bIsBPT_Enabled := s[2] = prefixBP_Enabled;
  bIsBPT_Disabled := s[2] = prefixBP_Disabled;

  with lb, Canvas do
  begin
    Font.Assign (tListBox (Control).Font);
    Font.Style := [];
    Brush.Color := tListBox (Control).Color;
    frameIt := False;
    if ([odInactive, odDisabled, odGrayed] * State) <> [] then
    begin
      font.Color := clGrayText
    end
    else begin
      frameIt := odSelected in State;
      if frameIt and Focused then
      begin
        Brush.Color := clHighlight;
        Font.Color  := clHighlightText;
      end;
    end;

    if bIsBPT_Enabled then
    begin
      Brush.Color := clRed;
      Font.Color := clWhite;
    end
    else if bIsBPT_Disabled then
    begin
      Brush.Color := clGreen;
      Font.Color := clWhite;
    end;

    if bIsHere then
    begin
      Brush.Color := clPurple;
      Font.Color := clWhite;
    end;

    FillRect (Rect);

    TextOut (Rect.Left, Rect.Top, s);

    if frameIt then
    begin
      if Focused then
      begin
        Pen.Style := psDot;
        Pen.Mode := pmCopy;
        Pen.Color := clWindowText;
        FrameRect (Rect);
      end
      else
        DrawFocusRect (Rect);
    end;
  end;
end;

procedure TfrmDebuggerWindow.actPatchCodeExecute(Sender: TObject);
var
  s: string;
  idx: Integer;
begin
  if not cmdLocateExecPoint.Enabled then
    Exit;

  idx := GetSelectedLine;

  s := FScript.ScriptGetLineCode(idx);

  if not InputQuery('Patch code', 'Enter new line of code', s) then
    Exit;

  PatchCodeLine(idx, s);
end;

procedure TfrmDebuggerWindow.PatchCodeLine(const AIndex: Integer;
  const ACode: string);
var
  i: Integer;
  s: string;

begin
  // Patch the script (in engine)
  FScript.ScriptSetLineCode(AIndex, ACode);

  // Patch RAW lines
  FScript.ScriptGetRawLines[AIndex] := ACode;

  // Patch script lines
  FScript.ScriptGetLines[AIndex] := ACode;

  // Patch in display
  s := lbScript.Items[AIndex];
  i := Pos(':', s);
  lbScript.Items[AIndex] := Copy(s, 1, i+1) + ACode;
end;

procedure TfrmDebuggerWindow.SaveLoadDimensions(const ASave: Boolean;
  const AState: Integer);
var
  dim: TCompelWorkspaceDimensions;
begin
  // Load dimensions
  if ASave = False then
  begin
    if not FWorkspace.LoadSaveDimensions(True, strDbgTag, AState, dim) then
      Exit;
    CompelIDEUtils.FormToWksDimension(Self, dim, True);
  end
  else begin
    CompelIDEUtils.FormToWksDimension(Self, dim, False);
    FWorkspace.LoadSaveDimensions(False, strDbgTag, AState, dim);
  end;
end;

end.
