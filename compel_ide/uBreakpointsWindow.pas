unit uBreakpointsWindow;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Grids, ToolWin, StdCtrls,
  CompelScript, CompelWorkspace, ActnList,
  CompelIDEConsts, CompelIDEUtils;

type
  TfrmBreakpointsWindow = class(TForm)
    lvBreakpoints: TListView;
    tbBpt: TToolBar;
    Button2: TButton;
    actlistBreakpoints: TActionList;
    actDelete: TAction;
    actToggleEnable: TAction;
    Button3: TButton;
    actDelAll: TAction;
    Button1: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actToggleEnableExecute(Sender: TObject);
    procedure lvBreakpointsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvBreakpointsCompare(Sender: TObject; Item1,
      Item2: TListItem; Data: Integer; var Compare: Integer);

    function GetSelectedBPItem: TCompelBreakpointItem;
    procedure actDelAllExecute(Sender: TObject);
  private
    FScript: TCompelScript;
    FWorkspace: TCompelWorkspace;
    FBpts: TCompelBreakpointList;

    procedure RefreshIDE;
  public
    procedure UpdateView(const AShow: Boolean = False);
    procedure SetScript(ACompelScript: TCompelScript);
    procedure SetWorkspace(AWks: TCompelWorkspace);
    procedure SaveLoadWorkspace(const ASave: Boolean = True);
    procedure UpdateBreakpoints;
    procedure ResetHitCount;
    procedure SaveLoadDimensions(const ASave: Boolean;const AState: Integer);    
  end;

implementation

const
  boolStr: array[Boolean] of string = ('No', 'Yes');

{$R *.dfm}

procedure TfrmBreakpointsWindow.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //
end;

procedure TfrmBreakpointsWindow.SaveLoadWorkspace(const ASave: Boolean);
var
  i: Integer;
  Info: TCompelBreakpointInfo;
begin
  if ASave then
  begin
    FWorkspace.Breakpoints.Clear;
    for i := 0 to FBpts.Count - 1 do
    begin
      FBpts.GetBPInfo(i, Info);
      FWorkspace.Breakpoints.Add(Info);
    end;
    FWorkspace.LoadSaveBreakpoints(False);
  end else
  // Load breakpoints
  begin
    FBpts.Clear;
    FWorkspace.LoadSaveBreakpoints(True);
    for i := 0 to FWorkspace.Breakpoints.Count - 1  do
    begin
      Info := FWorkspace.Breakpoints[i];
      FBpts.AddBP(Info.LineNo, bpNormal, Info.Enabled);
    end;
  end;
end;

procedure TfrmBreakpointsWindow.SetScript(ACompelScript: TCompelScript);
begin
  FScript := ACompelScript;
  if not Assigned(FScript) then
    Exit;
  FScript.Breakpoints := FBpts;
end;

procedure TfrmBreakpointsWindow.SetWorkspace(AWks: TCompelWorkspace);
begin
  FWorkspace := AWks;
end;

procedure TfrmBreakpointsWindow.UpdateView(const AShow: Boolean);
begin
  UpdateBreakpoints;
  if AShow then
  begin
    if Self.WindowState = wsMinimized then
      Self.WindowState := wsNormal;
    Self.BringToFront;
  end;
end;

procedure TfrmBreakpointsWindow.FormCreate(Sender: TObject);
begin
  //
  FScript := nil;
  FWorkspace := nil;
  FBpts := TCompelBreakpointList.Create;
end;

procedure TfrmBreakpointsWindow.FormDestroy(Sender: TObject);
begin
  FBpts.Free;
end;

procedure TfrmBreakpointsWindow.actDeleteExecute(Sender: TObject);
var
  ListItem: TListItem;
  Idx: Integer;
begin
  ListItem := lvBreakpoints.Selected;
  if not Assigned(ListItem) then
    Exit;
  Idx := FBpts.FindBpIndex(Integer(ListItem.Data));
  if Idx = -1 then
    Exit;
  FBpts.Delete(Idx);
  lvBreakpoints.DeleteSelected;

  RefreshIDE;
end;

procedure TfrmBreakpointsWindow.actToggleEnableExecute(Sender: TObject);
var
  ListItem: TListItem;
  Idx: Integer;
  b: Boolean;
begin
  ListItem := lvBreakpoints.Selected;
  if not Assigned(ListItem) then
    Exit;

  Idx := FBpts.FindBpIndex(Integer(ListItem.Data));
  if Idx = -1 then
    Exit;

  b := not FBpts[Idx].Enabled;
  FBpts[Idx].Enabled := b;

  ListItem.SubItems[0] := boolStr[b];

  RefreshIDE;
end;

procedure TfrmBreakpointsWindow.UpdateBreakpoints;
var
  i: Integer;
  ListItem: TListItem;
  Item: TCompelBreakpointItem;
begin
  lvBreakpoints.Items.BeginUpdate;
  lvBreakpoints.Clear;
  for i := 0 to FBpts.Count - 1 do
  begin
    Item := FBpts[i];
    ListItem := lvBreakpoints.Items.Add;
    ListItem.Data := Pointer(Item.GetLineNo); // Associate the data with the BP line number
    ListItem.Caption := IntToStr(Item.GetLineNo+1);
    ListItem.SubItems.Add(boolStr[Item.Enabled]);
    ListItem.SubItems.Add(IntToStr(Item.Hits));
  end;
  lvBreakpoints.Items.EndUpdate;
end;

procedure TfrmBreakpointsWindow.lvBreakpointsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    actDeleteExecute(Sender);
    Key := 0;
  end
  else if Key = VK_F5 then
  begin
    UpdateBreakpoints;
  end;
end;

procedure TfrmBreakpointsWindow.lvBreakpointsCompare(Sender: TObject;
  Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare := Integer(Item1.Data) - Integer(Item2.Data);
end;

function TfrmBreakpointsWindow.GetSelectedBPItem: TCompelBreakpointItem;
var
  ListItem: TListItem;
begin
  Result := nil;
  ListItem := lvBreakpoints.Selected;
  if not Assigned(ListItem) then
    Exit;
  Result := FBpts.FindBpItem(Integer(ListItem.Data));
end;

procedure TfrmBreakpointsWindow.actDelAllExecute(Sender: TObject);
begin
  FBpts.Clear;
  UpdateBreakpoints;
  RefreshIDE;
end;

procedure TfrmBreakpointsWindow.RefreshIDE;
begin
  PostMessage((Self.Owner as TForm).Handle, WM_IDE_REFRESH, ideconstsRedrawBreakpoints, 0);
end;

procedure TfrmBreakpointsWindow.SaveLoadDimensions(const ASave: Boolean;
  const AState: Integer);
var
  dim: TCompelWorkspaceDimensions;
begin
  // Load dimensions
  if ASave = False then
  begin
    if not FWorkspace.LoadSaveDimensions(True, strBptTag, AState, dim) then
      Exit;
    CompelIDEUtils.FormToWksDimension(Self, dim, True);
  end
  else begin
    CompelIDEUtils.FormToWksDimension(Self, dim, False);
    FWorkspace.LoadSaveDimensions(False, strBptTag, AState, dim);
  end;
end;

procedure TfrmBreakpointsWindow.ResetHitCount;
var
  i: Integer;
  Item: TCompelBreakpointItem;
begin
  for i := 0 to FBpts.Count - 1 do
  begin
    Item := FBpts[i];
    Item.Hits := 0;
  end;
end;

end.
