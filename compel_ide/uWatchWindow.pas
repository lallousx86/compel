unit uWatchWindow;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Grids, ToolWin, StdCtrls,
  CompelScript, CompelWorkspace, CompelWatches, ActnList, Menus,
  CompelIDEConsts, CompelIDEUtils, ImgList;

type

  TfrmWatchWindow = class(TForm)
    lvWatches: TListView;
    ToolBar1: TToolBar;
    tbbAdd: TButton;
    tbbDelete: TButton;
    ActionList1: TActionList;
    actAdd: TAction;
    actDelete: TAction;
    PopupMenu1: TPopupMenu;
    mnuAdd: TMenuItem;
    mnuDelete: TMenuItem;
    actInspect: TAction;
    mnuInspect: TMenuItem;
    cmdInspect: TButton;
    actChangeValue: TAction;
    mnuChangeValue: TMenuItem;
    Button1: TButton;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actInspectExecute(Sender: TObject);
    procedure lvWatchesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure actChangeValueExecute(Sender: TObject);
  private
    FScript: TCompelScript;
    FWorkspace: TCompelWorkspace;
    FWatches: TCompelWatchList;
    procedure UpdateWatches;
    procedure LvAddWatchItem(const AVar, AVal: string);
    function  GetSelectedWatchInfo(var AVar, AVal: string): Boolean;
  public
    procedure SetScript(ACompelScript: TCompelScript);
    procedure SetWorkspace(AWks: TCompelWorkspace);
    procedure SaveLoadWorkspace(const ASave: Boolean = True);
    procedure UpdateView(const AShow: Boolean = False);
    procedure SaveLoadDimensions(const ASave: Boolean;const AState: Integer);    
  end;

implementation

{$R *.dfm}

procedure TfrmWatchWindow.SaveLoadWorkspace(const ASave: Boolean);
var
  i: Integer;
begin
  if not Assigned(FWorkspace) then
    Exit;

  if ASave then
  begin
    FWorkspace.Watches.Clear;
    for i := 0 to FWatches.Count - 1 do
      FWorkspace.Watches.Add(FWatches.GetName(i));

    FWorkspace.LoadSaveWatches(False);
  end
  else begin
    FWorkspace.LoadSaveWatches(True);
    for i := 0 to FWorkspace.Watches.Count - 1 do
      FWatches.AddItem(FWorkspace.Watches[i]);
  end;
end;

procedure TfrmWatchWindow.SetScript(ACompelScript: TCompelScript);
begin
  FScript := ACompelScript;
  FWatches.Script := FScript;
end;

procedure TfrmWatchWindow.SetWorkspace(AWks: TCompelWorkspace);
begin
  FWorkspace := AWks;
end;

procedure TfrmWatchWindow.FormCreate(Sender: TObject);
begin
  FWorkspace := nil;
  FScript := nil;
  FWatches := TCompelWatchList.Create(nil);
end;

procedure TfrmWatchWindow.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //
end;

procedure TfrmWatchWindow.UpdateView(const AShow: Boolean);
begin

  UpdateWatches;
  
  if AShow then
  begin
    if Self.WindowState = wsMinimized then
      Self.WindowState := wsNormal;
    Self.BringToFront;
  end;
end;

procedure TfrmWatchWindow.FormDestroy(Sender: TObject);
begin
  FWatches.Free;
end;

procedure TfrmWatchWindow.UpdateWatches;
var
  i: Integer;
  k, v: string;
begin
  with lvWatches do
  begin
    Items.BeginUpdate;
    lvWatches.Clear;
    for i := 0 to FWatches.Count -1 do
    begin
       if (not FWatches.GetItem(i, k, v)) then
         Continue;
       LvAddWatchItem(k, v);
    end;
    Items.EndUpdate;
  end;
end;

procedure TfrmWatchWindow.actAddExecute(Sender: TObject);
var
  s: string;
begin
  s := '$';
  if not InputQuery('Add watch', 'Enter variable name:', s) then
    Exit;
  FWatches.AddItem(s);
  UpdateWatches;
end;

procedure TfrmWatchWindow.actDeleteExecute(Sender: TObject);
var
  ListItem: TListItem;
begin
  ListItem := lvWatches.Selected;
  if not Assigned(ListItem) then
    Exit;
  FWatches.DeleteItem(ListItem.Caption);
  lvWatches.DeleteSelected;
end;

procedure TfrmWatchWindow.LvAddWatchItem(const AVar, AVal: string);
var
  ListItem: TListItem;
begin
   ListItem := lvWatches.Items.Add;
   ListItem.Caption := AVar;
   ListItem.SubItems.Add(AVal);
end;

procedure TfrmWatchWindow.actInspectExecute(Sender: TObject);
var
  f: TForm;
  m: TMemo;
  vVar, vVal: string;
begin
  if not GetSelectedWatchInfo(vVar, vVal) then
    Exit;

  f := TForm.Create(Self);
  m := TMemo.Create(f);

  m.Text := vVal;

  f.Width := 400;
  f.Height := 300;

  m.Align := alClient;
  m.ReadOnly := True;
  m.WordWrap := True;
  m.ScrollBars := ssVertical;
  f.InsertControl(m);
  f.Caption := 'Inspecting <' + vVar + '>';
  f.Position := poOwnerFormCenter;
  f.ShowModal;

  f.Free;
end;

procedure TfrmWatchWindow.lvWatchesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    actDeleteExecute(Sender);
  end
  else if Key = VK_INSERT then
  begin
    actAddExecute(Sender);
  end
  else if Key = VK_RETURN then
  begin
    actInspectExecute(Sender);
  end;
end;

function TfrmWatchWindow.GetSelectedWatchInfo(var AVar,
  AVal: string): Boolean;
var
  ListItem: TListItem;
  idx: Integer;
begin
  Result := False;

  ListItem := lvWatches.Selected;
  if not Assigned(ListItem) then
    Exit;

  idx := FWatches.FindIndex(ListItem.Caption);
  if idx = -1 then
    Exit;

  if not FWatches.GetItem(idx, AVar, AVal) then
    Exit;

  Result := True;
end;

procedure TfrmWatchWindow.actChangeValueExecute(Sender: TObject);
var
  vVar, vVal: string;
  Value: TCompelValue;

  ListItem: TListItem;
begin
  if not GetSelectedWatchInfo(vVar, vVal) then
    Exit;

  Value := TCompelValue.FindValue(FScript, vVar);
  if not Assigned(Value) then
    Exit;

  if not InputQuery('Change value', 'Enter new value:', vVal) then
    Exit;

  Value.Value := vVal;
  Value.Free;

  ListItem := lvWatches.Selected;
  ListItem.SubItems[0] := vVal;
end;

procedure TfrmWatchWindow.SaveLoadDimensions(const ASave: Boolean;
  const AState: Integer);
var
  dim: TCompelWorkspaceDimensions;
begin
  // Load dimensions
  if ASave = False then
  begin
    if not FWorkspace.LoadSaveDimensions(True, strWatchTag, AState, dim) then
      Exit;
    CompelIDEUtils.FormToWksDimension(Self, dim, True);
  end
  else begin
    CompelIDEUtils.FormToWksDimension(Self, dim, False);
    FWorkspace.LoadSaveDimensions(False, strWatchTag, AState, dim);
  end;
end;

end.
