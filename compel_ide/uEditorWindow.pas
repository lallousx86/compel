unit uEditorWindow;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CompelScript, CompelWorkspace,
  CompelIDEConsts, CompelIDEUtils, SynEdit;

type
  TfrmEditorWindow = class(TForm)
    memoLines: TSynEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    FFileName: string;
    FScript: TCompelScript;
    FWorkspace: TCompelWorkspace;
    FActivateOnce: Boolean;
    FNewNumber: Integer;
    FCurDirFollowDocument: Boolean;
    fSearchFromCaret: boolean;

    procedure OnActivateOnce(Sender: TObject);
  public
    function  OpenFile(const AFileName: string): Boolean;
    function  OpenNewFile: Boolean;
    procedure UpdateView(const AShow: Boolean = False);
    procedure SetScript(ACompelScript: TCompelScript);
    procedure SetWorkspace(AWks: TCompelWorkspace);
    procedure SaveLoadWorkspace(const ASave: Boolean = True);
    function  SaveFile: Boolean;
    function  IsModified: Boolean;
    procedure MarkAsModified;
    function  GetLineNo: Integer;
    procedure GotoLineNo(ALineNo: Integer);
    procedure SaveLoadDimensions(const ASave: Boolean;const AState: Integer);
    procedure ShowSearchReplaceDialog(AReplace: boolean);
    procedure DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);

  published
    property FileName: string read FFileName write FFileName;
    property NewNumber: Integer read FNewNumber;
    property CurDirFollowDocument: Boolean read FCurDirFollowDocument write FCurDirFollowDocument;
    property SynEditor: TSynEdit read memoLines;
  end;

var
  frmEditorWindow: TfrmEditorWindow;

implementation

{$R *.dfm}

uses dlgSearchText, dlgReplaceText, dlgConfirmReplace;

  // options - to be saved to the registry
var
  gbSearchBackwards: boolean;
  gbSearchCaseSensitive: boolean;
  gbSearchFromCaret: boolean;
  gbSearchSelectionOnly: boolean;
  gbSearchTextAtCaret: boolean;
  gbSearchWholeWords: boolean;

  gsSearchText: string;
  gsSearchTextHistory: string;
  gsReplaceText: string;
  gsReplaceTextHistory: string;

var
  g_new_count: Integer = 1;

procedure TfrmEditorWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //
end;

function TfrmEditorWindow.OpenFile(const AFileName: string): Boolean;
var
  s: string;
begin
  FFileName := AFileName;
  Self.Caption := FFileName;

  try
    memoLines.Lines.LoadFromFile(AFileName);
    if CurDirFollowDocument then
    begin
      s := ExtractFilePath(AFileName);
      if Length(s) <> 0 then
        ChDir(s);
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function TfrmEditorWindow.OpenNewFile: Boolean;
begin
  FFileName := '';
  Self.Caption := Format('<untitled %d>', [g_new_count]);
  FNewNumber := g_new_count;
  Inc(g_new_count);

  try
    memoLines.Lines.Add('# Welcome to COMPEL');
    memoLines.Lines.Add('end');
    Result := True;
  except
    Result := False;
  end;
end;

procedure TfrmEditorWindow.SaveLoadWorkspace(const ASave: Boolean);
begin
  if not Assigned(FWorkspace) then
    Exit;

  if ASave then
  begin
  end
  else begin
  end;
end;

procedure TfrmEditorWindow.SetScript(ACompelScript: TCompelScript);
begin
  FScript := ACompelScript;
end;

procedure TfrmEditorWindow.SetWorkspace(AWks: TCompelWorkspace);
begin
  FWorkspace := AWks;
end;

procedure TfrmEditorWindow.UpdateView(const AShow: Boolean);
begin
  if AShow then
  begin
    Self.WindowState := wsNormal;
    Self.Show;
  end;
end;

procedure TfrmEditorWindow.FormCreate(Sender: TObject);
begin
  FWorkspace := nil;
  FScript := nil;
  FActivateOnce := False;
  CurDirFollowDocument := True;
end;

function TfrmEditorWindow.SaveFile: Boolean;
begin
  Result := False;
  if Length(FFileName) = 0 then
    Exit;
  try
    memoLines.Lines.SaveToFile(FFileName);
    memoLines.Modified := False;
    Self.Caption := FFileName;
    Self.Caption := FFileName;
    Result := True;
  except
  end;
end;

function TfrmEditorWindow.IsModified: Boolean;
begin
  Result := memoLines.Modified;
end;

procedure TfrmEditorWindow.MarkAsModified;
begin
  memoLines.Modified := True;
end;

function TfrmEditorWindow.GetLineNo: Integer;
begin
{$IFNDEF COMPEL_MEMOEDIT}
  Result := memoLines.CaretY;
  if not memoLines.Gutter.ZeroStart then
    Dec(Result);
{$ELSE}
  Result := SendMessage(memoLines.Handle, EM_LINEFROMCHAR, memoLines.Selstart, 0);
{$ENDIF}
end;

procedure TfrmEditorWindow.GotoLineNo(ALineNo: Integer);
begin
{$IFNDEF COMPEL_MEMOEDIT}
  if not memoLines.Gutter.ZeroStart then
    Inc(ALineNo);
  memoLines.CaretY := ALineNo;
{$ELSE}
  memoLines.Perform(EM_LINESCROLL, 0, ALineNo);
  memoLines.SelStart := memoLines.Perform(EM_LINEINDEX, ALineNo, 0);
  memoLines.SelLength := 0;
  memoLines.SetFocus;
{$ENDIF}
end;

procedure TfrmEditorWindow.FormActivate(Sender: TObject);
begin
  if FActivateOnce = False then
    OnActivateOnce(Sender);
  FActivateOnce := True;
end;

procedure TfrmEditorWindow.OnActivateOnce(Sender: TObject);
begin
  //
end;

procedure TfrmEditorWindow.SaveLoadDimensions(const ASave: Boolean;
  const AState: Integer);
var
  dim: TCompelWorkspaceDimensions;
begin
  // Load dimensions
  if ASave = False then
  begin
    if not FWorkspace.LoadSaveDimensions(True, strEdtTag, AState, dim) then
      Exit;
    CompelIDEUtils.FormToWksDimension(Self, dim, True);
  end
  else begin
    CompelIDEUtils.FormToWksDimension(Self, dim, False);
    FWorkspace.LoadSaveDimensions(False, strEdtTag, AState, dim);
  end;
end;


procedure TfrmEditorWindow.DoSearchReplaceText(AReplace,
  ABackwards: boolean);
var
  Options: TSynSearchOptions;
begin
  if AReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];

  if ABackwards then
    Include(Options, ssoBackwards);

  if gbSearchCaseSensitive then
    Include(Options, ssoMatchCase);

  if not fSearchFromCaret then
    Include(Options, ssoEntireScope);

  if gbSearchSelectionOnly then
    Include(Options, ssoSelectedOnly);

  if gbSearchWholeWords then
    Include(Options, ssoWholeWord);

  if SynEditor.SearchReplace(gsSearchText, gsReplaceText, Options) = 0 then
  begin
    ShowMessage(Format('"%s" not found', [gsSearchText]));
    if ssoBackwards in Options then
      SynEditor.BlockEnd := SynEditor.BlockBegin
    else
      SynEditor.BlockBegin := SynEditor.BlockEnd;
    SynEditor.CaretXY := SynEditor.BlockBegin;
  end;

  if ConfirmReplaceDialog <> nil then
    ConfirmReplaceDialog.Free;
end;

procedure TfrmEditorWindow.ShowSearchReplaceDialog(AReplace: boolean);
var
  dlg: TTextSearchDialog;
begin
  if AReplace then
    dlg := TTextReplaceDialog.Create(Self)
  else
    dlg := TTextSearchDialog.Create(Self);
  with dlg do try
    // assign search options
    SearchBackwards := gbSearchBackwards;
    SearchCaseSensitive := gbSearchCaseSensitive;
    SearchFromCursor := gbSearchFromCaret;
    SearchInSelectionOnly := gbSearchSelectionOnly;
    // start with last search text
    SearchText := gsSearchText;
    if gbSearchTextAtCaret then begin
      // if something is selected search for that text
      if SynEditor.SelAvail and (SynEditor.BlockBegin.Y = SynEditor.BlockEnd.Y)
      then
        SearchText := SynEditor.SelText
      else
        SearchText := SynEditor.GetWordAtRowCol(SynEditor.CaretXY);
    end;
    SearchTextHistory := gsSearchTextHistory;
    if AReplace then with dlg as TTextReplaceDialog do
    begin
        ReplaceText := gsReplaceText;
        ReplaceTextHistory := gsReplaceTextHistory;
    end;
    SearchWholeWords := gbSearchWholeWords;
    if ShowModal = mrOK then
    begin
      gbSearchBackwards := SearchBackwards;
      gbSearchCaseSensitive := SearchCaseSensitive;
      gbSearchFromCaret := SearchFromCursor;
      gbSearchSelectionOnly := SearchInSelectionOnly;
      gbSearchWholeWords := SearchWholeWords;
      gsSearchText := SearchText;
      gsSearchTextHistory := SearchTextHistory;
      if AReplace then with dlg as TTextReplaceDialog do
      begin
        gsReplaceText := ReplaceText;
        gsReplaceTextHistory := ReplaceTextHistory;
      end;
      fSearchFromCaret := gbSearchFromCaret;
      if gsSearchText <> '' then
      begin
        DoSearchReplaceText(AReplace, gbSearchBackwards);
        fSearchFromCaret := TRUE;
      end;
    end;
  finally
    dlg.Free;
  end;
end;

end.
