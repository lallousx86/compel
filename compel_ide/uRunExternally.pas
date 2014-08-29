unit uRunExternally;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, uCompelIDE, CompelUtil;

type
  TfrmRunExternally = class(TForm)
    lblScript: TLabeledEdit;
    lblParams: TLabeledEdit;
    cmdBrowse: TButton;
    cmdRun: TButton;
    cmdClose: TButton;
    cmdEdit: TButton;
    edtStartDir: TLabeledEdit;
    cmdDetails: TButton;
    edtRawFile: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    chkNamespace: TCheckBox;
    chkShell: TCheckBox;
    chkDbgOut: TCheckBox;
    chkInteractive: TCheckBox;
    edtInteractive: TEdit;
    procedure cmdBrowseClick(Sender: TObject);
    procedure cmdRunClick(Sender: TObject);
    procedure cmdCloseClick(Sender: TObject);
    procedure cmdEditClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmdDetailsClick(Sender: TObject);
    procedure chkInteractiveClick(Sender: TObject);
  private
    { Private declarations }
    FIDE: TfrmCompelIDE;

    FbActivated: Boolean;
    procedure OnActivateOnce(Sender: TObject);
    procedure ShowDetails(ADetails: Boolean; const AToggle: Boolean = False);
  public
    class function Go(Form: TfrmCompelIDE): Integer;
  end;

var
  frmRunExternally: TfrmRunExternally;

implementation

{$R *.dfm}

{ TfrmRunExternally }

class function TfrmRunExternally.Go(Form: TfrmCompelIDE): Integer;
var
  frm: TfrmRunExternally;
begin
  frm := TfrmRunExternally.Create(Form);
  frm.FIDE := Form;
  frm.ShowModal;
  Result := Integer(frm.ModalResult);
  frm.Free;
end;

procedure TfrmRunExternally.OnActivateOnce(Sender: TObject);
begin
  ShowDetails(False);
end;

procedure TfrmRunExternally.ShowDetails(ADetails: Boolean; const AToggle: Boolean);
begin
  if AToggle then
    ADetails := not Boolean(cmdDetails.Tag);
    
  if ADetails then
  begin
    Self.Height := 460;
    cmdDetails.Tag := 1;
    cmdDetails.Caption := '&Options <<<';    
  end
  else begin
    Self.Height := 160;
    cmdDetails.Tag := 0;
    cmdDetails.Caption := '&Options >>>';
  end;
end;

procedure TfrmRunExternally.chkInteractiveClick(Sender: TObject);
begin
  edtInteractive.Visible := chkInteractive.Checked;
end;

procedure TfrmRunExternally.cmdBrowseClick(Sender: TObject);
begin
  if not FIDE.dlgOpenDialog.Execute then
    Exit;

  lblScript.Text := FIDE.dlgOpenDialog.FileName;
  edtStartDir.Text := ExtractFilePath(lblScript.Text);
end;

procedure TfrmRunExternally.cmdRunClick(Sender: TObject);
var
  path: string;
  L: TStringList;
begin
  path := FIDE.GetCCompelToolPath;

  if not FileExists(path) then
  begin
    ShowMessage('Could not locate ccompel.exe tool!');
  end;
  L := TStringList.Create;
  L.Add('@echo off');
  L.Add(Format('%s %s -args "%s" -cwd "%s"',
    [path,
    lblScript.Text,
    lblParams.Text,
    edtStartDir.Text]));
  L.Add('pause');
  path := ComputeTempFileName('compel') + '_compel_temp.bat';
  L.SaveToFile(path);

  WinExec(PChar(SysUtils.GetEnvironmentVariable('COMSPEC') + ' /C ' + path), SW_SHOW);

  Windows.Sleep(2000);

  SysUtils.DeleteFile(path);
  L.Free;
end;

procedure TfrmRunExternally.FormActivate(Sender: TObject);
begin
  if FbActivated = False then
  begin
    FbActivated := True;
    OnActivateOnce(Sender);
  end;
end;

procedure TfrmRunExternally.FormCreate(Sender: TObject);
begin
  FbActivated := False;
end;

procedure TfrmRunExternally.cmdCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmRunExternally.cmdDetailsClick(Sender: TObject);
begin
  ShowDetails(False, True);
end;

procedure TfrmRunExternally.cmdEditClick(Sender: TObject);
begin
  WinExec(PChar('notepad '+lblScript.Text), SW_SHOW);
end;

end.
