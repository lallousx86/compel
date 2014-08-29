unit uAbout;

interface

uses Windows, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ShellAPI;

type
  TAboutBox = class(TForm)
    Panel1: TPanel;
    OKButton: TButton;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    Shape1: TShape;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    procedure Label2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

procedure TAboutBox.Label2Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'http://synedit.sourceforge.net', nil, nil, SW_SHOW);
end;

end.

