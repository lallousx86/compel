object frmRunExternally: TfrmRunExternally
  Left = 192
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Run Externally'
  ClientHeight = 433
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000088888888888888888888888888880000000
    00000000000000000000000080000FF7F7F7F7F7F7F7F7F7F7F7F7F080000F7F
    7F7F7F7F888888888888888088880FF7F7F7F7F0000000000000000000080F7F
    7F7F7FF0FFFFFFFFFFFFFFFFFF080FF7F7F7F7F0FFFF700000F700000F080F7F
    7F7F7FF0FFFF7FFFF0F7FFFF0F080FF7F7F7F7F0FFFF777777F777777F080F7F
    7F7F7FF0FFFFFFFFFFFFFFFFFF080FF7F7F7F7F0FFFFFFFFFFFFFFFFFF080F7F
    7F7F7FF0FFFFFFFFFFFFFFFFFF080FF7F7F7F7F0FFFFFFFFFFFFFFFFFF080F7F
    7F7F7FF0000000000000000000080FF7F7F7F7F0CCCCCCCCCCCC070707080F7F
    7F7F7FF0000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFF080000000
    00000000000000000000000080000F7F7F7F7F7F7F7F7F7F7F7F7F7080000F00
    07F000F70007F7F7F7F7F7F080000FFFFFFFFFFFFFFFFFFFFFFFFFF080000000
    00000000000000000000000080000CCCCCCCCCCCCCCCCCCCC070707080000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    FFFFFFFFFFFFFFFFFFFF80000007000000070000000700000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000100000007000000070000000700000007000000070000
    0007000000070000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblScript: TLabeledEdit
    Left = 8
    Top = 24
    Width = 417
    Height = 21
    EditLabel.Width = 43
    EditLabel.Height = 13
    EditLabel.Caption = 'Script file'
    TabOrder = 0
  end
  object lblParams: TLabeledEdit
    Left = 8
    Top = 72
    Width = 417
    Height = 21
    EditLabel.Width = 53
    EditLabel.Height = 13
    EditLabel.Caption = 'Parameters'
    TabOrder = 1
  end
  object cmdBrowse: TButton
    Left = 8
    Top = 104
    Width = 75
    Height = 25
    Caption = '&Browse'
    TabOrder = 2
    OnClick = cmdBrowseClick
  end
  object cmdRun: TButton
    Left = 88
    Top = 104
    Width = 75
    Height = 25
    Caption = '&Run'
    TabOrder = 3
    OnClick = cmdRunClick
  end
  object cmdClose: TButton
    Left = 176
    Top = 104
    Width = 75
    Height = 25
    Caption = '&Close'
    TabOrder = 4
    OnClick = cmdCloseClick
  end
  object cmdEdit: TButton
    Left = 262
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Not&epad'
    TabOrder = 5
    OnClick = cmdEditClick
  end
  object edtStartDir: TLabeledEdit
    Left = 8
    Top = 168
    Width = 417
    Height = 21
    EditLabel.Width = 68
    EditLabel.Height = 13
    EditLabel.Caption = 'Start directory:'
    TabOrder = 6
  end
  object cmdDetails: TButton
    Left = 350
    Top = 104
    Width = 75
    Height = 25
    Caption = '&Options >>>'
    TabOrder = 7
    OnClick = cmdDetailsClick
  end
  object edtRawFile: TLabeledEdit
    Left = 8
    Top = 211
    Width = 417
    Height = 21
    EditLabel.Width = 41
    EditLabel.Height = 13
    EditLabel.Caption = 'Raw file:'
    TabOrder = 8
  end
  object LabeledEdit2: TLabeledEdit
    Left = 8
    Top = 255
    Width = 417
    Height = 21
    EditLabel.Width = 92
    EditLabel.Height = 13
    EditLabel.Caption = 'Dump symbol table:'
    TabOrder = 9
  end
  object chkNamespace: TCheckBox
    Left = 8
    Top = 376
    Width = 305
    Height = 17
    Caption = 'Register commands with their full &namespace naming'
    TabOrder = 10
  end
  object chkShell: TCheckBox
    Left = 8
    Top = 399
    Width = 243
    Height = 17
    Caption = 'Enable &shell commands support'
    TabOrder = 11
  end
  object chkDbgOut: TCheckBox
    Left = 8
    Top = 353
    Width = 329
    Height = 17
    Caption = 'Debug &outs the script lines and internal processing'
    TabOrder = 12
  end
  object chkInteractive: TCheckBox
    Left = 8
    Top = 282
    Width = 113
    Height = 17
    Caption = 'Interactive mode:'
    Checked = True
    State = cbChecked
    TabOrder = 13
    OnClick = chkInteractiveClick
  end
  object edtInteractive: TEdit
    Left = 125
    Top = 280
    Width = 300
    Height = 21
    TabOrder = 14
  end
end
