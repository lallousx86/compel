object AboutBox: TAboutBox
  Left = 243
  Top = 108
  ActiveControl = OKButton
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 268
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 329
    Height = 252
    BevelOuter = bvLowered
    TabOrder = 0
    object ProgramIcon: TImage
      Left = 8
      Top = 14
      Width = 32
      Height = 32
      AutoSize = True
      Picture.Data = {
        055449636F6E0000010001002020100000000400E80200001600000028000000
        2000000040000000010004000000000000020000000000000000000010000000
        1000000000000000000080000080000000808000800000008000800080800000
        C0C0C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
        FFFFFF0000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000099000000
        0000000000000000099999999991000000000000000000009990000099999000
        0000000000000009900000000099991000000000000000990000000000000999
        0000000000000099000000000000001990000000000009900000000000000000
        9900000000000990000000000000000009900000000099900000000000000000
        0009000000009900000000000000000000009000000099000000000000000000
        0000000000009900000000000000000000000000000099000000000000000000
        0000000000009900000000000000000000000000000099900000000000000000
        0000000000009990000000000000000000000000000099900000000000000000
        0000000000000999000000000000000000000000000009999000000000000000
        0000000000000099990000000000000000000000000000999990000000000000
        0000000000000009999900000000000000000000000000009999990000000000
        0000000000000000099909999990000000000000000000000019900000000000
        0000000000000000000099100000000000000000000000000000009990000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000}
      IsControl = True
    end
    object ProductName: TLabel
      Left = 46
      Top = 5
      Width = 65
      Height = 13
      Caption = 'COMPEL IDE'
      IsControl = True
    end
    object Version: TLabel
      Left = 46
      Top = 43
      Width = 59
      Height = 13
      Caption = 'Version 0.31'
      IsControl = True
    end
    object Copyright: TLabel
      Left = 46
      Top = 24
      Width = 238
      Height = 13
      Caption = 'Copyright (c) 2006 <elias.bachaalany@gmail.com>'
      IsControl = True
    end
    object Shape1: TShape
      Left = 64
      Top = 70
      Width = 193
      Height = 1
      Shape = stRoundRect
    end
    object Label1: TLabel
      Left = 8
      Top = 88
      Width = 85
      Height = 13
      Caption = 'Special thanks to:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 16
      Top = 107
      Width = 3
      Height = 13
      OnClick = Label2Click
    end
    object Memo1: TMemo
      Left = 8
      Top = 107
      Width = 313
      Height = 102
      Color = clInactiveBorder
      Lines.Strings = (
        'SynEdit team (http://synedit.sourceforge.net)'
        'Borland Delphi Team (http://www.borland.com/delphi)'
        'Mr. K. N for the COMPEL logo'
        'Mr. I.G for his inspiration and technical assistance')
      ReadOnly = True
      TabOrder = 0
    end
  end
  object OKButton: TButton
    Left = 138
    Top = 223
    Width = 65
    Height = 33
    Caption = 'OK'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clPurple
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 1
    IsControl = True
  end
end
