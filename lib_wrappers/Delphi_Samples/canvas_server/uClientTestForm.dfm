object Form1: TForm1
  Left = 192
  Top = 107
  Width = 783
  Height = 540
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 664
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Serialize 2dtxt'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 664
    Top = 40
    Width = 89
    Height = 25
    Caption = 'Deserialize 2dtxt'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 664
    Top = 96
    Width = 89
    Height = 25
    Caption = 'Serialize 4pt R'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 664
    Top = 128
    Width = 89
    Height = 25
    Caption = 'Deserialize 4pt R'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 664
    Top = 184
    Width = 89
    Height = 25
    Caption = 'Ser 2d img'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 664
    Top = 216
    Width = 89
    Height = 25
    Caption = 'Deser 2d img'
    TabOrder = 5
    OnClick = Button6Click
  end
end
