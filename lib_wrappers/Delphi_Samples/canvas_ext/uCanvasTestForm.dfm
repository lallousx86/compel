object frmCanvasTest: TfrmCanvasTest
  Left = 0
  Top = 101
  Caption = 'CanvasExt - test'
  ClientHeight = 362
  ClientWidth = 464
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object cmdCreate: TButton
    Left = 8
    Top = 128
    Width = 75
    Height = 25
    Caption = '&Reset'
    TabOrder = 0
    OnClick = cmdCreateClick
  end
  object cmdCircle: TButton
    Left = 96
    Top = 190
    Width = 75
    Height = 25
    Caption = 'Circle'
    TabOrder = 1
    OnClick = cmdCircleClick
  end
  object Button1: TButton
    Left = 8
    Top = 159
    Width = 75
    Height = 25
    Caption = 'Rect'
    TabOrder = 2
    OnClick = Button1Click
  end
  object lbledtParameters: TLabeledEdit
    Left = 8
    Top = 24
    Width = 201
    Height = 21
    EditLabel.Width = 55
    EditLabel.Height = 13
    EditLabel.Caption = 'Parameters'
    TabOrder = 3
    Text = '2,2,10,30'
  end
  object Button2: TButton
    Left = 96
    Top = 159
    Width = 75
    Height = 25
    Caption = 'Ellipse'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 190
    Width = 75
    Height = 25
    Caption = 'Square'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 96
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Line'
    TabOrder = 6
    OnClick = Button4Click
  end
  object edtBrushColor: TLabeledEdit
    Tag = 1
    Left = 215
    Top = 24
    Width = 82
    Height = 21
    EditLabel.Width = 55
    EditLabel.Height = 13
    EditLabel.Caption = 'Brush Color'
    TabOrder = 7
    Text = '$0000FF'
    OnChange = edtBrushColorChange
    OnDblClick = edtBrushColorDblClick
  end
  object edtPenColor: TLabeledEdit
    Tag = 2
    Left = 303
    Top = 24
    Width = 82
    Height = 21
    EditLabel.Width = 46
    EditLabel.Height = 13
    EditLabel.Caption = 'Pen Color'
    TabOrder = 8
    Text = '$00FF00'
    OnChange = edtBrushColorChange
    OnDblClick = edtBrushColorDblClick
  end
  object Button5: TButton
    Left = 8
    Top = 221
    Width = 75
    Height = 25
    Caption = 'FRect'
    TabOrder = 9
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 96
    Top = 221
    Width = 75
    Height = 25
    Caption = 'Text'
    TabOrder = 10
    OnClick = Button6Click
  end
  object edtText: TLabeledEdit
    Left = 8
    Top = 72
    Width = 201
    Height = 21
    EditLabel.Width = 22
    EditLabel.Height = 13
    EditLabel.Caption = 'Text'
    TabOrder = 11
    Text = 'This is a text'
    OnDblClick = edtTextDblClick
  end
  object Button7: TButton
    Left = 8
    Top = 252
    Width = 75
    Height = 25
    Caption = 'Clear Canvas'
    TabOrder = 12
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 96
    Top = 252
    Width = 75
    Height = 25
    Caption = 'Delete shape'
    TabOrder = 13
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 8
    Top = 283
    Width = 75
    Height = 25
    Caption = 'Set title'
    TabOrder = 14
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 96
    Top = 283
    Width = 75
    Height = 25
    Caption = 'Set XY'
    TabOrder = 15
    OnClick = Button10Click
  end
  object Button11: TButton
    Left = 8
    Top = 314
    Width = 75
    Height = 25
    Caption = 'Load bitmap'
    TabOrder = 16
    OnClick = Button11Click
  end
  object Button12: TButton
    Left = 96
    Top = 314
    Width = 75
    Height = 25
    Caption = 'OnTop'
    TabOrder = 17
    OnClick = Button12Click
  end
  object Button13: TButton
    Left = 177
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Show/hide'
    TabOrder = 18
    OnClick = Button13Click
  end
  object Button14: TButton
    Left = 177
    Top = 159
    Width = 75
    Height = 25
    Caption = 'Save bitmap'
    TabOrder = 19
    OnClick = Button14Click
  end
  object Button15: TButton
    Left = 177
    Top = 190
    Width = 75
    Height = 25
    Caption = 'Allow close'
    TabOrder = 20
    OnClick = Button15Click
  end
  object ColorDialog1: TColorDialog
    Left = 392
    Top = 16
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 224
    Top = 64
  end
end
