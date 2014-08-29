object frmBreakpointsWindow: TfrmBreakpointsWindow
  Left = 311
  Top = 301
  Width = 388
  Height = 254
  Caption = 'Breakpoint Window'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lvBreakpoints: TListView
    Left = 0
    Top = 0
    Width = 380
    Height = 198
    Align = alClient
    Columns = <
      item
        Caption = 'Line'
      end
      item
        Caption = 'Enabled'
        Width = 60
      end
      item
        Caption = 'Hits'
      end>
    ReadOnly = True
    RowSelect = True
    SortType = stData
    TabOrder = 0
    ViewStyle = vsReport
    OnCompare = lvBreakpointsCompare
    OnKeyDown = lvBreakpointsKeyDown
  end
  object tbBpt: TToolBar
    Left = 0
    Top = 198
    Width = 380
    Height = 29
    Align = alBottom
    Caption = 'tbBpt'
    TabOrder = 1
    object Button3: TButton
      Left = 0
      Top = 2
      Width = 48
      Height = 22
      Action = actDelete
      Caption = 'DEL'
      TabOrder = 1
    end
    object Button2: TButton
      Left = 48
      Top = 2
      Width = 65
      Height = 22
      Action = actToggleEnable
      Caption = 'TOGGLE'
      TabOrder = 0
    end
    object Button1: TButton
      Left = 113
      Top = 2
      Width = 65
      Height = 22
      Action = actDelAll
      Caption = 'DEL ALL'
      TabOrder = 2
    end
  end
  object actlistBreakpoints: TActionList
    Left = 344
    Top = 160
    object actDelete: TAction
      Caption = 'actDelete'
      OnExecute = actDeleteExecute
    end
    object actToggleEnable: TAction
      Caption = 'Enable/disable toggle'
      OnExecute = actToggleEnableExecute
    end
    object actDelAll: TAction
      Caption = 'Delete all'
      OnExecute = actDelAllExecute
    end
  end
end
