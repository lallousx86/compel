object frmDebuggerWindow: TfrmDebuggerWindow
  Left = 229
  Top = 287
  Caption = 'Debugger Window'
  ClientHeight = 336
  ClientWidth = 807
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  Icon.Data = {
    0000010001002020100000000400E80200001600000028000000200000004000
    0000010004000000000000020000000000000000000010000000100000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000099000000000000000000
    0000099999999991000000000000000000009990000099999000000000000000
    0009900000000099991000000000000000990000000000000999000000000000
    00990000000000000019900000000000099000000000000000009900000000BB
    BBBB0000BBBBBBB00000BBBB0B0000BB999BB000BB0000BB000BB009BB0000BB
    9900BB00BB0000BB00BB0000BB0000BB9900BB00BB0000BB00BB0000BB0000BB
    9900BB00BB0000BB00BB00BBBB0000BB9900BB00BBBBBBB000BB0000000000BB
    9900BB00BB0000BB00BB0000000000BB9990BB00BB0000BB00BB00000B0000BB
    999BB000BB0000BB000BB000BB0000BBBBBB0000BBBBBBB00000BBBBB0000000
    0999000000000000000000000000000009999000000000000000000000000000
    0099990000000000000000000000000000999990000000000000000000000000
    0009999900000000000000000000000000009999990000000000000000000000
    0000099909999990000000000000000000000019900000000000000000000000
    0000000099100000000000000000000000000000009990000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbScript: TListBox
    Left = 0
    Top = 26
    Width = 807
    Height = 310
    Style = lbOwnerDrawFixed
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 16
    ParentFont = False
    PopupMenu = popmenuDbg
    TabOrder = 0
    OnDrawItem = lbScriptDrawItem
  end
  object ToolBar2: TToolBar
    Left = 0
    Top = 0
    Width = 807
    Height = 26
    AutoSize = True
    Caption = 'Debugger Toolbar'
    Color = clBtnFace
    EdgeBorders = [ebLeft, ebTop, ebRight, ebBottom]
    ParentColor = False
    TabOrder = 1
    object cmdToggleBreakpoint: TBitBtn
      Left = 0
      Top = 0
      Width = 33
      Height = 22
      Caption = 'TOGGLE BP'
      TabOrder = 0
    end
    object cmdRunToCursor: TBitBtn
      Left = 33
      Top = 0
      Width = 40
      Height = 22
      Caption = 'HERE'
      TabOrder = 2
    end
    object cmdLocateExecPoint: TBitBtn
      Left = 73
      Top = 0
      Width = 40
      Height = 22
      Caption = 'LOCATE EP'
      TabOrder = 3
    end
    object cmdCycle: TBitBtn
      Left = 113
      Top = 0
      Width = 32
      Height = 22
      Caption = 'Cycle'
      TabOrder = 1
    end
  end
  object popmenuDbg: TPopupMenu
    Left = 272
    Top = 192
    object mnuSetNewEP: TMenuItem
      Caption = 'Set new EP'
    end
    object mnuToggleBP: TMenuItem
      Caption = 'Toggle BP'
    end
    object Patchline1: TMenuItem
      Action = actPatchCode
    end
  end
  object actlistDebugWindow: TActionList
    Left = 312
    Top = 192
    object actPatchCode: TAction
      Caption = 'Patch line'
      OnExecute = actPatchCodeExecute
    end
  end
end
