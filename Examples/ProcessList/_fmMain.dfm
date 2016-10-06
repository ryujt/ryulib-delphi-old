object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'Get Process List'
  ClientHeight = 562
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object moList: TMemo
    Left = 0
    Top = 41
    Width = 784
    Height = 521
    Align = alClient
    ImeName = #54620#44397#50612' '#51077#47141' '#49884#49828#53596' (IME 2000)'
    PopupMenu = PopupMenu
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 784
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 116
    ExplicitTop = 28
    ExplicitWidth = 185
    object btUpdate: TButton
      Left = 11
      Top = 9
      Width = 110
      Height = 25
      Caption = 'Get Process List'
      TabOrder = 0
      OnClick = btUpdateClick
    end
  end
  object PopupMenu: TPopupMenu
    AutoHotkeys = maManual
    Left = 476
    Top = 288
    object miSelectAll: TMenuItem
      Caption = #51204#52404' '#49440#53469
      ShortCut = 16449
      OnClick = miSelectAllClick
    end
  end
end
