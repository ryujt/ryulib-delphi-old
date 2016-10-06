object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 411
  ClientWidth = 852
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 852
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 68
    ExplicitTop = 144
    ExplicitWidth = 185
    object btOpen: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Open'
      TabOrder = 0
      OnClick = btOpenClick
    end
  end
  object moSrc: TMemo
    Left = 0
    Top = 41
    Width = 852
    Height = 370
    Align = alClient
    Font.Charset = HANGEUL_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = #44404#47548#52404
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object OpenDialog: TOpenDialog
    Filter = 'Pascal file|*.pas'
    Left = 420
    Top = 212
  end
end
