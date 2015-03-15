object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 290
  ClientWidth = 554
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
  object Label1: TLabel
    Left = 16
    Top = 17
    Width = 63
    Height = 13
    Caption = #45936#51060#53552' '#49373#49457
  end
  object Label2: TLabel
    Left = 216
    Top = 17
    Width = 63
    Height = 13
    Caption = #45936#51060#53552' '#49548#48708
  end
  object btStartCreate: TButton
    Left = 16
    Top = 36
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = btStartCreateClick
  end
  object btStopCreate: TButton
    Left = 97
    Top = 36
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 1
    OnClick = btStopCreateClick
  end
  object btStartUse: TButton
    Left = 216
    Top = 36
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 2
    OnClick = btStartUseClick
  end
  object btStopUse: TButton
    Left = 297
    Top = 36
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 3
    OnClick = btStopUseClick
  end
  object tmCreate: TTimer
    Enabled = False
    Interval = 20
    OnTimer = tmCreateTimer
    Left = 16
    Top = 80
  end
  object tmUse: TTimer
    Enabled = False
    Interval = 20
    OnTimer = tmUseTimer
    Left = 212
    Top = 76
  end
end
