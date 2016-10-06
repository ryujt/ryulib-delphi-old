object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 301
  ClientWidth = 562
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
  object lbCount: TLabel
    Left = 8
    Top = 16
    Width = 37
    Height = 13
    Caption = 'lbCount'
  end
  object btClose: TButton
    Left = 368
    Top = 64
    Width = 75
    Height = 25
    Caption = 'btClose'
    TabOrder = 0
    OnClick = btCloseClick
  end
  object btRun1: TButton
    Left = 76
    Top = 144
    Width = 75
    Height = 25
    Caption = 'btRun1'
    TabOrder = 1
    OnClick = btRun1Click
  end
  object btRun2: TButton
    Left = 76
    Top = 175
    Width = 75
    Height = 25
    Caption = 'btRun2'
    TabOrder = 2
    OnClick = btRun2Click
  end
  object btRun3: TButton
    Left = 76
    Top = 206
    Width = 75
    Height = 25
    Caption = 'btRun3'
    TabOrder = 3
    OnClick = btRun3Click
  end
  object btStop1: TButton
    Left = 157
    Top = 144
    Width = 75
    Height = 25
    Caption = 'btStop1'
    TabOrder = 4
    OnClick = btStop1Click
  end
  object btStop2: TButton
    Left = 157
    Top = 175
    Width = 75
    Height = 25
    Caption = 'btStop2'
    TabOrder = 5
    OnClick = btStop2Click
  end
  object btStop3: TButton
    Left = 157
    Top = 206
    Width = 75
    Height = 25
    Caption = 'btStop3'
    TabOrder = 6
    OnClick = btStop3Click
  end
  object Timer: TTimer
    Interval = 5
    OnTimer = TimerTimer
    Left = 120
    Top = 44
  end
end
