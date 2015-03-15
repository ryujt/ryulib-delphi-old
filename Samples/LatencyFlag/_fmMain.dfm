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
  object btFalse: TButton
    Left = 17
    Top = 16
    Width = 75
    Height = 25
    Caption = 'btFalse'
    TabOrder = 0
    OnClick = btFalseClick
  end
  object btTrue: TButton
    Left = 17
    Top = 47
    Width = 75
    Height = 25
    Caption = 'btTrue'
    TabOrder = 1
    OnClick = btTrueClick
  end
  object Timer: TTimer
    Interval = 10
    OnTimer = TimerTimer
    Left = 148
    Top = 24
  end
end
