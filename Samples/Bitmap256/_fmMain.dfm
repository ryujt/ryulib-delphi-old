object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 730
  ClientWidth = 1008
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ScrollBox: TScrollBox
    Left = 0
    Top = 41
    Width = 768
    Height = 689
    Align = alClient
    TabOrder = 0
    object Image: TImage
      Left = 0
      Top = 0
      Width = 105
      Height = 105
      AutoSize = True
    end
  end
  object moMsg: TMemo
    Left = 768
    Top = 41
    Width = 240
    Height = 689
    Align = alRight
    ImeName = 'Microsoft IME 2010'
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1008
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object btStart: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btStart'
      TabOrder = 0
      OnClick = btStartClick
    end
  end
  object Timer: TTimer
    Interval = 20
    OnTimer = TimerTimer
    Left = 324
    Top = 244
  end
end
