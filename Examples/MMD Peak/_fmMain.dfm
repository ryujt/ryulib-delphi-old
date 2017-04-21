object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'Volume'
  ClientHeight = 61
  ClientWidth = 226
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
  object Label1: TLabel
    Left = 12
    Top = 12
    Width = 49
    Height = 13
    Caption = 'Wave Out'
  end
  object Label2: TLabel
    Left = 12
    Top = 40
    Width = 15
    Height = 13
    Caption = 'Mic'
  end
  object RenderBar: TProgressBar
    Left = 67
    Top = 8
    Width = 150
    Height = 17
    TabOrder = 0
  end
  object CaptureBar: TProgressBar
    Left = 67
    Top = 36
    Width = 150
    Height = 17
    TabOrder = 1
  end
  object Timer: TTimer
    Interval = 20
    OnTimer = TimerTimer
    Left = 44
    Top = 16
  end
end
