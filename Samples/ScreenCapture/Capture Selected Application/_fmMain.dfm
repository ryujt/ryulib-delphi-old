object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 337
  ClientWidth = 635
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
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 635
    Height = 337
    Align = alClient
    TabOrder = 0
    object Image: TImage
      Left = 0
      Top = 0
      Width = 100
      Height = 100
      AutoSize = True
    end
  end
  object Timer: TTimer
    Interval = 5
    OnTimer = TimerTimer
    Left = 312
    Top = 172
  end
end
