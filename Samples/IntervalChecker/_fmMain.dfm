object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 412
  ClientWidth = 852
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
  object moMsg: TMemo
    Left = 0
    Top = 0
    Width = 852
    Height = 412
    Align = alClient
    ImeName = 'Microsoft IME 2010'
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Timer: TTimer
    Interval = 5
    OnTimer = TimerTimer
    Left = 420
    Top = 212
  end
end
