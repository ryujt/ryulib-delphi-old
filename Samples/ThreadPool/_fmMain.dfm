object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 301
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 269
    Height = 301
    Align = alLeft
    ImeName = 'Microsoft IME 2010'
    ItemHeight = 13
    TabOrder = 0
  end
  object btStart: TButton
    Left = 280
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btStart'
    TabOrder = 1
    OnClick = btStartClick
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = btStartClick
    Left = 300
    Top = 72
  end
end
