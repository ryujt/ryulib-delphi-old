object fmNormalBitmap: TfmNormalBitmap
  Left = 0
  Top = 0
  Align = alClient
  BorderStyle = bsNone
  Caption = 'fmNormalBitmap'
  ClientHeight = 513
  ClientWidth = 684
  Color = 10197915
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 13
  object ScrollBox: TScrollBox
    Left = 0
    Top = 0
    Width = 684
    Height = 513
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 0
    Visible = False
  end
  object Timer: TTimer
    Interval = 5
    OnTimer = TimerTimer
    Left = 328
    Top = 244
  end
end
