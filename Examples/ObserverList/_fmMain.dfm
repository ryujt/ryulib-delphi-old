object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 209
  ClientWidth = 359
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 359
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 36
    ExplicitTop = 40
    ExplicitWidth = 185
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 359
    Height = 80
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 1
    inline frText: TfrText
      Left = 1
      Top = 1
      Width = 357
      Height = 78
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 1
      ExplicitTop = 1
      ExplicitWidth = 357
      ExplicitHeight = 238
      inherited lbVolume: TLabel
        Width = 357
        Height = 78
        ExplicitWidth = 357
        ExplicitHeight = 169
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 121
    Width = 359
    Height = 88
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 2
    ExplicitLeft = 40
    ExplicitTop = 344
    ExplicitWidth = 185
    ExplicitHeight = 41
    inline frBar: TfrBar
      Left = 1
      Top = 1
      Width = 357
      Height = 86
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 39
      ExplicitTop = 19
    end
  end
  object Timer: TTimer
    Interval = 500
    OnTimer = TimerTimer
    Left = 12
    Top = 4
  end
end
