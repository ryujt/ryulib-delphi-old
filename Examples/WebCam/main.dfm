object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'Video Capture Devices'
  ClientHeight = 730
  ClientWidth = 1008
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 334
    Top = 8
    Width = 320
    Height = 240
    AutoSize = True
    Stretch = True
  end
  object btStop: TButton
    Left = 89
    Top = 258
    Width = 75
    Height = 25
    Caption = 'btStop'
    TabOrder = 2
    OnClick = btStopClick
  end
  object btStart: TButton
    Left = 8
    Top = 258
    Width = 75
    Height = 25
    Caption = 'btStart'
    TabOrder = 1
    OnClick = btStartClick
  end
  object btSnapShot: TButton
    Left = 170
    Top = 258
    Width = 75
    Height = 25
    Caption = 'btSnapShot'
    TabOrder = 0
  end
  object plCam: TPanel
    Left = 8
    Top = 8
    Width = 320
    Height = 240
    BevelOuter = bvNone
    TabOrder = 3
  end
  object cbDeviceList: TComboBox
    Left = 8
    Top = 289
    Width = 145
    Height = 21
    ImeName = 'Microsoft Office IME 2007'
    ItemHeight = 13
    TabOrder = 4
    OnChange = cbDeviceListChange
  end
  object cbResolutionList: TComboBox
    Left = 8
    Top = 316
    Width = 145
    Height = 21
    ImeName = 'Microsoft Office IME 2007'
    ItemHeight = 13
    TabOrder = 5
    OnChange = cbResolutionListChange
  end
  object Timer: TTimer
    Interval = 5
    OnTimer = TimerTimer
    Left = 656
    Top = 24
  end
end
