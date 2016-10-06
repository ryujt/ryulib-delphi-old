object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 292
  ClientWidth = 385
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
  object ProgressBar1: TProgressBar
    Left = 16
    Top = 60
    Width = 150
    Height = 17
    TabOrder = 0
  end
  object btStart: TButton
    Left = 180
    Top = 60
    Width = 75
    Height = 25
    Caption = 'btStart'
    TabOrder = 1
    OnClick = btStartClick
  end
  object edURL: TEdit
    Left = 16
    Top = 24
    Width = 337
    Height = 21
    TabOrder = 2
    Text = 'edURL'
  end
  object btStop: TButton
    Left = 180
    Top = 91
    Width = 75
    Height = 25
    Caption = 'btStop'
    TabOrder = 3
    OnClick = btStopClick
  end
end
