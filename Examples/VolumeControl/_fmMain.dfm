object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 77
  ClientWidth = 201
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
    Left = 19
    Top = 20
    Width = 15
    Height = 13
    Caption = 'Mic'
  end
  object Label2: TLabel
    Left = 19
    Top = 51
    Width = 39
    Height = 13
    Caption = 'Speaker'
  end
  object sbMic: TScrollBar
    Left = 64
    Top = 16
    Width = 121
    Height = 17
    PageSize = 0
    TabOrder = 0
    OnChange = sbMicChange
  end
  object sbSpeaker: TScrollBar
    Left = 64
    Top = 47
    Width = 121
    Height = 17
    PageSize = 0
    TabOrder = 1
    OnChange = sbSpeakerChange
  end
end
