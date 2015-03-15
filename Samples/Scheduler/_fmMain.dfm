object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'Scheduler Sample'
  ClientHeight = 412
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btStart: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btStart'
    TabOrder = 0
    OnClick = btStartClick
  end
  object btStop: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'btStop'
    TabOrder = 1
    OnClick = btStopClick
  end
end
