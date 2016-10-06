object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 290
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbMsg: TLabel
    Left = 176
    Top = 29
    Width = 27
    Height = 13
    Caption = 'lbMsg'
  end
  object btStart: TButton
    Left = 20
    Top = 24
    Width = 137
    Height = 25
    Caption = #47700#47784#51109' '#51648#53020' '#48372#44592
    TabOrder = 0
    OnClick = btStartClick
  end
  object btStop: TButton
    Left = 20
    Top = 60
    Width = 75
    Height = 25
    Caption = 'btStop'
    TabOrder = 1
    OnClick = btStopClick
  end
end
