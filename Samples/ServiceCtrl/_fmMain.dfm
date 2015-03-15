object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 257
  ClientWidth = 496
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
  object btSrart: TButton
    Left = 316
    Top = 44
    Width = 75
    Height = 25
    Caption = 'btSrart'
    TabOrder = 0
    OnClick = btSrartClick
  end
  object btStop: TButton
    Left = 316
    Top = 75
    Width = 75
    Height = 25
    Caption = 'btStop'
    TabOrder = 1
    OnClick = btStopClick
  end
  object btStatus: TButton
    Left = 316
    Top = 106
    Width = 75
    Height = 25
    Caption = 'btStatus'
    TabOrder = 2
    OnClick = btStatusClick
  end
end
