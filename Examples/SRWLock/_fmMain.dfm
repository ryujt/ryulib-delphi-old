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
    Left = 12
    Top = 24
    Width = 293
    Height = 325
    ImeName = 'Microsoft IME 2010'
    Lines.Strings = (
      'moMsg')
    TabOrder = 0
  end
  object btCS_Single: TButton
    Left = 316
    Top = 22
    Width = 75
    Height = 25
    Caption = 'btCS_Single'
    TabOrder = 1
    OnClick = btCS_SingleClick
  end
  object btSL_Single: TButton
    Left = 316
    Top = 53
    Width = 75
    Height = 25
    Caption = 'btSL_Single'
    TabOrder = 2
    OnClick = btSL_SingleClick
  end
  object btCS_Multi: TButton
    Left = 316
    Top = 112
    Width = 75
    Height = 25
    Caption = 'btCS_Multi'
    TabOrder = 3
    OnClick = btCS_MultiClick
  end
  object btSL_Multi: TButton
    Left = 316
    Top = 143
    Width = 75
    Height = 25
    Caption = 'btSL_Multi'
    TabOrder = 4
    OnClick = btSL_MultiClick
  end
end
