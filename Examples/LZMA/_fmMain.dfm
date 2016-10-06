object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 239
  ClientWidth = 468
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
  object btLZMA: TButton
    Left = 387
    Top = 2
    Width = 75
    Height = 25
    Caption = 'LZMA'
    TabOrder = 0
    OnClick = btLZMAClick
  end
  object moMsg: TMemo
    Left = 4
    Top = 4
    Width = 377
    Height = 229
    ImeName = 'Microsoft IME 2010'
    TabOrder = 1
  end
  object btTestLZMA: TButton
    Left = 387
    Top = 28
    Width = 75
    Height = 25
    Caption = 'Test LZMA'
    TabOrder = 2
    OnClick = btTestLZMAClick
  end
end
