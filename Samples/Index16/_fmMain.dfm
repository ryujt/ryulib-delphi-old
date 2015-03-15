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
  object btInsert: TButton
    Left = 444
    Top = 16
    Width = 75
    Height = 25
    Caption = 'btInsert'
    TabOrder = 0
    OnClick = btInsertClick
  end
  object moMsg: TMemo
    Left = 12
    Top = 8
    Width = 261
    Height = 274
    ImeName = 'Microsoft IME 2010'
    TabOrder = 1
  end
  object edKey: TEdit
    Left = 317
    Top = 64
    Width = 121
    Height = 21
    ImeName = 'Microsoft IME 2010'
    TabOrder = 2
  end
  object btFind: TButton
    Left = 444
    Top = 62
    Width = 75
    Height = 25
    Caption = 'btFind'
    TabOrder = 3
    OnClick = btFindClick
  end
end
