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
  PixelsPerInch = 96
  TextHeight = 13
  object btAdd1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btAdd1'
    TabOrder = 0
    OnClick = btAdd1Click
  end
  object btFind: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'btFind'
    TabOrder = 1
    OnClick = btFindClick
  end
  object btAdd2: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btAdd2'
    TabOrder = 2
    OnClick = btAdd2Click
  end
  object btRemove: TButton
    Left = 8
    Top = 70
    Width = 75
    Height = 25
    Caption = 'btRemove'
    TabOrder = 3
    OnClick = btRemoveClick
  end
end
