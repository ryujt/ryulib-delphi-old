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
  object btAddDatas: TButton
    Left = 471
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btAddDatas'
    TabOrder = 0
    OnClick = btAddDatasClick
  end
  object btCheck: TButton
    Left = 471
    Top = 39
    Width = 75
    Height = 25
    Caption = 'btCheck'
    TabOrder = 1
    OnClick = btCheckClick
  end
  object btRemove: TButton
    Left = 471
    Top = 70
    Width = 75
    Height = 25
    Caption = 'btRemove'
    TabOrder = 2
    OnClick = btRemoveClick
  end
  object moMsg: TMemo
    Left = 8
    Top = 8
    Width = 393
    Height = 237
    ImeName = 'Microsoft IME 2010'
    Lines.Strings = (
      'moMsg')
    TabOrder = 3
  end
end
