object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 412
  ClientWidth = 509
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
  object btAdd: TButton
    Left = 12
    Top = 12
    Width = 75
    Height = 25
    Caption = 'btAdd'
    TabOrder = 0
    OnClick = btAddClick
  end
  object moMsg: TMemo
    Left = 236
    Top = 8
    Width = 265
    Height = 396
    ImeName = 'Microsoft IME 2010'
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object btSearch: TButton
    Left = 12
    Top = 43
    Width = 75
    Height = 25
    Caption = 'btSearch'
    TabOrder = 2
    OnClick = btSearchClick
  end
  object edSearch: TEdit
    Left = 93
    Top = 45
    Width = 121
    Height = 21
    ImeName = 'Microsoft IME 2010'
    TabOrder = 3
  end
  object btList: TButton
    Left = 12
    Top = 74
    Width = 75
    Height = 25
    Caption = 'btList'
    TabOrder = 4
    OnClick = btListClick
  end
  object btBulkAdd: TButton
    Left = 12
    Top = 105
    Width = 75
    Height = 25
    Caption = 'btBulkAdd'
    TabOrder = 5
    OnClick = btBulkAddClick
  end
  object btClear: TButton
    Left = 12
    Top = 136
    Width = 75
    Height = 25
    Caption = 'btClear'
    TabOrder = 6
    OnClick = btClearClick
  end
end
