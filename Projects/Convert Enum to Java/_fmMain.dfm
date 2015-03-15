object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'Convert Enum to Java'
  ClientHeight = 562
  ClientWidth = 784
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
  object moMsg: TMemo
    Left = 0
    Top = 41
    Width = 784
    Height = 521
    Align = alClient
    ImeName = 'Microsoft IME 2010'
    Lines.Strings = (
      '// Comment test'
      'taLeftJustify, '
      ''
      'taRightJustify, '
      'taCenter')
    ScrollBars = ssBoth
    TabOrder = 0
    ExplicitWidth = 554
    ExplicitHeight = 249
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 784
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 554
    object btConvert: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Convert'
      TabOrder = 0
      OnClick = btConvertClick
    end
  end
end
