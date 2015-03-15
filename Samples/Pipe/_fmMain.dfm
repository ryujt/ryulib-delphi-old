object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 316
  ClientWidth = 552
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
  object moMsg: TMemo
    Left = 0
    Top = 0
    Width = 285
    Height = 237
    ImeName = 'Microsoft IME 2010'
    Lines.Strings = (
      'moMsg')
    TabOrder = 0
  end
  object edMsg: TEdit
    Left = 248
    Top = 276
    Width = 121
    Height = 21
    ImeName = 'Microsoft IME 2010'
    TabOrder = 1
    Text = 'edMsg'
  end
  object Button1: TButton
    Left = 375
    Top = 274
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 2
    OnClick = Button1Click
  end
end
