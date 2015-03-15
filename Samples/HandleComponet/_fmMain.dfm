object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 581
  ClientWidth = 664
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
  object Memo1: TMemo
    Left = 20
    Top = 188
    Width = 469
    Height = 325
    ImeName = 'Microsoft IME 2003'
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 540
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 540
    Top = 175
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 2
    OnClick = Button2Click
  end
end
