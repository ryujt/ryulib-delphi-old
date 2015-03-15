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
  object Button1: TButton
    Left = 444
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 444
    Top = 47
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 444
    Top = 78
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 2
    OnClick = Button3Click
  end
  object moMsg: TMemo
    Left = 12
    Top = 12
    Width = 409
    Height = 257
    ImeName = 'Microsoft IME 2010'
    TabOrder = 3
  end
end
