object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 301
  ClientWidth = 467
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
  object btRun: TButton
    Left = 372
    Top = 16
    Width = 75
    Height = 25
    Caption = 'btRun'
    TabOrder = 0
    OnClick = btRunClick
  end
  object moMsg: TMemo
    Left = 8
    Top = 4
    Width = 333
    Height = 265
    ImeName = 'Microsoft IME 2003'
    TabOrder = 1
  end
end
