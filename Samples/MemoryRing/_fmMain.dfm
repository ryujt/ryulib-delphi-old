object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 243
  ClientWidth = 472
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 472
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btTest: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btTest'
      TabOrder = 0
      OnClick = btTestClick
    end
  end
  object moMsg: TMemo
    Left = 0
    Top = 41
    Width = 472
    Height = 202
    Align = alClient
    ImeName = 'Microsoft IME 2010'
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
