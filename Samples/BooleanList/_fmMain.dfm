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
  object btTest01: TButton
    Left = 96
    Top = 20
    Width = 75
    Height = 25
    Caption = 'btTest01'
    TabOrder = 0
    OnClick = btTest01Click
  end
  object btTest02: TButton
    Left = 96
    Top = 51
    Width = 75
    Height = 25
    Caption = 'btTest02'
    TabOrder = 1
  end
end
