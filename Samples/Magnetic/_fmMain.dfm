object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 504
  ClientWidth = 558
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btShow: TButton
    Left = 60
    Top = 44
    Width = 75
    Height = 25
    Caption = 'btShow'
    TabOrder = 0
    OnClick = btShowClick
  end
  object btHide: TButton
    Left = 60
    Top = 80
    Width = 75
    Height = 25
    Caption = 'btHide'
    TabOrder = 1
    OnClick = btHideClick
  end
end
