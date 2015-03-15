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
  object btUse: TButton
    Left = 308
    Top = 104
    Width = 75
    Height = 25
    Caption = 'btUse'
    TabOrder = 0
    OnClick = btUseClick
  end
  object btRemove: TButton
    Left = 308
    Top = 135
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'btRemove'
    TabOrder = 1
    OnClick = btRemoveClick
  end
end
