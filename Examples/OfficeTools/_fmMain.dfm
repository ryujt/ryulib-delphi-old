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
  PixelsPerInch = 96
  TextHeight = 13
  object btOpen: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'btOpen'
    TabOrder = 0
    OnClick = btOpenClick
  end
  object OpenDialog: TOpenDialog
    Filter = 'PPT file(s)|*.ppt;*.pptx'
    Left = 112
    Top = 20
  end
end
