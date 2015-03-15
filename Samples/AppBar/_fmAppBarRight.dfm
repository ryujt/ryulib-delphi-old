object fmAppBarRight: TfmAppBarRight
  Left = 0
  Top = 0
  Caption = 'fmAppBarRight'
  ClientHeight = 337
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btClose: TButton
      Left = 12
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btClose'
      TabOrder = 0
      OnClick = btCloseClick
    end
    object btBottom: TButton
      Left = 93
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btBottom'
      TabOrder = 1
      OnClick = btBottomClick
    end
  end
end
