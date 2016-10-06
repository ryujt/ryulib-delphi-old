object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 586
  ClientWidth = 729
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 729
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 288
    ExplicitTop = 264
    ExplicitWidth = 185
    object btClear: TButton
      Left = 89
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btClear'
      TabOrder = 0
      OnClick = btClearClick
    end
    object btOpen: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btOpen'
      TabOrder = 1
      OnClick = btOpenClick
    end
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 84
    Top = 116
  end
end
