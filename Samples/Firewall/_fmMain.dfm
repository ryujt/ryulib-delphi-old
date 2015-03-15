object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = #48169#54868#48317' '#46321#47197
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
  object Button1: TButton
    Left = 182
    Top = 43
    Width = 75
    Height = 25
    Caption = 'btOpen'
    TabOrder = 0
    OnClick = Button1Click
  end
  object edCaption: TEdit
    Left = 16
    Top = 16
    Width = 241
    Height = 21
    ImeName = 'Microsoft IME 2010'
    TabOrder = 1
    Text = 'Mediawave - RoomServer'
  end
  object OpenDialog: TOpenDialog
    Filter = 'Exe files|*.exe'
    Left = 80
    Top = 88
  end
end
