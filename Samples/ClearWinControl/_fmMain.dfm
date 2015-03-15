object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 412
  ClientWidth = 646
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 244
    Top = 148
    Width = 185
    Height = 41
    Caption = 'Panel1'
    TabOrder = 0
    object Button1: TButton
      Left = 92
      Top = 12
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 0
    end
  end
  object Button2: TButton
    Left = 60
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 1
    OnClick = Button2Click
  end
end
