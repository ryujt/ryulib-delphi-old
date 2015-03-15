object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 337
  ClientWidth = 635
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
  object moMsg: TMemo
    Left = 0
    Top = 41
    Width = 635
    Height = 296
    Align = alClient
    ImeName = 'Microsoft IME 2010'
    ScrollBars = ssBoth
    TabOrder = 0
    ExplicitTop = 0
    ExplicitHeight = 337
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 336
    ExplicitTop = 168
    ExplicitWidth = 185
    object btOpen: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Open'
      TabOrder = 0
      OnClick = btOpenClick
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'All file(s)|*.*'
    Left = 220
    Top = 104
  end
end
