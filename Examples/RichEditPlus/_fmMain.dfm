object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 211
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
  object RichEdit1: TRichEdit
    Left = 8
    Top = 8
    Width = 349
    Height = 149
    Font.Charset = HANGEUL_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    HideScrollBars = False
    ImeName = 'Microsoft IME 2010'
    Lines.Strings = (
      'RichEdit1'
      'dsfasd'
      'fas'
      'df'
      'asdf'
      'asdf'
      'asd'
      'gfads'
      'gsdgsdfgsdfgadfg'
      'sdfg'
      'sdfg'
      'sdf'
      'gsdfgsdfgsdfgsdfgsdfgsdfg'
      'sdfg'
      'sdfg'
      'sd'
      'fg'
      'zxcfasdfas')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 300
    Top = 20
  end
end
