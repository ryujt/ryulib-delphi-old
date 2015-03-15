object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 397
  ClientWidth = 513
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Fixedsys'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 0
    Top = 165
    Width = 513
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 232
  end
  object moInput: TMemo
    Left = 0
    Top = 41
    Width = 513
    Height = 124
    Align = alTop
    Lines.Strings = (
      '1234'#39'a//bc'#39#12593#12596#12599
      '-------/======'
      '------//======'
      ''
      '{'
      '  sdfasdfas'
      '  (*'
      '}'
      ''
      '(**************'
      '   s {dfsadfa} sd'
      '   ..//'
      '   '#39' ss// '#39
      '****************************)'
      ''
      'dsfsd (* sdfsdf*)'
      '( *'
      ''
      '(*  sdfsds *  sdfsf *)'
      'sdafasdf')
    ScrollBars = ssBoth
    TabOrder = 0
    OnDblClick = moInputDblClick
  end
  object moResult: TMemo
    Left = 0
    Top = 168
    Width = 513
    Height = 229
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 513
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 116
    ExplicitTop = 252
    ExplicitWidth = 185
    object btStart: TButton
      Left = 12
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btStart'
      TabOrder = 0
      OnClick = btStartClick
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'All files|*.*'
    Left = 108
    Top = 48
  end
end
