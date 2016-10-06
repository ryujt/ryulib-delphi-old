object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 514
  ClientWidth = 686
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
  object spl1: TSplitter
    Left = 0
    Top = 221
    Width = 686
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 165
    ExplicitWidth = 349
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 686
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btn1: TButton
      Left = 4
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btn1'
      TabOrder = 0
      OnClick = btn1Click
    end
  end
  object moInput: TMemo
    Left = 0
    Top = 41
    Width = 686
    Height = 180
    Align = alTop
    ImeName = 'Microsoft IME 2010'
    Lines.Strings = (
      'Hi <br>'
      '<font size="3" color="red">This '
      '  <font color="white">is some text!</font>'
      '</font>'
      '<br>'
      ''
      'Normal <b>and this is bold text</b>')
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object moResult: TMemo
    Left = 0
    Top = 224
    Width = 686
    Height = 290
    Align = alClient
    ImeName = 'Microsoft IME 2010'
    ScrollBars = ssBoth
    TabOrder = 2
  end
end
