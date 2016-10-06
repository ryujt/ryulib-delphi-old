object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 243
  ClientWidth = 472
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
  object moMsg: TMemo
    Left = 0
    Top = 41
    Width = 472
    Height = 202
    Align = alClient
    ImeName = 'Microsoft IME 2010'
    Lines.Strings = (
      '{'
      '  "Result": 1, '
      '  "Name": "Ryu", '
      '  "email": "ryujt658@hanmail.net",'
      '  "Family": {'
      '    "Wife":" Lee",'
      '    "Son": "Do",'
      '    "Daughter": "In"'
      '    }'
      '}')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 472
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object btTest1: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Test1'
      TabOrder = 0
      OnClick = btTest1Click
    end
    object btTest2: TButton
      Left = 84
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Test2'
      TabOrder = 1
      OnClick = btTest2Click
    end
  end
end
