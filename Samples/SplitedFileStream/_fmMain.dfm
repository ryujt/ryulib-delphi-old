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
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 554
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 180
    ExplicitTop = 76
    ExplicitWidth = 185
    object btSrc: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btSrc'
      TabOrder = 0
      OnClick = btSrcClick
    end
    object btSplit: TButton
      Left = 89
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btSplit'
      TabOrder = 1
      OnClick = btSplitClick
    end
    object btDst: TButton
      Left = 170
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btDst'
      TabOrder = 2
      OnClick = btDstClick
    end
    object btTest: TButton
      Left = 292
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btTest'
      TabOrder = 3
      OnClick = btTestClick
    end
  end
end
