object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 381
  ClientWidth = 510
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 510
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
    object btRight: TButton
      Left = 120
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btRight'
      TabOrder = 1
      OnClick = btRightClick
    end
    object btBottom: TButton
      Left = 196
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btBottom'
      TabOrder = 2
      OnClick = btBottomClick
    end
  end
  inline frCommon: TfrCommon
    Left = 0
    Top = 41
    Width = 510
    Height = 340
    Align = alClient
    TabOrder = 1
    ExplicitTop = 41
    ExplicitWidth = 510
    ExplicitHeight = 340
    inherited moMsg: TMemo
      Width = 510
      Height = 340
      ExplicitWidth = 510
      ExplicitHeight = 340
    end
  end
end
