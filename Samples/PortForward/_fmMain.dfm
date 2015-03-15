object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 397
  ClientWidth = 566
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 566
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btOpen: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Open'
      TabOrder = 0
      OnClick = btOpenClick
    end
    object btOpenUsingThread: TButton
      Left = 84
      Top = 9
      Width = 137
      Height = 25
      Caption = 'Open (using thread)'
      TabOrder = 1
      OnClick = btOpenUsingThreadClick
    end
  end
  object moMsg: TMemo
    Left = 0
    Top = 41
    Width = 566
    Height = 356
    Align = alClient
    ImeName = 'Microsoft IME 2010'
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
