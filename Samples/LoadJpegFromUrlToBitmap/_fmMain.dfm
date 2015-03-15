object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 567
  ClientWidth = 765
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 0
    Top = 153
    Width = 765
    Height = 414
    Align = alClient
    ExplicitLeft = 148
    ExplicitTop = 228
    ExplicitWidth = 320
    ExplicitHeight = 240
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 765
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 304
    ExplicitTop = 116
    ExplicitWidth = 185
    object btLoadJPEG: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btLoad'
      TabOrder = 0
      OnClick = btLoadJPEGClick
    end
    object btAsync: TButton
      Left = 84
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btAsync'
      TabOrder = 1
      OnClick = btAsyncClick
    end
  end
  object moMsg: TMemo
    Left = 0
    Top = 41
    Width = 765
    Height = 112
    Align = alTop
    ImeName = 'Microsoft IME 2010'
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
