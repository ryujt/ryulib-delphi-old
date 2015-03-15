object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 389
  ClientWidth = 640
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
  object ScrollBox: TScrollBox
    Left = 0
    Top = 41
    Width = 640
    Height = 348
    Align = alClient
    TabOrder = 0
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 105
      Height = 105
      AutoSize = True
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 640
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object btCapture: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btCapture'
      TabOrder = 0
      OnClick = btCaptureClick
    end
  end
end
