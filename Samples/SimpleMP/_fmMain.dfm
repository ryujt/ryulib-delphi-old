object fmMain: TfmMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'fmMain'
  ClientHeight = 253
  ClientWidth = 482
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btExecute: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = '1+2...+100'
    TabOrder = 0
    OnClick = btExecuteClick
  end
  object btSpeedCheck: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'SpeedCheck'
    TabOrder = 1
    OnClick = btSpeedCheckClick
  end
  object moMsg: TMemo
    Left = 0
    Top = 44
    Width = 482
    Height = 209
    Align = alBottom
    ImeName = 'Microsoft IME 2010'
    ScrollBars = ssBoth
    TabOrder = 2
  end
end
