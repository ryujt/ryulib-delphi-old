object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'Insert Debug Code'
  ClientHeight = 561
  ClientWidth = 784
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 784
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btExecute: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Execute'
      TabOrder = 0
      OnClick = btExecuteClick
    end
  end
  object moResult: TMemo
    Left = 0
    Top = 41
    Width = 784
    Height = 520
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
