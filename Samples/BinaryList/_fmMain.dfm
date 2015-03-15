object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 293
  ClientWidth = 426
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
  object ListBox: TListBox
    Left = 0
    Top = 41
    Width = 426
    Height = 252
    Align = alClient
    ImeName = #54620#44397#50612' '#51077#47141' '#49884#49828#53596' (IME 2000)'
    ItemHeight = 13
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 426
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object btAdd: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btAdd'
      TabOrder = 0
      OnClick = btAddClick
    end
    object btTest: TButton
      Left = 89
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btTest'
      TabOrder = 1
      OnClick = btTestClick
    end
  end
end
