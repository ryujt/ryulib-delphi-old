object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'TScreenCapture Sample'
  ClientHeight = 292
  ClientWidth = 832
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
    Width = 832
    Height = 41
    Align = alTop
    TabOrder = 0
    object Button1: TButton
      Left = 335
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Moniter'
      TabOrder = 0
      OnClick = Button1Click
    end
    object EditX: TSpinEdit
      Left = 416
      Top = 10
      Width = 55
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
    end
    object EditY: TSpinEdit
      Left = 477
      Top = 10
      Width = 55
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
    end
    object EditH: TSpinEdit
      Left = 599
      Top = 10
      Width = 55
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 3
      Value = 100
    end
    object EditW: TSpinEdit
      Left = 538
      Top = 10
      Width = 55
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 4
      Value = 100
    end
    object Button2: TButton
      Left = 660
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Rect'
      TabOrder = 5
      OnClick = Button2Click
    end
    object CheckBoxWithCursor: TCheckBox
      Left = 13
      Top = 12
      Width = 121
      Height = 17
      Caption = 'Capture With Cursor'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object ComboBoxMoniter: TComboBox
      Left = 136
      Top = 10
      Width = 193
      Height = 21
      Style = csDropDownList
      ImeName = 'Microsoft Office IME 2007'
      ItemHeight = 13
      TabOrder = 7
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 41
    Width = 832
    Height = 251
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 168
    ExplicitTop = 96
    ExplicitWidth = 185
    ExplicitHeight = 41
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 105
      Height = 105
      AutoSize = True
    end
  end
end
