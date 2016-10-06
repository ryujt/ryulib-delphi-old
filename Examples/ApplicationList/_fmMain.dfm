object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 815
  ClientWidth = 909
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object plBottom: TPanel
    Left = 0
    Top = 785
    Width = 909
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    Color = clBlack
    ParentBackground = False
    TabOrder = 0
    object Label1: TLabel
      Left = 84
      Top = 8
      Width = 75
      Height = 13
      Caption = #54532#47196#44536#47016' '#49440#53469
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lbResolution: TLabel
      Left = 8
      Top = 8
      Width = 48
      Height = 13
      Caption = '0000x000'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object bt1024x768: TSpeedButton
      Tag = 1
      Left = 499
      Top = 4
      Width = 57
      Height = 22
      GroupIndex = 1
      Caption = '1024x768'
      OnClick = SpeedButtonClick
    end
    object bt1280x800: TSpeedButton
      Tag = 2
      Left = 558
      Top = 4
      Width = 57
      Height = 22
      GroupIndex = 2
      Caption = '1280x800'
      OnClick = SpeedButtonClick
    end
    object btNot1024x768: TSpeedButton
      Left = 744
      Top = 6
      Width = 23
      Height = 22
      GroupIndex = 1
      Down = True
      Visible = False
    end
    object btNot1280x800: TSpeedButton
      Left = 768
      Top = 6
      Width = 23
      Height = 22
      GroupIndex = 2
      Down = True
      Visible = False
    end
    object cbApplication: TComboBox
      Left = 165
      Top = 4
      Width = 328
      Height = 21
      ImeName = 'Microsoft IME 2010'
      TabOrder = 0
      OnChange = cbApplicationChange
      OnDropDown = cbApplicationDropDown
      OnKeyDown = cbApplicationKeyDown
      OnKeyPress = cbApplicationKeyPress
      OnKeyUp = cbApplicationKeyUp
    end
  end
  object plDeskCam: TPanel
    Left = 0
    Top = 0
    Width = 909
    Height = 785
    Align = alClient
    BevelOuter = bvNone
    Color = clGray
    ParentBackground = False
    TabOrder = 1
  end
end
