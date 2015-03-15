object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 612
  ClientWidth = 771
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
    Width = 771
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object cbAppList: TComboBox
      Left = 4
      Top = 11
      Width = 222
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      Text = #44277#50976#54624' '#54532#47196#44536#47016#51012' '#49440#53469#54616#49464#50836
      OnChange = cbAppListChange
      OnDropDown = cbAppListDropDown
      OnKeyPress = cbAppListKeyPress
    end
  end
end
