object fmSelectPathDlg: TfmSelectPathDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #54260#45908' '#49440#53469
  ClientHeight = 299
  ClientWidth = 394
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object plClient: TPanel
    Left = 0
    Top = 0
    Width = 394
    Height = 299
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 394
      Height = 45
      Align = alTop
      BevelOuter = bvLowered
      Caption = #45433#54868' '#54028#51068#46308#51060' '#51200#51109' '#46112' '#54260#45908#47484' '#49440#53469#54616#49884#44592' '#48148#46989#45768#45796'.'
      TabOrder = 0
    end
    object edPath: TEdit
      Left = 0
      Top = 45
      Width = 394
      Height = 21
      Align = alTop
      BevelOuter = bvNone
      ImeName = 'Microsoft IME 2010'
      TabOrder = 1
      OnKeyPress = edPathKeyPress
    end
    object Panel2: TPanel
      Left = 0
      Top = 258
      Width = 394
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      DesignSize = (
        394
        41)
      object btOk: TXiButton
        Left = 231
        Top = 8
        Width = 75
        Height = 25
        ColorFace = 14737632
        ColorGrad = 3653375
        ColorDark = 11447982
        ColorLight = 16250871
        ColorBorder = 6447714
        ColorText = clBlack
        OverColorFace = 13619151
        OverColorGrad = clWhite
        OverColorDark = 7960953
        OverColorLight = 15658734
        OverColorBorder = 7697781
        OverColorText = clBlack
        DownColorFace = 13882323
        DownColorGrad = clWhite
        DownColorDark = 15329769
        DownColorLight = 8158332
        DownColorBorder = 5131854
        DownColorText = clBlack
        DisabledColorFace = 14869218
        DisabledColorGrad = clWhite
        DisabledColorDark = 14211288
        DisabledColorLight = 15395562
        DisabledColorBorder = 12895428
        DisabledColorText = clGray
        ColorFocusRect = 9079434
        ColorScheme = csSilver
        Ctl3D = True
        Layout = blGlyphLeft
        Spacing = 4
        TransparentGlyph = True
        Gradient = False
        HotTrack = True
        Anchors = [akTop, akRight]
        Caption = #54869#51064
        TabOrder = 0
        OnClick = btOkClick
      end
      object btCancel: TXiButton
        Left = 312
        Top = 8
        Width = 75
        Height = 25
        ColorFace = 14737632
        ColorGrad = 3653375
        ColorDark = 11447982
        ColorLight = 16250871
        ColorBorder = 6447714
        ColorText = clBlack
        OverColorFace = 13619151
        OverColorGrad = clWhite
        OverColorDark = 7960953
        OverColorLight = 15658734
        OverColorBorder = 7697781
        OverColorText = clBlack
        DownColorFace = 13882323
        DownColorGrad = clWhite
        DownColorDark = 15329769
        DownColorLight = 8158332
        DownColorBorder = 5131854
        DownColorText = clBlack
        DisabledColorFace = 14869218
        DisabledColorGrad = clWhite
        DisabledColorDark = 14211288
        DisabledColorLight = 15395562
        DisabledColorBorder = 12895428
        DisabledColorText = clGray
        ColorFocusRect = 9079434
        ColorScheme = csSilver
        Ctl3D = True
        Layout = blGlyphLeft
        Spacing = 4
        TransparentGlyph = True
        Gradient = False
        HotTrack = True
        Anchors = [akTop, akRight]
        Caption = #52712#49548
        TabOrder = 1
        OnClick = btCancelClick
      end
    end
  end
  object tmStart: TTimer
    Interval = 200
    Left = 184
    Top = 132
  end
end
