object frmAbout: TfrmAbout
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'About'
  ClientHeight = 366
  ClientWidth = 477
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  GlassFrame.Enabled = True
  GlassFrame.Top = 10
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    477
    366)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 82
    Top = 26
    Width = 316
    Height = 29
    Anchors = []
    Caption = 'The TrkAeroTabbed Text Editor!'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 9458722
    Font.Height = -24
    Font.Name = 'Calibri'
    Font.Style = [fsBold]
    ParentFont = False
    ExplicitTop = 25
  end
  object Label1: TLabel
    Left = 92
    Top = 116
    Width = 143
    Height = 21
    Caption = 'Components Used:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 9458722
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 241
    Top = 116
    Width = 105
    Height = 126
    Caption = 
      'TrkAeroTab'#13'TrkVistaPanel'#13'TrkGlassButton'#13'RichEdit'#13'Groupbox'#13'Status' +
      'bar'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 9458722
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 81
    Top = 50
    Width = 317
    Height = 21
    Caption = 'Demo by: Jeff Hansen - www.Jeffijoe.com'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 9458722
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 30
    Top = 77
    Width = 418
    Height = 21
    Caption = 'RK Components by: Roy M. Klever - www.rmklever.com'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 9458722
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label6: TLabel
    Left = 106
    Top = 248
    Width = 121
    Height = 21
    Caption = 'Free Icons from:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 9458722
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label7: TLabel
    Left = 241
    Top = 248
    Width = 212
    Height = 63
    Caption = 'www.YusukeKamiyamane.com'#13'www.VisualPharm.com'#13'www.FatCow.com'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 9458722
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object btnNew: TrkGlassButton
    Left = 138
    Top = 318
    Width = 227
    Height = 38
    AltFocus = False
    AltRender = False
    Caption = 'Awesome!'
    Color = clWhite
    ColorDown = clSilver
    ColorFocused = clWhite
    ColorFrame = clGray
    DropDownAlignment = paLeft
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    Glossy = True
    GlossyLevel = 37
    ImageIndex = 0
    LightHeight = 12
    ShadowStyle = ssNone
    TabOrder = 0
    TabStop = True
    TextAlign = taCenter
    OnClick = btnNewClick
    SingleBorder = True
  end
end
