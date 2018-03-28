object frmStat: TfrmStat
  Left = 329
  Top = 116
  BorderStyle = bsDialog
  Caption = 'Statistic'
  ClientHeight = 364
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object viewStat: TrkView
    Left = 0
    Top = 0
    Width = 313
    Height = 321
    Align = alClient
    ShowHint = True
    TabOrder = 0
    MultipleSelection = True
    HotTracking = True
    BorderStyle = bsNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    CellWidth = 0
    CellHeight = 17
    CellOffset = 3
    CellSpace = 0
    HeaderHeight = 0
    CenterView = True
    CellSelect = True
    Columns = '150, 150'
    ColorSel = 16750899
    ParentColor = False
    OnListPaint = viewStatListPaint
    ExplicitHeight = 288
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 321
    Width = 313
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    Color = 16316664
    ParentBackground = False
    TabOrder = 1
    object imgBottom: TImage
      Left = 0
      Top = 0
      Width = 313
      Height = 4
      Align = alTop
      ExplicitTop = 36
      ExplicitWidth = 411
    end
    object btnClose: TButton
      Left = 230
      Top = 10
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Close'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
end
