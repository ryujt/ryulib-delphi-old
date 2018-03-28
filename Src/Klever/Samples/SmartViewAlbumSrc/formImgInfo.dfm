object frmInfo: TfrmInfo
  Left = 329
  Top = 116
  BorderIcons = [biMinimize, biMaximize]
  Caption = 'Information'
  ClientHeight = 460
  ClientWidth = 389
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
    Width = 389
    Height = 417
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
    OnDblClick = viewStatDblClick
    CellWidth = 0
    CellHeight = 19
    CellOffset = 8
    CellSpace = 0
    CellSelect = True
    HeaderVisible = True
    Columns = '150, 200'
    ColorSel = 16750899
    ParentColor = False
    OnHeaderPaint = viewStatHeaderPaint
    OnListPaint = viewStatListPaint
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 417
    Width = 389
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    Color = 16316664
    ParentBackground = False
    TabOrder = 1
    DesignSize = (
      389
      43)
    object imgBottom: TImage
      Left = 0
      Top = 0
      Width = 389
      Height = 4
      Align = alTop
      ExplicitTop = 36
      ExplicitWidth = 411
    end
    object btnClose: TButton
      Left = 306
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Close'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
end
