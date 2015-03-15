object frmView: TfrmView
  Left = 634
  Top = 183
  BorderStyle = bsNone
  Caption = 'Preview'
  ClientHeight = 447
  ClientWidth = 560
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlPreview: TPanel
    Left = 0
    Top = 0
    Width = 560
    Height = 447
    Align = alClient
    BevelOuter = bvSpace
    TabOrder = 0
    object ieZoom: TImageEnView
      Left = 1
      Top = 1
      Width = 558
      Height = 445
      ParentCtl3D = False
      BorderStyle = bsNone
      ZoomFilter = rfFastLinear
      ScrollBars = ssNone
      DelayZoomFilter = True
      ImageEnVersion = '3.1.1'
      EnableInteractionHints = True
      Align = alClient
      TabOrder = 1
    end
    object iePreview: TImageEnView
      Left = 1
      Top = 1
      Width = 558
      Height = 445
      Cursor = 24
      HelpContext = -17
      ParentCtl3D = False
      BorderStyle = bsNone
      LegacyBitmap = False
      MouseInteract = [miScroll]
      AutoFit = True
      DelayZoomFilter = True
      OnSpecialKey = iePreviewSpecialKey
      ImageEnVersion = '3.1.1'
      EnableInteractionHints = True
      Align = alClient
      PopupMenu = popView
      TabOrder = 0
      OnDblClick = iePreviewDblClick
      OnMouseDown = iePreviewMouseDown
      OnMouseMove = iePreviewMouseMove
      OnMouseUp = iePreviewMouseUp
      object labInfo: TLabel
        Left = 3
        Top = 3
        Width = 4
        Height = 17
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        Visible = False
      end
    end
  end
  object popView: TPopupMenu
    OnPopup = popViewPopup
    Left = 8
    Top = 40
    object meuFull: TMenuItem
      Caption = 'Toggle full screen'
      OnClick = popFullScreenClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object popZoom: TMenuItem
      Caption = 'No Zoom'
      OnClick = popZoomClick
    end
    object popZoom1: TMenuItem
      Tag = 1
      Caption = 'Zoom x 1.5'
      OnClick = popZoomClick
    end
    object popZoom2: TMenuItem
      Tag = 2
      Caption = 'Zoom x 2'
      Checked = True
      OnClick = popZoomClick
    end
    object popZoom3: TMenuItem
      Tag = 3
      Caption = 'Zoom x 3'
      OnClick = popZoomClick
    end
  end
end
