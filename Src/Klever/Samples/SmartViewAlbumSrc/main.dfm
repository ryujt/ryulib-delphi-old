object frmMain: TfrmMain
  Left = 420
  Top = 131
  Caption = 'Project Mini Album'
  ClientHeight = 463
  ClientWidth = 815
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Label15: TLabel
    Left = 6
    Top = 27
    Width = 51
    Height = 15
    Caption = 'Filename '
    Font.Charset = ANSI_CHARSET
    Font.Color = clGrayText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label16: TLabel
    Left = 6
    Top = 43
    Width = 24
    Height = 15
    Caption = 'Date'
    Font.Charset = ANSI_CHARSET
    Font.Color = clGrayText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label17: TLabel
    Left = 6
    Top = 59
    Width = 20
    Height = 15
    Caption = 'Size'
    Font.Charset = ANSI_CHARSET
    Font.Color = clGrayText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label18: TLabel
    Left = 6
    Top = 76
    Width = 56
    Height = 15
    Caption = 'Image Size'
    Font.Charset = ANSI_CHARSET
    Font.Color = clGrayText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label19: TLabel
    Left = 6
    Top = 92
    Width = 34
    Height = 15
    Caption = 'Rating'
    Font.Charset = ANSI_CHARSET
    Font.Color = clGrayText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label20: TLabel
    Left = 6
    Top = 108
    Width = 41
    Height = 15
    Caption = 'Camera'
    Font.Charset = ANSI_CHARSET
    Font.Color = clGrayText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label21: TLabel
    Left = 6
    Top = 125
    Width = 37
    Height = 15
    Caption = 'Author'
    Font.Charset = ANSI_CHARSET
    Font.Color = clGrayText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label22: TLabel
    Left = 6
    Top = 141
    Width = 47
    Height = 15
    Caption = 'Exposure'
    Font.Charset = ANSI_CHARSET
    Font.Color = clGrayText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label23: TLabel
    Left = 6
    Top = 157
    Width = 40
    Height = 15
    Caption = 'Blender'
    Font.Charset = ANSI_CHARSET
    Font.Color = clGrayText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label24: TLabel
    Left = 6
    Top = 174
    Width = 18
    Height = 15
    Caption = 'ISO'
    Font.Charset = ANSI_CHARSET
    Font.Color = clGrayText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 815
    Height = 463
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 105
      Height = 105
    end
    object pnlBottom: TPanel
      Left = 0
      Top = 430
      Width = 815
      Height = 33
      Align = alBottom
      BevelOuter = bvNone
      Color = 16316664
      ParentBackground = False
      TabOrder = 0
      object imgBottom: TImage
        Left = 0
        Top = 0
        Width = 815
        Height = 3
        Align = alTop
      end
      object labInfo: TLabel
        Left = 16
        Top = 10
        Width = 3
        Height = 15
        Font.Charset = ANSI_CHARSET
        Font.Color = clGray
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object Panel1: TPanel
        Left = 644
        Top = 3
        Width = 171
        Height = 30
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        object tbSize: TTrackBar
          Left = 78
          Top = 0
          Width = 93
          Height = 30
          Align = alRight
          Max = 255
          Min = 31
          Frequency = 16
          Position = 127
          PositionToolTip = ptTop
          ShowSelRange = False
          TabOrder = 0
          TickMarks = tmTopLeft
          OnChange = tbSizeChange
        end
        object btnShow: TrkGlassButton
          Left = 18
          Top = 2
          Width = 24
          Height = 24
          AltFocus = False
          AltRender = False
          Color = clWhite
          ColorDown = clSilver
          ColorFrame = clGray
          DropDownAlignment = paLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Glossy = True
          GlossyLevel = 37
          LightHeight = 27
          ShadowStyle = ssNone
          TabOrder = 1
          TabStop = True
          TextAlign = taLeft
          Visible = False
          SingleBorder = True
        end
        object btnStyle: TrkGlassButton
          Tag = 1
          Left = 48
          Top = 2
          Width = 24
          Height = 24
          AltFocus = False
          AltRender = False
          Caption = 'Glass'
          Color = clWhite
          ColorDown = clSilver
          ColorFrame = clGray
          DropDownAlignment = paLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Glossy = True
          GlossyLevel = 37
          Images = ilGUImain
          LightHeight = 27
          ShadowStyle = ssNone
          TabOrder = 2
          TabStop = True
          TextAlign = taLeft
          OnClick = btnStyleClick
          SingleBorder = True
        end
      end
    end
    object pnlThumbs: TPanel
      Left = 0
      Top = 0
      Width = 815
      Height = 430
      Align = alClient
      BevelOuter = bvNone
      Color = clWindow
      ParentBackground = False
      TabOrder = 1
      object pnlTop: TPanel
        Left = 0
        Top = 0
        Width = 815
        Height = 62
        Align = alTop
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 0
        object imgGrd: TImage
          Left = 0
          Top = 59
          Width = 815
          Height = 3
          Align = alBottom
          OnClick = imgGrdClick
          OnMouseEnter = imgGrdMouseEnter
          OnMouseLeave = imgGrdMouseLeave
        end
        object labAlbum: TLabel
          Left = 0
          Top = 34
          Width = 815
          Height = 25
          Align = alBottom
          Alignment = taCenter
          AutoSize = False
          Caption = '< Add Title >'
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -15
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          OnClick = labAlbumClick
          OnMouseEnter = labAlbumMouseEnter
          OnMouseLeave = labAlbumMouseLeave
          ExplicitWidth = 411
        end
        object Image2: TImage
          Left = 0
          Top = 30
          Width = 815
          Height = 4
          Align = alBottom
          Visible = False
          ExplicitTop = 29
          ExplicitWidth = 575
        end
        object editTitle: TEdit
          Left = 72
          Top = 32
          Width = 81
          Height = 28
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          Visible = False
          OnExit = editTitleExit
          OnKeyDown = editTitleKeyDown
          OnKeyPress = editTitleKeyPress
        end
        object rkVistaPanel1: TrkVistaPanel
          Left = 0
          Top = 0
          Width = 815
          Height = 33
          Align = alTop
          BevelInner = bvNone
          BevelOuter = bvNone
          BottomLine = False
          BottomColor = 12615680
          Color1 = clWhite
          Color2 = 15724527
          Color3 = 7091712
          Color4 = clWhite
          ColorFrame = clSilver
          Frames = [frBottom]
          ParentBackground = False
          Style = vgSimple
          DesignSize = (
            815
            33)
          object btnMain: TrkGlassButton
            Left = 4
            Top = 4
            Width = 37
            Height = 24
            AltFocus = False
            AltRender = False
            Arrow = True
            Caption = ' '
            Color = clWhite
            ColorDown = clSilver
            ColorFocused = 8454143
            ColorFrame = clGray
            DropDownAlignment = paLeft
            DropDownMenu = popMain
            Font.Charset = ANSI_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = []
            Glossy = True
            GlossyLevel = 37
            ImageIndex = 6
            Images = ilGUImain
            LightHeight = 12
            ShadowStyle = ssNone
            TabOrder = 0
            TabStop = True
            TextAlign = taLeft
            SingleBorder = True
          end
          object btnGroup: TrkGlassButton
            Left = 41
            Top = 4
            Width = 75
            Height = 24
            AltFocus = False
            AltRender = False
            Arrow = True
            Caption = 'Group by'
            Color = clWhite
            ColorDown = clSilver
            ColorFocused = 8454143
            ColorFrame = clGray
            DropDownAlignment = paLeft
            DropDownMenu = popGroup
            Font.Charset = ANSI_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = []
            Glossy = True
            GlossyLevel = 37
            LightHeight = 12
            ShadowStyle = ssNone
            TabOrder = 1
            TabStop = True
            TextAlign = taLeft
            SingleBorder = True
          end
          object btnSort: TrkGlassButton
            Left = 116
            Top = 4
            Width = 64
            Height = 24
            AltFocus = False
            AltRender = False
            Arrow = True
            Caption = 'Sort by'
            Color = clWhite
            ColorDown = clSilver
            ColorFocused = 8454143
            ColorFrame = clGray
            DropDownAlignment = paLeft
            DropDownMenu = popSort
            Font.Charset = ANSI_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = []
            Glossy = True
            GlossyLevel = 37
            LightHeight = 12
            ShadowStyle = ssNone
            TabOrder = 2
            TabStop = True
            TextAlign = taLeft
            SingleBorder = True
          end
          object btnView: TrkGlassButton
            Left = 180
            Top = 4
            Width = 75
            Height = 24
            AltFocus = False
            AltRender = False
            Arrow = True
            Caption = 'View'
            Color = clWhite
            ColorDown = clSilver
            ColorFocused = 8454143
            ColorFrame = clGray
            DropDownAlignment = paLeft
            DropDownMenu = popViewThumbs
            DuoStyle = True
            Font.Charset = ANSI_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = []
            Glossy = True
            GlossyLevel = 37
            Images = ilGUImain
            LightHeight = 12
            ShadowStyle = ssNone
            TabOrder = 3
            TabStop = True
            TextAlign = taLeft
            OnClick = btnStyleClick
            SingleBorder = True
          end
          object editFilter: TEdit
            Left = 690
            Top = 4
            Width = 121
            Height = 23
            Anchors = [akTop, akRight]
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            TabOrder = 4
            TextHint = 'Search...'
            OnKeyDown = editFilterKeyDown
            OnKeyPress = editTitleKeyPress
          end
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 62
        Width = 815
        Height = 368
        Align = alClient
        BevelOuter = bvNone
        Caption = 'Panel2'
        TabOrder = 1
        object svMain: TrkSmartView
          Left = 0
          Top = 0
          Width = 575
          Height = 368
          Align = alClient
          BorderStyle = bsNone
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Pitch = fpVariable
          Font.Style = []
          HotTracking = True
          ParentFont = False
          ShowHint = True
          TabOrder = 0
          TabStop = True
          OnMouseDown = svMainMouseDown
          OnDblClick = svMainDblClick
          OnKeyDown = svMainKeyDown
          OnSelecting = svMainSelecting
          OnCellHit = svMainCellHit
          OnDividerHit = svMainDividerHit
          OnDividerPaint = svMainDividerPaint
          CellAutoAdj = True
          MultipleSelection = True
          CellSelect = True
          Color = clWhite
          ColorSel = 16750899
          object proOpen: TVistaProBar
            Left = 305
            Top = 146
            Width = 168
            Height = 17
            MarqueeSize = 40
            MarqueeFade = 30
            MarqueeSpeed = 25
            AltMode = False
            Percentage = False
            Position = 50
            ShowPosText = False
            Color = clLime
            Visible = False
          end
          object editView: TEdit
            Left = 3
            Top = 6
            Width = 127
            Height = 23
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Pitch = fpVariable
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            Visible = False
            OnExit = editViewExit
            OnKeyDown = editViewKeyDown
            OnKeyPress = editViewKeyPress
          end
        end
        object pnlInfo: TPanel
          Left = 575
          Top = 0
          Width = 240
          Height = 368
          Align = alRight
          BevelOuter = bvNone
          Color = clWindow
          Padding.Left = 8
          Padding.Right = 8
          ParentBackground = False
          TabOrder = 1
          object pnlImgInfo: TPanel
            Left = 8
            Top = 77
            Width = 224
            Height = 291
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 0
            DesignSize = (
              224
              291)
            object Label2: TLabel
              Left = 6
              Top = 86
              Width = 78
              Height = 20
              Caption = 'Information'
              Font.Charset = ANSI_CHARSET
              Font.Color = 9458722
              Font.Height = -15
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object imgInfoSplit: TImage
              Left = 0
              Top = 73
              Width = 223
              Height = 3
            end
            object Label4: TLabel
              Left = 6
              Top = 112
              Width = 51
              Height = 15
              Caption = 'Filename '
              Font.Charset = ANSI_CHARSET
              Font.Color = clGrayText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object Label6: TLabel
              Left = 6
              Top = 128
              Width = 24
              Height = 15
              Caption = 'Date'
              Font.Charset = ANSI_CHARSET
              Font.Color = clGrayText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object Label7: TLabel
              Left = 6
              Top = 144
              Width = 20
              Height = 15
              Caption = 'Size'
              Font.Charset = ANSI_CHARSET
              Font.Color = clGrayText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object Label8: TLabel
              Left = 6
              Top = 193
              Width = 41
              Height = 15
              Caption = 'Camera'
              Font.Charset = ANSI_CHARSET
              Font.Color = clGrayText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object Label9: TLabel
              Left = 6
              Top = 177
              Width = 34
              Height = 15
              Caption = 'Rating'
              Font.Charset = ANSI_CHARSET
              Font.Color = clGrayText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object Label10: TLabel
              Left = 6
              Top = 161
              Width = 56
              Height = 15
              Caption = 'Image Size'
              Font.Charset = ANSI_CHARSET
              Font.Color = clGrayText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object Label11: TLabel
              Left = 6
              Top = 210
              Width = 37
              Height = 15
              Caption = 'Author'
              Font.Charset = ANSI_CHARSET
              Font.Color = clGrayText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object Label12: TLabel
              Left = 6
              Top = 226
              Width = 47
              Height = 15
              Caption = 'Exposure'
              Font.Charset = ANSI_CHARSET
              Font.Color = clGrayText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object Label13: TLabel
              Left = 6
              Top = 242
              Width = 40
              Height = 15
              Caption = 'Blender'
              Font.Charset = ANSI_CHARSET
              Font.Color = clGrayText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object Label14: TLabel
              Left = 6
              Top = 259
              Width = 18
              Height = 15
              Caption = 'ISO'
              Font.Charset = ANSI_CHARSET
              Font.Color = clGrayText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object labFName: TLabel
              Left = 70
              Top = 112
              Width = 69
              Height = 15
              Caption = 'Not available'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object labFDate: TLabel
              Left = 70
              Top = 128
              Width = 69
              Height = 15
              Caption = 'Not available'
              Font.Charset = ANSI_CHARSET
              Font.Color = clGrayText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object labFSize: TLabel
              Left = 70
              Top = 144
              Width = 69
              Height = 15
              Caption = 'Not available'
              Font.Charset = ANSI_CHARSET
              Font.Color = clGrayText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object labISize: TLabel
              Left = 70
              Top = 161
              Width = 69
              Height = 15
              Caption = 'Not available'
              Font.Charset = ANSI_CHARSET
              Font.Color = clGrayText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object labCamera: TLabel
              Left = 70
              Top = 193
              Width = 69
              Height = 15
              Caption = 'Not available'
              Font.Charset = ANSI_CHARSET
              Font.Color = clGrayText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object labAuthor: TLabel
              Left = 70
              Top = 210
              Width = 69
              Height = 15
              Caption = 'Not available'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object labExp: TLabel
              Left = 70
              Top = 226
              Width = 69
              Height = 15
              Caption = 'Not available'
              Font.Charset = ANSI_CHARSET
              Font.Color = clGrayText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object labBlender: TLabel
              Left = 70
              Top = 242
              Width = 69
              Height = 15
              Caption = 'Not available'
              Font.Charset = ANSI_CHARSET
              Font.Color = clGrayText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object labISO: TLabel
              Left = 70
              Top = 259
              Width = 69
              Height = 15
              Caption = 'Not available'
              Font.Charset = ANSI_CHARSET
              Font.Color = clGrayText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object imgRating: TImage
              Left = 70
              Top = 178
              Width = 72
              Height = 13
              Anchors = [akRight, akBottom]
            end
            object Label3: TLabel
              Left = 6
              Top = 20
              Width = 52
              Height = 20
              Caption = 'Caption'
              Font.Charset = ANSI_CHARSET
              Font.Color = 9458722
              Font.Height = -15
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object labCap: TLabel
              Left = 6
              Top = 46
              Width = 211
              Height = 20
              AutoSize = False
              Caption = 'Not available'
              EllipsisPosition = epEndEllipsis
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
              OnClick = labCapClick
              OnMouseEnter = labAlbumMouseEnter
              OnMouseLeave = labAlbumMouseLeave
            end
            object editInfo: TEdit
              Left = 3
              Top = 8
              Width = 217
              Height = 23
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
              TabOrder = 0
              Visible = False
              OnExit = editViewExit
              OnKeyDown = editInfoKeyDown
              OnKeyPress = editViewKeyPress
            end
          end
          object Panel3: TPanel
            Left = 8
            Top = 0
            Width = 224
            Height = 77
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              224
              77)
            object Label1: TLabel
              Left = 6
              Top = 20
              Width = 31
              Height = 20
              Caption = 'Tags'
              Font.Charset = ANSI_CHARSET
              Font.Color = 9458722
              Font.Height = -15
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object labAddTags: TLabel
              Left = 6
              Top = 46
              Width = 211
              Height = 20
              AutoSize = False
              Caption = 'Not available'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
              OnClick = labAddTagsClick
              OnMouseEnter = labAlbumMouseEnter
              OnMouseLeave = labAlbumMouseLeave
            end
            object svKeyWords: TrkSmartView
              AlignWithMargins = True
              Left = 4
              Top = 66
              Width = 216
              Height = 11
              Margins.Left = 0
              Margins.Top = 66
              Margins.Right = 0
              Margins.Bottom = 0
              Anchors = [akLeft, akTop, akRight, akBottom]
              BorderStyle = bsNone
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 12615680
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              HotTracking = True
              ParentFont = False
              ShowHint = True
              TabOrder = 0
              OnClick = svKeyWordsClick
              OnListPaint = svKeyWordsListPaint
              CellWidth = 0
              CellHeight = 19
              CellOffset = 3
              CellSpace = 0
              MultipleSelection = True
              CellSelect = True
              Columns = '189, 20'
              ColorSel = 16750899
            end
            object editKeywords: TEdit
              Left = 3
              Top = 43
              Width = 217
              Height = 23
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
              Visible = False
              OnExit = editViewExit
              OnKeyDown = editKeywordsKeyDown
              OnKeyPress = editViewKeyPress
            end
          end
        end
      end
    end
  end
  object dlgSave: TSaveDialog
    Left = 16
    Top = 216
  end
  object dlgOpen: TOpenDialog
    Left = 16
    Top = 272
  end
  object appMain: TApplicationEvents
    OnDeactivate = appMainDeactivate
    Left = 88
    Top = 104
  end
  object timerSearch: TTimer
    Enabled = False
    Interval = 100
    OnTimer = timerSearchTimer
    Left = 16
    Top = 328
  end
  object fxSupport: TrkSupFileExt
    Filter = 'jpg;jpeg;png;tiff;tif;bmp;tga'
    Options = [extResolveLink]
    Left = 16
    Top = 104
  end
  object popMain: TPopupMenu
    Images = ilGUImain
    Left = 360
    Top = 80
    object popClose: TMenuItem
      Caption = 'Close'
      OnClick = btnCloseClick
    end
    object popOpen: TMenuItem
      Caption = 'Open'
      ImageIndex = 7
      OnClick = btnOpenClick
    end
    object popSave: TMenuItem
      Caption = 'Save As...'
      ImageIndex = 9
      OnClick = btnSaveAsClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object popAdd: TMenuItem
      Caption = 'Add Images...'
      OnClick = btnAddImagesClick
    end
    object popDelete: TMenuItem
      Caption = 'Delete Images...'
      OnClick = btnDelImagesClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Exit1: TMenuItem
      Caption = 'Exit'
      OnClick = Exit1Click
    end
  end
  object popGroup: TPopupMenu
    Left = 360
    Top = 136
    object grpNone: TMenuItem
      Caption = 'None'
      Checked = True
      OnClick = grpNoneClick
    end
    object grpPath: TMenuItem
      Tag = 1
      Caption = 'Path'
      OnClick = grpNoneClick
    end
    object grpRating: TMenuItem
      Tag = 2
      Caption = 'Rating'
      OnClick = grpNoneClick
    end
    object grpOrientation: TMenuItem
      Tag = 3
      Caption = 'Orientation'
      OnClick = grpNoneClick
    end
    object grpHue: TMenuItem
      Tag = 4
      Caption = 'Color Tone (Hue)'
      OnClick = grpNoneClick
    end
    object grpMatch: TMenuItem
      Tag = 5
      Caption = 'Color Match'
      OnClick = grpNoneClick
    end
  end
  object popSort: TPopupMenu
    OnPopup = popSortPopup
    Left = 416
    Top = 80
    object sortAuto: TMenuItem
      Tag = -1
      Caption = 'Automatic'
      Checked = True
      OnClick = sortNameClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object sortDate: TMenuItem
      Tag = 2
      Caption = 'Date Modified'
      OnClick = sortNameClick
    end
    object sortSize: TMenuItem
      Tag = 1
      Caption = 'File Size'
      OnClick = sortNameClick
    end
    object sortPSize: TMenuItem
      Tag = 7
      Caption = 'Image Size'
      OnClick = sortNameClick
    end
    object sortRating: TMenuItem
      Tag = 6
      Caption = 'Rating'
      OnClick = sortNameClick
    end
    object sortText: TMenuItem
      Tag = 5
      Caption = 'Caption'
      OnClick = sortNameClick
    end
    object sortName: TMenuItem
      Caption = 'File Name'
      OnClick = sortNameClick
    end
    object sortPath: TMenuItem
      Tag = 9
      Caption = 'File Path'
      OnClick = sortNameClick
    end
    object sortType: TMenuItem
      Tag = 8
      Caption = 'File Type'
      OnClick = sortNameClick
    end
    object sortHue: TMenuItem
      Tag = 3
      Caption = 'Color Match'
      OnClick = sortNameClick
    end
    object sortMatch: TMenuItem
      Tag = 4
      Caption = 'Color Tone'
      OnClick = sortNameClick
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object sortAsc: TMenuItem
      Tag = 99
      Caption = '&Ascending'
      Checked = True
      OnClick = sortNameClick
    end
    object sortDes: TMenuItem
      Tag = 100
      Caption = '&Descending'
      OnClick = sortNameClick
    end
  end
  object popViewThumbs: TPopupMenu
    OnPopup = popViewThumbsPopup
    Left = 432
    Top = 136
    object popInfoPnl: TMenuItem
      Caption = 'Information'
      ShortCut = 118
      OnClick = popInfoPnlClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object viewThumbnails: TMenuItem
      Caption = 'Thumbnails'
      OnClick = viewThumbnailsClick
    end
    object viewModified: TMenuItem
      Tag = 1
      Caption = 'Thumbnails with Date Modified'
      OnClick = viewThumbnailsClick
    end
    object viewFileSize: TMenuItem
      Tag = 2
      Caption = 'Thumbnails with File Size'
      OnClick = viewThumbnailsClick
    end
    object viewImageSize: TMenuItem
      Tag = 3
      Caption = 'Thumbnails with Image Size'
      OnClick = viewThumbnailsClick
    end
    object viewRating: TMenuItem
      Tag = 4
      Caption = 'Thumbnails with Rating'
      OnClick = viewThumbnailsClick
    end
    object viewText: TMenuItem
      Tag = 5
      Caption = 'Thumbnails with Caption'
      OnClick = viewThumbnailsClick
    end
    object viewFileName: TMenuItem
      Tag = 6
      Caption = 'Thumbnails with File Name'
      Checked = True
      OnClick = viewThumbnailsClick
    end
    object viewDetails: TMenuItem
      Tag = 7
      Caption = 'Details'
      OnClick = viewThumbnailsClick
    end
  end
  object ilGUImain: TImageList
    ColorDepth = cd32Bit
    Left = 16
    Top = 160
    Bitmap = {
      494C01010A002800440010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009A4D4D009A4D4D009A4D4D009A4D4D009A4D4D009A4D
      4D009A4D4D009A4D4D009A4D4D009A4D4D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000006E3637006E36
      37006E3637006E3637006E3637006E3637006E3637006E3637006E3637006E36
      37006E3637006E3637006E3637000000000000000000000000006E3637006E36
      37006E3637006E3637009A4D4D00FFFFFF00FFD2D200FFD2D200FFD2D200FFD2
      D200FFD2D200FFC0C000FF9B9B009A4D4D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080584800CCA08F00C898
      8800C48F800050413500C3B4AB00C6BBB300CAC1BC00CEC8C400564D4800A14B
      41009C423900983931006E363700000000000000000080584800CCA08F00C898
      8800C48F8000504135009A4D4D00FFFFFF00808080006332320080808000C0C0
      C0006332320063323200FFC0C0009A4D4D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080584800D0A89600CCA0
      8F00C8988800715B4B00504135008D786800D0D0D000D0D0D0008F7F7300A654
      4A00A14B41009C4239006E363700000000000000000080584800D0A89600CCA0
      8F00C8988800715B4B009A4D4D00FFFFFF00FFFFFF0063323200FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFD2D2009A4D4D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080584800D4B09E00D0A8
      9600CCA08F00715B4B000000000050413500E9D9CE00ECDDD40085746700AA5D
      5200A6544A00A14B41006E363700000000000000000080584800D4B09E00D0A8
      9600CCA08F00715B4B009A4D4D00FFFFFF00FFFFFF0063323200FFFFFF00C0C0
      C0006332320063323200FFD2D2009A4D4D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080584800D8B8A500D4B0
      9E00D0A89600715B4B00715B4B00715B4B00715B4B00483A2F007D6A5B00AF66
      5A00AA5D5200A6544A006E363700000000000000000080584800D8B8A500D4B0
      9E00D0A89600715B4B009A4D4D00FFFFFF00FFFFFF0063323200FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFD2D2009A4D4D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080584800DCC0AC00D8B8
      A500D4B09E00D0A89600CCA08F00C8988800C48F8000BD817300B8786B00B46F
      6300AF665A00AA5D52006E363700000000000000000080584800DCC0AC00D8B8
      A500D4B09E00D0A896009A4D4D00FFFFFF00808080006332320080808000C0C0
      C0006332320063323200FFD2D2009A4D4D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080584800E0C8B400DCC0
      AC00A0582800A0582800A0582800A0582800A0582800A0582800A0582800A058
      2800B46F6300AF665A006E363700000000000000000080584800E0C8B400DCC0
      AC00A0582800A05828009A4D4D00FFC0C000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF009A4D4D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080584800E6D4BF00B088
      7000F8F8E300F8F8DE00F8F8E100F8F8DC00F8F8CE00F8F8C400F8F8C800F8F8
      C800A0582800B46F63006E363700000000000000000080584800E6D4BF00B088
      7000F8F8E300F8F8DE009A4D4D009A4D4D009A4D4D009A4D4D009A4D4D009A4D
      4D009A4D4D009A4D4D009A4D4D009A4D4D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080584800EADCC600B088
      7000F8F8F100F8F8E600F8F8DC00F8F8E100F8F8D900F8F8CE00F8F8C400F8F8
      C800A0582800B8786B006E363700000000000000000080584800EADCC600B088
      7000F8F8F100F8F8E600F8F8DC00F8F8E100F8F8D900F8F8CE00F8F8C400F8F8
      C800A0582800B8786B006E363700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080584800EEE4CD00B088
      7000F8F8FB00F8F8F100F8F8E600F8F8DC00F8F8E300F8F8D900F8F8CE00F8F8
      C400A0582800BD8173006E363700000000000000000080584800EEE4CD00B088
      7000F8F8FB00F8F8F100F8F8E600F8F8DC00F8F8E300F8F8D900F8F8CE00F8F8
      C400A0582800BD8173006E363700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080584800F2ECD500B088
      7000F8F8F800F8F8FB00F8F8F100F8F8E600F8F8DC00F8F8E300F8F8D900F8F8
      CE00A0582800C48F80006E363700000000000000000080584800F2ECD500B088
      7000F8F8F800F8F8FB00F8F8F100F8F8E600F8F8DC00F8F8E300F8F8D900F8F8
      CE00A0582800C48F80006E363700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080584800F6F4DC00B088
      7000F8F8F800F8F8F800F8F8FB00F8F8F100F8F8E600F8F8DE00F8F8E100F8F8
      DC00A0582800A05828006E363700000000000000000080584800F6F4DC00B088
      7000F8F8F800F8F8F800F8F8FB00F8F8F100F8F8E600F8F8DE00F8F8E100F8F8
      DC00A0582800A05828006E363700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080584800F8F8E000B088
      7000F8F8F800F8F8F800F8F8F800F8F8FB00F8F8F100F8F8E300F8F8DE00F8F8
      E100A0582800F8F8E0006E363700000000000000000080584800F8F8E000B088
      7000F8F8F800F8F8F800F8F8F800F8F8FB00F8F8F100F8F8E300F8F8DE00F8F8
      E100A0582800F8F8E0006E363700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008058480080584800B088
      7000805848008058480080584800805848008058480080584800805848008058
      480080584800805848006E36370000000000000000008058480080584800B088
      7000805848008058480080584800805848008058480080584800805848008058
      480080584800805848006E363700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B5848400B584
      8400B5848400B5848400B5848400B5848400B5848400B5848400B5848400B584
      8400B5848400B5848400B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000070707000707070007070
      7000707070007070700070707000000000000000000070707000707070007070
      7000707070007070700070707000000000000000000070707000707070007070
      7000707070000000000070707000707070007070700070707000000000007070
      7000707070007070700070707000000000000000000000000000C6A59C00FFEF
      D600F7E7C600F7DEBD00F7DEB500F7D6AD00F7D6A500EFCE9C00EFCE9400EFCE
      9400EFCE9400F7D69C00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000707070009C9C9C009C9C
      9C009C9C9C009C9C9C00707070000000000000000000707070009C9C9C009C9C
      9C009C9C9C009C9C9C00707070000000000000000000707070009C9C9C009C9C
      9C007070700000000000707070009C9C9C009C9C9C0070707000000000007070
      70009C9C9C009C9C9C0070707000000000000000000000000000C6A59C00FFEF
      D600F7E7CE00F7DEC600F7DEBD00F7D6B500F7D6A500EFCE9C00EFCE9C00EFCE
      9400EFCE9400EFCE9C00B5848400000000008592990000609000006090000060
      9000006090000060900000609000006090000060900000609000006090000060
      900000000000000000000000000000000000000000009C9C9C00B6B6B600B6B6
      B600B6B6B600B6B6B6009C9C9C0000000000000000009C9C9C00B6B6B600B6B6
      B600B6B6B600B6B6B6009C9C9C0000000000000000009C9C9C00B6B6B600B6B6
      B6009C9C9C00000000009C9C9C00B6B6B600B6B6B6009C9C9C00000000009C9C
      9C00B6B6B600B6B6B6009C9C9C00000000000000000000000000C6ADA500FFEF
      E700F7E7D600F7E7CE00F7DEC600F7DEBD00F7D6B500F7D6AD00EFCE9C00EFCE
      9C00EFCE9400EFCE9C00B584840000000000859299008592990026A1D1000691
      CD000691CD000691CD000691CD000691CD000691CD000691CD000691CD000060
      900000000000000000000000000000000000000000009C9C9C009C9C9C009C9C
      9C009C9C9C009C9C9C009C9C9C0000000000000000009C9C9C009C9C9C009C9C
      9C009C9C9C009C9C9C009C9C9C0000000000000000009C9C9C009C9C9C009C9C
      9C009C9C9C00000000009C9C9C009C9C9C009C9C9C009C9C9C00000000009C9C
      9C009C9C9C009C9C9C009C9C9C00000000000000000000000000C6ADA500FFF7
      E700F7E7D600F7E7CE00F7E7C600F7DEC600F7DEB500F7D6B500F7D6AD00EFCE
      9C00EFCE9C00EFCE9400B584840000000000859299008592990078BFD10061DA
      F7005AD5F50054D1F3004CCBF00044C7EE003DC2ED0037BEEB0030B9E9000691
      CD00006090000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CEB5AD00FFFF
      F700FFEFE700F7E7D600F7E7D600F7E7CE00F7DEC600F7DEBD00F7D6B500F7D6
      AD00EFCE9C00EFCE9C00B5848400000000008592990064DBF800859299006ED4
      ED0063DAF7005DD7F60055D1F3004DCDF10046C8EF003FC4ED0037BFEB0029B5
      E700006090000000000000000000000000000000000070707000707070007070
      7000707070007070700070707000000000000000000070707000707070007070
      7000707070007070700070707000000000000000000070707000707070007070
      7000707070000000000070707000707070007070700070707000000000007070
      7000707070007070700070707000000000000000000000000000D6B5AD00FFFF
      FF00FFF7EF00FFEFE700F7E7D600F7E7CE00F7E7C600F7DEC600F7DEBD00F7D6
      AD00F7D6A500F7D6A500B584840000000000859299007CE8F8008592990074C2
      D50067DDF90065DCF8005DD6F60055D1F3004ECDF10046C8EF003FC4ED0037BF
      EB000691CD0000609000000000000000000000000000707070009C9C9C009C9C
      9C009C9C9C009C9C9C00707070000000000000000000707070009C9C9C009C9C
      9C009C9C9C009C9C9C00707070000000000000000000707070009C9C9C009C9C
      9C007070700000000000707070009C9C9C009C9C9C0070707000000000007070
      70009C9C9C009C9C9C0070707000000000000000000000000000D6BDB500FFFF
      FF00FFF7F700FFF7EF00FFEFDE00F7E7D600F7E7CE00F7E7C600F7DEC600F7DE
      BD00F7D6B500F7D6AD00B5848400000000008592990086EEF80064DBF8008592
      99006FD4ED0067DDF90064DBF8005DD6F60055D1F3004DCDF10046C8EF003FC4
      ED0030B6E500006090000000000000000000000000009C9C9C00B6B6B600B6B6
      B600B6B6B600B6B6B6009C9C9C0000000000000000009C9C9C00B6B6B600B6B6
      B600B6B6B600B6B6B6009C9C9C0000000000000000009C9C9C00B6B6B600B6B6
      B6009C9C9C00000000009C9C9C00B6B6B600B6B6B6009C9C9C00000000009C9C
      9C00B6B6B600B6B6B6009C9C9C00000000000000000000000000D6BDB500FFFF
      FF00FFFFFF00FFF7F700FFF7EF00FFEFE700F7E7D600F7E7CE00F7DEC600F7DE
      BD00F7DEB500F7DEB500B5848400000000008592990090F4F80086EEF8008592
      990073C4D80067DCF80067DDF90065DCF8005ED7F60056D2F4004ECDF10047C9
      EF003AC1EB000691CD000060900000000000000000009C9C9C009C9C9C009C9C
      9C009C9C9C009C9C9C009C9C9C0000000000000000009C9C9C009C9C9C009C9C
      9C009C9C9C009C9C9C009C9C9C0000000000000000009C9C9C009C9C9C009C9C
      9C009C9C9C00000000009C9C9C009C9C9C009C9C9C009C9C9C00000000009C9C
      9C009C9C9C009C9C9C009C9C9C00000000000000000000000000DEBDB500FFFF
      FF00FFFFFF00FFFFFF00FFF7F700FFEFE700FFEFDE00F7E7D600F7E7CE00F7DE
      C600F7DEC600F7D6B500B5848400000000008592990094F5F80090F4F80064DB
      F800859299008592990085929900859299008592990085929900859299008592
      9900859299008592990085929900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DEC6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFEFE700FFEFDE00FFEFDE00FFEF
      D600E7DEC600C6BDAD00B5848400000000008592990094F5F80094F5F80090F4
      F80064DBF80064DBF80064DBF80064DBF80064DBF80064DBF80064DBF80064DB
      F800859299000000000000000000000000000000000070707000707070007070
      7000707070007070700070707000000000000000000070707000707070007070
      7000707070007070700070707000000000000000000070707000707070007070
      7000707070000000000070707000707070007070700070707000000000007070
      7000707070007070700070707000000000000000000000000000E7C6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF7EF00FFF7EF00F7E7D600C6A5
      9400B5948C00B58C8400B5848400000000008592990094F5F80094F5F80094F5
      F80090F4F80064DBF80085929900859299008592990085929900859299008592
      99009830000098300000983000009830000000000000707070009C9C9C009C9C
      9C009C9C9C009C9C9C00707070000000000000000000707070009C9C9C009C9C
      9C009C9C9C009C9C9C00707070000000000000000000707070009C9C9C009C9C
      9C007070700000000000707070009C9C9C009C9C9C0070707000000000007070
      70009C9C9C009C9C9C0070707000000000000000000000000000E7C6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700E7CECE00BD8C
      7300EFB57300EFA54A00C6846B00000000008592990094F5F80094F5F80094F5
      F80094F5F8008592990000000000000000000000000000000000000000000000
      00000000000098300000E4A5820098300000000000009C9C9C00B6B6B600B6B6
      B600B6B6B600B6B6B6009C9C9C0000000000000000009C9C9C00B6B6B600B6B6
      B600B6B6B600B6B6B6009C9C9C0000000000000000009C9C9C00B6B6B600B6B6
      B6009C9C9C00000000009C9C9C00B6B6B600B6B6B6009C9C9C00000000009C9C
      9C00B6B6B600B6B6B6009C9C9C00000000000000000000000000EFCEBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E7D6CE00C694
      7B00FFC67300CE94730000000000000000000000000085929900859299008592
      9900859299000000000000000000000000000000000000000000000000000000
      000000000000BE713C009830000098300000000000009C9C9C009C9C9C009C9C
      9C009C9C9C009C9C9C009C9C9C0000000000000000009C9C9C009C9C9C009C9C
      9C009C9C9C009C9C9C009C9C9C0000000000000000009C9C9C009C9C9C009C9C
      9C009C9C9C00000000009C9C9C009C9C9C009C9C9C009C9C9C00000000009C9C
      9C009C9C9C009C9C9C009C9C9C00000000000000000000000000E7C6B500FFF7
      F700FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00E7CECE00C694
      7B00CE9C84000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000098300000BE713C000000
      0000BE713C009830000000000000983000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7C6B500EFCE
      B500EFCEB500EFCEB500EFCEB500E7C6B500E7C6B500EFCEB500D6BDB500BD84
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000983000009830
      0000983000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AC9E80CCC2B3
      91E82D2921360000000000000000000000004E473B5ECCBB98F4A6997CC50000
      0000000000000000000000000000000000000000000000000000075E90CC066A
      A3E8001725360000000000000000000000000129405E046EABF4065A8BC50000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AC9D81CBE7D6
      B5FFCFBE9AF66D6451823D382D499C8F74BAD0C09DF4E6D6B4FF978B71B40000
      0000000000000000000000000000000000000000000000000000085F91CB42A2
      D1FF0771ACF6033A5B82001F3249055684BA147AB2F440A1D0FF07537FB40000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000068604E7DE2D1
      B2FAF9EBCEFFDCCDABF7D0C09EEEE6D6B7FBFDEFD4FFD5C4A3F3463F34540000
      00000000000000000000000000000000000000000000000000000137567D42A0
      CDFA89D7F3FF3695C4F71F81B4EE4FAAD3FB98E2FAFF2587BAF300243A540000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000322E253CCFBF
      9DF0FEF0D5FFFEF0D5FFFEF0D5FFFEF0D5FFFEF0D5FFD1C09BF912110E160000
      0000000000000000000000000000000000000000000000000000001A293C197C
      B2F09CE5FCFF9CE5FCFF9CE5FCFF9CE5FCFF9CE5FCFF0470AEF900090F160000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000EFEFEF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002D292136CFBE
      9DF1FDF2DDFFFDF2DDFFFDF2DDFFFDF2DDFFFDF2DDFFCEBE9CF428251E300000
      000000000000000000000000000000000000000000000000000000172536167A
      B0F1B5EBFBFFB5EBFBFFB5EBFBFFB5EBFBFFB5EBFBFF1075AFF4001521300000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A8A8A800A8A8A800EFEF
      EF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000808060AA29579C0E4D6
      BDFAFDF5E5FFFDF5E5FFFDF5E5FFFDF5E5FFFDF5E5FFE9DDC5FBBFB08FE31B19
      142100000000000000000000000000000000000000000003070A065887C062AB
      D0FACEF1FAFFCEF1FAFFCEF1FAFFCEF1FAFFCEF1FAFF76B9D7FB0868A0E3000D
      1721000000000000000000000000000000000000000080808000000000000000
      0000000000000000000080808000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A8A8A800EFEFEF00A8A8
      A800EFEFEF000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000211F1829BCAE8EE1DBCFB3F7FBF6
      EBFFFCF7EDFFFCF7EDFFFCF7EDFFFCF7EDFFFCF7EDFFFCF7EDFFE9DFC9FBCABA
      97F12F2B223800000000000000000000000000111C2909679EE14F9AC2F7E3F4
      F7FFE8F7F9FFE8F7F9FFE8F7F9FFE8F7F9FFE8F7F9FFE8F7F9FF89BED7FB096E
      A9F10018273800000000000000000000000000000000DBDBDB00000000004040
      4000404040004040400000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A8A8A80000000000EFEF
      EF00A8A8A800EFEFEF0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CEBD99F6E1D3B6FEF1EAD9FFF5EE
      E0FFFBF7EDFFFCF8EFFFFCF8EFFFFCF8EFFFFAF5EAFFEEE5D1FFEAE0CAFFE0D2
      B4FFCCBB98F4000000000000000000000000026DABF64A9BC7FEADD3E5FFC2DF
      ECFFE7F5F7FFEDF8F9FFEDF8F9FFEDF8F9FFE0F1F5FF97C6DEFF81BAD8FF4296
      C5FF046DAAF40000000000000000000000000000000000000000DBDBDB000000
      0000404040004040400000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A8A8A800000000000000
      0000EFEFEF00A8A8A800EFEFEF00000000000000000000000000000000000000
      000000000000000000000000000000000000685F4D7CA09279BDAC9F82CBBDAE
      8EE0D0C09DF6F5EEDFFFFCF8EFFFF7F1E4FFD1C09DF79B8E75B77E7460957E74
      6095564F3F670000000000000000000000000237577C0E5A86BD0F6191CB0C69
      9FE01174AEF6BFDEEBFFEDF8F9FFCDE6EFFF0E74AFF70C5782B7094769950947
      6995002D4867000000000000000000000000000000000000000000000000DBDB
      DB00000000004040400000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A8A8A80000000000EFEF
      EF00A8A8A800EFEFEF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00004A443759CFC1A0F3FCF8EFFFD8CAAFF5665D4B7900000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000263D591B7BB1F3EDF8F9FF4594BFF50135547900000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000DBDBDB000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A8A8A800EFEFEF00A8A8
      A800EFEFEF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6B594EBF3ECDCFFD1C09BF90C0A090E00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000096BA5EBB7D9E8FF0570AEF900050A0E00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000DBDBDB0080808000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A8A8A800A8A8A800EFEF
      EF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007C735D94DACBAAFE998B73B50000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000054468942686BBFE09547FB50000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000EFEFEF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000201D1726BBAC8CDF2B2820340000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000010192606659BDF001624340000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF00FFFFFC0000000000C001C00000000000
      8001800000000000800180000000000080018000000000008001800000000000
      8001800000000000800180000000000080018000000000008001800100000000
      8001800100000000800180010000000080018001000000008001800100000000
      8001800100000000FFFFFFFF00000000FFFFFFFFC001FFFF81818421C001FFFF
      81818421C001000F81818421C001000F81818421C0010007FFFFFFFFC0010007
      81818421C001000381818421C001000381818421C001000181818421C0010001
      FFFFFFFFC001000781818421C001000081818421C00103F881818421C00387F8
      81818421C007FF92FFFFFFFFC00FFFC7FFFFFFFF00000000FFFFFFFF00000000
      FFFFFFFF00000000FFFFFFFF00000000FFFFFFFF00000000FFFFFFFF00000000
      FFFFFFFF00000000FFFF9FFF00000000FFFF8FFF0000000081FF87FF00000000
      81FFA3FF00000000C1FFB1FF00000000E1FFA3FF00000000F1FF87FF00000000
      F9FF8FFF00000000FFFF9FFF0000000000000000000000000000000000000000
      000000000000}
  end
  object XPManifest1: TXPManifest
    Left = 88
    Top = 160
  end
end
