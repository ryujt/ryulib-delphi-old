object fmDirectBitmap: TfmDirectBitmap
  Left = 0
  Top = 0
  Align = alClient
  BorderStyle = bsNone
  Caption = 'fmDirectBitmap'
  ClientHeight = 328
  ClientWidth = 570
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ScrollBox: TScrollBox
    Left = 0
    Top = 0
    Width = 570
    Height = 328
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 1
    Visible = False
  end
  object DXDraw: TDXDraw
    Left = 112
    Top = 40
    Width = 320
    Height = 240
    Hint = #54868#47732#51012' '#45908#48660#53364#47533' '#54616#49884#47732' '#50896#48376' '#49324#51060#51592#47196' '#48372#51077#45768#45796'.'
    AutoInitialize = False
    AutoSize = False
    Color = clBlack
    Display.FixedBitCount = True
    Display.FixedRatio = True
    Display.FixedSize = False
    Options = [doAllowReboot, do3D, doDirectX7Mode, doHardware]
    SurfaceHeight = 240
    SurfaceWidth = 320
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    Traces = <>
    OnDblClick = DXDrawDblClick
  end
  object Timer: TTimer
    Interval = 25
    OnTimer = on_Timer
    Left = 476
    Top = 24
  end
end
