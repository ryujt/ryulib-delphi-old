object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 290
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ListView: TListView
    Left = 0
    Top = 0
    Width = 554
    Height = 290
    Align = alClient
    Columns = <
      item
      end
      item
        Width = 180
      end>
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = ListViewDblClick
  end
end
