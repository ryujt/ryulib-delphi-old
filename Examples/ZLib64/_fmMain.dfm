object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 337
  ClientWidth = 635
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
  object BtnCompress: TButton
    Left = 552
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Compress'
    TabOrder = 0
    OnClick = BtnCompressClick
  end
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 538
    Height = 21
    ImeName = 'Microsoft IME 2010'
    TabOrder = 1
    Text = 
      #53581#49828#53944#47484' '#50517#52629#54644' BOA'#50836'. '#45224#54644#47932#44284' '#44552#44053#49328#51060' '#45811#44256#47560#47476#46020#47197', '#48512#52376#45784#51060' '#48372#50864#54616#49324' '#45768#45348#45208#46972#47564#49464'~ '#54861#51109#48120' 200'#47532' '#54868#47140#49328#44053 +
      '~ '#48513#54620#49324#46988' '#48513#54620#51004#47196' '#44600#51060' '#48372#51204#54616#49464
  end
  object BtnDecompress: TButton
    Left = 552
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Decompress'
    TabOrder = 2
    OnClick = BtnDecompressClick
  end
  object Edit2: TEdit
    Left = 8
    Top = 39
    Width = 538
    Height = 21
    ImeName = 'Microsoft IME 2010'
    ReadOnly = True
    TabOrder = 3
  end
end
