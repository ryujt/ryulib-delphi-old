object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 217
  ClientWidth = 565
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
  object btDeleteFirstNodeOfList: TButton
    Left = 8
    Top = 8
    Width = 181
    Height = 25
    Caption = 'btDeleteFirstNodeOfList'
    TabOrder = 0
    OnClick = btDeleteFirstNodeOfListClick
  end
  object btDeleteFirstNodeOfLinkedList: TButton
    Left = 8
    Top = 39
    Width = 181
    Height = 25
    Caption = 'btDeleteFirstNodeOfLinkedList'
    TabOrder = 1
    OnClick = btDeleteFirstNodeOfLinkedListClick
  end
  object btDeleteLastNodeOfList: TButton
    Left = 8
    Top = 91
    Width = 181
    Height = 25
    Caption = 'btDeleteLastNodeOfList'
    TabOrder = 2
    OnClick = btDeleteLastNodeOfListClick
  end
  object btDeleteLastNodeOfLinkedList: TButton
    Left = 8
    Top = 122
    Width = 181
    Height = 25
    Caption = 'btDeleteLastNodeOfLinkedList'
    TabOrder = 3
    OnClick = btDeleteLastNodeOfLinkedListClick
  end
  object moMsg: TMemo
    Left = 207
    Top = 8
    Width = 350
    Height = 201
    ImeName = 'Microsoft IME 2010'
    ScrollBars = ssBoth
    TabOrder = 4
  end
end
