object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 573
  ClientWidth = 792
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
  object Splitter1: TSplitter
    Left = 0
    Top = 281
    Width = 792
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 202
    ExplicitWidth = 332
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 792
    Height = 240
    Align = alTop
    ImeName = 'Microsoft IME 2003'
    Lines.Strings = (
      'unit _fmMain;'
      ''
      'interface'
      ''
      'uses'
      '  TokenReplace,'
      
        '  Windows, Messages, SysUtils, Variants, Classes, Graphics, Cont' +
        'rols, Forms,'
      '  Dialogs, StdCtrls, ExtCtrls;'
      ''
      'type'
      '  TfmMain = class(TForm)'
      '    Memo1: TMemo;'
      '    Memo2: TMemo;'
      '    btExecute: TButton;'
      '    Panel1: TPanel;'
      '    Splitter1: TSplitter;'
      '    procedure FormCreate(Sender: TObject);'
      '    procedure btExecuteClick(Sender: TObject);'
      '  private'
      '    FTokenReplace : TTokenReplace;'
      '  public'
      '  end;'
      ''
      'var'
      '  fmMain: TfmMain;'
      ''
      'implementation'
      ''
      '{$R *.dfm}'
      ''
      'procedure TfmMain.btExecuteClick(Sender: TObject);'
      'begin'
      '  FTokenReplace.Input := Memo1.Text;'
      '  FTokenReplace.Replace('#39'begin'#39', '#39#49884#51089#39');'
      '  FTokenReplace.Replace('#39'end'#39', '#39#45149#39');'
      '  FTokenReplace.Replace('#39':Time'#39', '#39'@'#49884#44036#39');'
      '  FTokenReplace.Replace('#39':TimeLimit'#39', '#39'@'#49884#44036#51228#54620#39');'
      '  FTokenReplace.Execute;'
      '  Memo2.Text := FTokenReplace.Output;'
      'end;'
      ''
      'procedure TfmMain.FormCreate(Sender: TObject);'
      'begin'
      '  FTokenReplace := TTokenReplace.Create(Self);'
      
        '  FTokenReplace.WhiteSpace := '#39'~!#$%^&*()_+`-={}|[]\";'#39#39'<>?,./ '#39 +
        '#13#10#0;'
      'end;'
      ''
      'end.')
    ScrollBars = ssBoth
    TabOrder = 0
    ExplicitTop = 38
  end
  object Memo2: TMemo
    Left = 0
    Top = 284
    Width = 792
    Height = 289
    Align = alClient
    ImeName = 'Microsoft IME 2003'
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 792
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 204
    ExplicitTop = 428
    ExplicitWidth = 185
    object btExecute: TButton
      Left = 8
      Top = 6
      Width = 75
      Height = 25
      Caption = 'btExecute'
      TabOrder = 0
      OnClick = btExecuteClick
    end
  end
end
