object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'Calss To DLL'
  ClientHeight = 562
  ClientWidth = 784
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
  object Splitter1: TSplitter
    Left = 0
    Top = 213
    Width = 784
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitLeft = 16
    ExplicitTop = 164
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 387
    Width = 784
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 388
    ExplicitWidth = 174
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 784
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btExecute: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Execute'
      TabOrder = 0
      OnClick = btExecuteClick
    end
  end
  object moSrc: TMemo
    Left = 0
    Top = 41
    Width = 784
    Height = 172
    Align = alTop
    ImeName = 'Microsoft IME 2010'
    Lines.Strings = (
      '  TCoreClient = class (TCoreBase)'
      '  private'
      '    FDeskCamDecoder : TDeskCamDecoderSync;'
      '    procedure on_BitmapDeskCamIsReady(Sender:TObject);'
      '  private'
      '    FVideoOut : TVideoOut;'
      '    procedure on_BitmapOfCamIsReady(Sender:TObject);'
      '  protected'
      '    procedure do_Initialize; override;'
      '    procedure do_Finalize; override;'
      '  protected'
      '    FRoomClient : TRoomClient;'
      
        '    procedure do_MediaPacket(AData:pointer; ASize:integer); virt' +
        'ual;'
      
        '    procedure do_TextPacket(AData:pointer; ASize:integer; AText:' +
        'string); virtual;'
      '  private'
      
        '    procedure on_RoomClient_ReceivedText(AData:pointer; ASize:in' +
        'teger; AText:string);'
      
        '    procedure on_RoomClient_ReceivedMedia(AData:pointer; ASize:i' +
        'nteger);'
      '    procedure on_RoomClient_Disconnected(Sender:TObject);'
      ''
      '    procedure on_RoomClient_ServerPrepared(Sender:TObject);'
      '  private'
      '    FDecBandwidthTick : integer;'
      '    FVoicePacketCounter : TVoicePacketCounter;'
      
        '    procedure on_VoicePacketCounter_Interval(Sender:TObject; APe' +
        'rcent:integer);'
      '  private'
      '    FVoiceOut : TVoiceOut;'
      '    FVitalChecker : TVitalChecker;'
      '  private'
      '    function GetIsLogined: boolean;'
      '    procedure SetIsLogined(const Value: boolean);'
      '    function GetRoomClient: IRoomClient; override;'
      '    function GetUserID: string; override;'
      '    function GetVoiceOut: IVoiceOut;'
      '    function GetIsShowActive: boolean;'
      '    function GetVideoOut: IVideoOut;'
      '  public'
      '    constructor Create(AOwner: TComponent); override;'
      '    destructor Destroy; override;'
      ''
      '    /// '#44053#51032#49892' '#49436#48260#50640' '#51217#49549#54620#45796'.  ('#53580#49828#53944' '#50857#46020#47196' '#47564#46308#50612' '#51664')'
      
        '    function Connect(AHost:string; APort:integer):boolean; overl' +
        'oad;'
      ''
      '    /// '#44053#51032#49892' '#49436#48260#50640' '#51217#49549#54620#45796'.'
      '    function Connect:boolean; overload;'
      ''
      '    procedure Disconnect; /// '#44053#51032#49892' '#51217#49549' '#51333#47308#54620#45796'.'
      ''
      '    {*'
      '      '#51204#49569' '#48155#51008' DeskCam'#51032' '#54788#51116' '#54868#47732#51012' '#50619#45716#45796'.'
      '      @param ABitmap '#54868#47732#51032' '#45936#51060#53552#47484' '#48372#44288' '#54624' TBitmap '#44061#52404
      '      @return '#54868#47732#51012' '#44032#51256' '#50772#45716' '#51648#51032' '#50668#48512'.  '#54868#47732#51060' '#48320#54616#51648' '#50506#50520#45796#47732', false'#44032' '#46108#45796'.'
      '    }'
      '    function GetBitmap(ABitmap:TBitmap):boolean;'
      '  published'
      
        '    property VideoOut : IVideoOut read GetVideoOut;  /// Cam'#51032' '#54868#47732 +
        ' '#46356#53076#45908#51032' '#51064#53552#54168#51060#49828
      
        '    property VoiceOut : IVoiceOut read GetVoiceOut;  /// '#51020#49457' '#46356#53076#45908#51032 +
        ' '#51064#53552#54168#51060#49828
      ''
      
        '    property IsLogined : boolean read GetIsLogined write SetIsLo' +
        'gined;  /// '#47196#44536#51064#51060' '#46104#50632#45716#44032'?'
      ''
      
        '    property IsShowActive : boolean read GetIsShowActive;  /// '#44053 +
        #51032' '#51473#51064#44032'?'
      '  end;')
    PopupMenu = PopupMenuOfSrc
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object moProject: TMemo
    Left = 0
    Top = 216
    Width = 784
    Height = 171
    Align = alTop
    ImeName = 'Microsoft IME 2010'
    PopupMenu = PopupMenuOfProject
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object moTemplateOfProject: TMemo
    Left = 16
    Top = 222
    Width = 389
    Height = 137
    ImeName = 'Microsoft IME 2010'
    Lines.Strings = (
      'library @DllName;'
      ''
      'uses'
      '  System.SysUtils,'
      '  System.Classes;'
      ''
      '@Body'
      'begin'
      'end.'
      '')
    TabOrder = 3
    Visible = False
  end
  object moUnit: TMemo
    Left = 0
    Top = 390
    Width = 784
    Height = 172
    Align = alClient
    ImeName = 'Microsoft IME 2010'
    PopupMenu = PopupMenuOfUnit
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object moTemplateOfUnit: TMemo
    Left = 16
    Top = 396
    Width = 389
    Height = 137
    ImeName = 'Microsoft IME 2010'
    Lines.Strings = (
      'unit @UnitName;'
      ''
      'uses'
      '  System.SysUtils,'
      '  System.Classes;'
      ''
      'type'
      '  T@UnitName = class'
      '  private'
      '    FHandle : pointer;'
      '  public'
      '    constructor Create;'
      '    destructor Destroy; override;'
      ''
      '@MethodInterface'
      '  end;'
      ''
      'implementation'
      ''
      '@ExternalFunctions'
      'constructor T@UnitName.Create;'
      'begin'
      '  inherited;'
      ''
      '  FHandle := _CreateObject;'
      'end;'
      ''
      'destructor T@UnitName.Destroy;'
      'begin'
      '  _DestroyObject(FHandle);'
      ''
      '  inherited;'
      'end;'
      ''
      '@MethodImplementation'
      'end.'
      '')
    TabOrder = 5
    Visible = False
  end
  object PopupMenuOfProject: TPopupMenu
    Left = 388
    Top = 288
    object miSelectAll: TMenuItem
      Caption = #47784#46160' '#49440#53469
      ShortCut = 16449
      OnClick = miSelectAllClick
    end
  end
  object PopupMenuOfUnit: TPopupMenu
    Left = 388
    Top = 424
    object MenuItem1: TMenuItem
      Caption = #47784#46160' '#49440#53469
      ShortCut = 16449
      OnClick = MenuItem1Click
    end
  end
  object PopupMenuOfSrc: TPopupMenu
    Left = 392
    Top = 84
    object MenuItem2: TMenuItem
      Caption = #47784#46160' '#49440#53469
      ShortCut = 16449
      OnClick = MenuItem2Click
    end
  end
end
