unit MMDevApi;

{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

uses
  Windows, ActiveX, ComObj, Propsys;

const
  PKEY_Device_FriendlyName: TPropertyKey = (
    fmtid: (D1:$a45c254e; D2:$df1c ; D3:$4efd;
      D4: ($80, $20, $67, $d1, $46, $a8, $50, $e0)
    );
    pid: 14
  );

const
  CLASS_IMMDeviceEnumerator: TGUID = '{BCDE0395-E52F-467C-8E3D-C4579291692E}';
  IID_IMMDeviceEnumerator: TGUID = '{A95664D2-9614-4F35-A746-DE8DB63617E6}';
  IID_IMMDevice: TGUID = '{D666063F-1587-4E43-81F1-B948E807363F}';
  IID_IMMDeviceCollection: TGUID = '{0BD7A1BE-7A1A-44DB-8397-CC5392387B5E}';
  IID_IAudioEndpointVolume: TGUID = '{5CDF2C82-841E-4546-9722-0CF74078229A}';
  IID_IAudioMeterInformation: TGUID = '{C02216F6-8C67-4B5B-9D00-D008E73E0064}';
  IID_IAudioEndpointVolumeCallback
    : TGUID = '{657804FA-D6AD-4496-8A60-352752AF4F89}';
  // undocumented
  IID_IRemoteAudioSession: TGUID = '{33969B1D-D06F-4281-B837-7EAAFD21A9C0}';
  IID_IAudioSessionQuerier: TGUID = '{94BE9D30-53AC-4802-829C-F13E5AD34776}';
  IID_IAudioSessionQuery: TGUID = '{94BE9D30-53AC-4802-829C-F13E5AD34775}';
  //
  IID_IAudioSessionEvents: TGUID = '{24918ACC-64B3-37C1-8CA9-74A66E9957A8}';
  IID_IAudioSessionControl: TGUID = '{F4B1A599-7266-4319-A8CA-E70ACB11E8CD}';
  IID_ISimpleAudioVolume: TGUID = '{87CE5498-68D6-44E5-9215-6DA47EF883D8}';
  IID_IAudioSessionManager: TGUID = '{BFA971F1-4D5E-40BB-935E-967039BFBEE4}';
  IID_IAudioClient: TGUID = '{1CB9AD4C-DBFA-4c32-B178-C2F568A703B2}';

  DEVICE_STATE_ACTIVE = $00000001;
  DEVICE_STATE_DISABLE = $00000002;
  DEVICE_STATE_NOTPRESENT = $00000004;
  DEVICE_STATE_UNPLUGGED = $00000008;
  DEVICE_STATEMASK_ALL = $0000000F;

  // QueryHardwareSupport
  ENDPOINT_HARDWARE_SUPPORT_VOLUME = $00000001;
  ENDPOINT_HARDWARE_SUPPORT_MUTE = $00000002;
  ENDPOINT_HARDWARE_SUPPORT_METER = $00000004;

type
  EDataFlow = TOleEnum;

const
  eRender = $00000000;
  eCapture = $00000001;
  eAll = $00000002;
  EDataFlow_enum_count = $00000003;

type
  ERole = TOleEnum;

const
  eConsole = $00000000;
  eMultimedia = $00000001;
  eCommunications = $00000002;
  ERole_enum_count = $00000003;
  // AudioSessionState
  AudioSessionStateInactive = 0;
  AudioSessionStateActive = 1;
  AudioSessionStateExpired = 2;
  // AudioSessionDisconnectReason
  DisconnectReasonDeviceRemoval = 0;
  DisconnectReasonServerShutdown = DisconnectReasonDeviceRemoval + 1;
  DisconnectReasonFormatChanged = DisconnectReasonServerShutdown + 1;
  DisconnectReasonSessionLogoff = DisconnectReasonFormatChanged + 1;
  DisconnectReasonSessionDisconnected = DisconnectReasonSessionLogoff + 1;
  DisconnectReasonExclusiveModeOverride =
    DisconnectReasonSessionDisconnected + 1;

type
  PAUDIO_VOLUME_NOTIFICATION_DATA = ^AUDIO_VOLUME_NOTIFICATION_DATA;

  AUDIO_VOLUME_NOTIFICATION_DATA = packed record
    guidEventContext: TGUID;
    bMuted: BOOL;
    fMasterVolume: Single;
    nChannels: UINT;
    afChannelVolumes: array [1 .. 1] of Single;
  end;

type
  IAudioClient = interface(IUnknown)
    ['{1CB9AD4C-DBFA-4c32-B178-C2F568A703B2}']
  end;

  IAudioEndpointVolumeCallback = interface(IUnknown)
    ['{657804FA-D6AD-4496-8A60-352752AF4F89}']
    function OnNotify(pNotify: PAUDIO_VOLUME_NOTIFICATION_DATA)
      : HRESULT; stdcall;
  end;

  IAudioEndpointVolume = interface(IUnknown)
    ['{5CDF2C82-841E-4546-9722-0CF74078229A}']
    function RegisterControlChangeNotify(pNotify: IAudioEndpointVolumeCallback)
      : HRESULT; stdcall;
    function UnregisterControlChangeNotify
      (pNotify: IAudioEndpointVolumeCallback): HRESULT; stdcall;
    function GetChannelCount(var pnChannelCount: UINT): HRESULT; stdcall;
    function SetMasterVolumeLevel(fLevelDB: Single; pguidEventContext: PGuid)
      : HRESULT; stdcall;
    function SetMasterVolumeLevelScalar(fLevel: Single;
      pguidEventContext: PGuid): HRESULT; stdcall;
    function GetMasterVolumeLevel(var pfLevelDB: Single): HRESULT; stdcall;
    function GetMasterVolumeLevelScalar(var pfLevel: Single): HRESULT; stdcall;
    function SetChannelVolumeLevel(nChannel: UINT; fLevelDB: Single;
      pguidEventContext: PGuid): HRESULT; stdcall;
    function SetChannelVolumeLevelScalar(nChannel: UINT; fLevel: Single;
      pguidEventContext: PGuid): HRESULT; stdcall;
    function GetChannelVolumeLevel(nChannel: UINT; fLevelDB: Single)
      : HRESULT; stdcall;
    function GetChannelVolumeLevelScalar(nChannel: UINT; fLevel: Single)
      : HRESULT; stdcall;
    function SetMute(bMute: BOOL; pguidEventContext: PGuid): HRESULT; stdcall;
    function GetMute(var pbMute: BOOL): HRESULT; stdcall;
    function GetVolumeStepInfo(var pnStep: UINT; var pnStepCount: UINT)
      : HRESULT; stdcall;
    function VolumeStepUp(pguidEventContext: PGuid): HRESULT; stdcall;
    function VolumeStepDown(pguidEventContext: PGuid): HRESULT; stdcall;
    function QueryHardwareSupport(var pdwHardwareSupportMask: UINT)
      : HRESULT; stdcall;
    function GetVolumeRange(var pflVolumeMindB: Single;
      var pflVolumeMaxdB: Single; var pflVolumeIncrementdB: Single)
      : HRESULT; stdcall;
  end;

  IAudioMeterInformation = interface(IUnknown)
    ['{C02216F6-8C67-4B5B-9D00-D008E73E0064}']
    function GetPeakValue(out pfPeak: Single): HRESULT; stdcall;
    function GetMeteringChannelCount(out pnChannelCount: UINT)
      : HRESULT; stdcall;
    function GetChannelsPeakValues(u32ChannelCount: UINT; afPeakValues: pSingle)
      : HRESULT; stdcall;
    function QueryHardwareSupport(out pdwHardwareSupportMask: UINT)
      : HRESULT; stdcall;
  end;

  IMMDevice = interface(IUnknown)
    ['{D666063F-1587-4E43-81F1-B948E807363F}']
    function Activate(const iid: TGUID; dwClsCtx: UINT;
      pActivationParams: PPropVariant; out ppInterface: IUnknown)
      : HRESULT; stdcall;
    function OpenPropertyStore(stgmAccess: integer;
      out ppProperties: IPropertyStore): HRESULT; stdcall;
    function GetId(ppstrId: PWChar): HRESULT; stdcall;
    function GetState(var pdwState: UINT): HRESULT; stdcall;
  end;

  IMMDeviceCollection = interface(IUnknown)
    ['{0BD7A1BE-7A1A-44DB-8397-CC5392387B5E}']
    function GetCount(var pcDevices: UINT): HRESULT; stdcall;
    function Item(nDevice: UINT; out ppDevice: IMMDevice): HRESULT; stdcall;
  end;

  IMMNotificationClient = interface(IUnknown)
    ['{7991EEC9-7E89-4D85-8390-6C703CEC60C0}']
  end;

  IMMDeviceEnumerator = interface(IUnknown)
    ['{A95664D2-9614-4F35-A746-DE8DB63617E6}']
    function EnumAudioEndpoints(dataFlow: EDataFlow; dwStateMask: DWORD;
      out ppDevices: IMMDeviceCollection): HRESULT; stdcall;
    function GetDefaultAudioEndpoint(dataFlow: EDataFlow; role: ERole;
      out ppEndpoint: IMMDevice): HRESULT; stdcall;
    function GetDevice(pwstrId: PWChar; out ppDevice: IMMDevice)
      : HRESULT; stdcall;
    function RegisterEndpointNotificationCallback
      (var pClient: IMMNotificationClient): HRESULT; stdcall;
    function UnregisterEndpointNotificationCallback
      (var pClient: IMMNotificationClient): HRESULT; stdcall;
  end;

  { undocumented }
  IRemoteAudioSession = interface(IUnknown)
    ['{33969B1D-D06F-4281-B837-7EAAFD21A9C0}']
    function func_a: HRESULT; stdcall;
    function func_b: HRESULT; stdcall;
    function func_c: HRESULT; stdcall;
    function func_d: HRESULT; stdcall;
    function func_e: HRESULT; stdcall;
    function func_f: HRESULT; stdcall;
    function func_g: HRESULT; stdcall;
    function func_h: HRESULT; stdcall;
    function func_i: HRESULT; stdcall;
    function func_j: HRESULT; stdcall;
    function func_k: HRESULT; stdcall;
    function GetProcessID(out pid: UINT): HRESULT; stdcall;
  end;

  { undocumented }
  IAudioSessionQuerier = interface(IUnknown)
    ['{94BE9D30-53AC-4802-829C-F13E5AD34776}']
    function GetNumSessions(out NumSessions: UINT): HRESULT; stdcall;
    function QuerySession(Num: UINT; out Session: IUnknown): HRESULT; stdcall;
  end;

  { undocumented }
  IAudioSessionQuery = interface(IUnknown)
    ['{94BE9D30-53AC-4802-829C-F13E5AD34775}']
    function GetQueryInterface(out AudioQuerier: IAudioSessionQuerier)
      : HRESULT; stdcall;
  end;

  IAudioSessionEvents = interface(IUnknown)
    ['{24918ACC-64B3-37C1-8CA9-74A66E9957A8}']
    function OnDisplayNameChanged(NewDisplayName: LPCWSTR; EventContext: PGuid)
      : HRESULT; stdcall;
    function OnIconPathChanged(NewIconPath: LPCWSTR; EventContext: PGuid)
      : HRESULT; stdcall;
    function OnSimpleVolumeChanged(NewVolume: Single; NewMute: LongBool;
      EventContext: PGuid): HRESULT; stdcall;
    function OnChannelVolumeChanged(ChannelCount: UINT;
      NewChannelArray: pSingle; ChangedChannel: UINT; EventContext: PGuid)
      : HRESULT; stdcall;
    function OnGroupingParamChanged(NewGroupingParam, EventContext: PGuid)
      : HRESULT; stdcall;
    function OnStateChanged(NewState: UINT): HRESULT; stdcall;
    // AudioSessionState
    function OnSessionDisconnected(DisconnectReason: UINT): HRESULT; stdcall;
    // AudioSessionDisconnectReason
  end;

  IAudioSessionControl = interface(IUnknown)
    ['{F4B1A599-7266-4319-A8CA-E70ACB11E8CD}']
    function GetState(out pRetVal: UINT): HRESULT; stdcall;
    function GetDisplayName(out pRetVal: LPWSTR): HRESULT; stdcall;
    // pRetVal must be freed by CoTaskMemFree
    function SetDisplayName(Value: LPCWSTR; EventContext: PGuid)
      : HRESULT; stdcall;
    function GetIconPath(out pRetVal: LPWSTR): HRESULT; stdcall;
    // pRetVal must be freed by CoTaskMemFree
    function SetIconPath(Value: LPCWSTR; EventContext: PGuid): HRESULT; stdcall;
    function GetGroupingParam(pRetVal: PGuid): HRESULT; stdcall;
    function SetGroupingParam(OverrideValue, EventContext: PGuid)
      : HRESULT; stdcall;
    function RegisterAudioSessionNotification(const NewNotifications
      : IAudioSessionEvents): HRESULT; stdcall;
    function UnregisterAudioSessionNotification(const NewNotifications
      : IAudioSessionEvents): HRESULT; stdcall;
  end;

  ISimpleAudioVolume = interface(IUnknown)
    ['{87CE5498-68D6-44E5-9215-6DA47EF883D8}']
    function SetMasterVolume(fLevel: Single; EventContext: PGuid)
      : HRESULT; stdcall;
    function GetMasterVolume(out fLevel: Single): HRESULT; stdcall;
    function SetMute(bMute: LongBool; EventContext: PGuid): HRESULT; stdcall;
    function GetMute(out bMute: LongBool): HRESULT; stdcall;
  end;

  IAudioSessionManager = interface(IUnknown)
    ['{BFA971F1-4D5E-40BB-935E-967039BFBEE4}']
    function GetAudioSessionControl(AudioSessionGuid: PGuid; StreamFlag: UINT;
      out SessionControl: IAudioSessionControl): HRESULT; stdcall;
    function GetSimpleAudioVolume(AudioSessionGuid: PGuid; StreamFlag: UINT;
      out AudioVolume: ISimpleAudioVolume): HRESULT; stdcall;
  end;

  // get ISimpleAudioVolume from ProcessID
function GetSimpleAudioVolumeFromPid(const AudioDevice: IMMDevice; pid: UINT;
  out SimpleAudio: ISimpleAudioVolume; out currguid: TGUID): Boolean; stdcall;

// get ISimpleAudioVolume from Process Handle
function GetSimpleAudioVolumeFromHandle(const AudioDevice: IMMDevice; Hnd: UINT;
  out SimpleAudio: ISimpleAudioVolume; out currguid: TGUID): Boolean; stdcall;

function GetSimpleAudioVolumeFromModuleName(const AudioDevice: IMMDevice;
  ModuleName: pWideChar; out SimpleAudio: ISimpleAudioVolume;
  out currguid: TGUID): Boolean; stdcall;

implementation

uses
  tlhelp32;

function GetProcessIdFromModuleName(ModuleName: pWideChar): LongWord;
var
  ToolSnap: LongWord;
  ProcessEntry: ProcessEntry32W;
  bCont: LongBool;
  ModuleName16: WideString;
begin
  Result := 0;
  ModuleName16 := ModuleName;
  ToolSnap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  ProcessEntry.dwSize := sizeof(ProcessEntry);
  bCont := Process32FirstW(ToolSnap, ProcessEntry);
  while bCont do
  begin
    if ProcessEntry.szExeFile = ModuleName16 then
      Result := ProcessEntry.th32ProcessID;
    ProcessEntry.dwSize := sizeof(ProcessEntry);
    bCont := (Result = 0) and Process32NextW(ToolSnap, ProcessEntry);
  end;
  CloseHandle(ToolSnap);
end;

function GetSimpleAudioVolumeFromModuleName(const AudioDevice: IMMDevice;
  ModuleName: pWideChar; out SimpleAudio: ISimpleAudioVolume;
  out currguid: TGUID): Boolean; stdcall;
var
  pid: LongWord;
begin
  pid := GetProcessIdFromModuleName(ModuleName);
  Result := GetSimpleAudioVolumeFromPid(AudioDevice, pid, SimpleAudio,
    currguid);
end;

function GetSimpleAudioVolumeFromHandle(const AudioDevice: IMMDevice; Hnd: UINT;
  out SimpleAudio: ISimpleAudioVolume; out currguid: TGUID): Boolean; stdcall;
var
  pid: UINT;
begin
  GetWindowThreadProcessId(Hnd, pid);
  Result := GetSimpleAudioVolumeFromPid(AudioDevice, pid, SimpleAudio,
    currguid);
end;

function GetSimpleAudioVolumeFromPid(const AudioDevice: IMMDevice; pid: UINT;
  out SimpleAudio: ISimpleAudioVolume; out currguid: TGUID): Boolean; stdcall;
var
  SessionManager: IAudioSessionManager;
  SessionQuery: IAudioSessionQuery;
  SessionQuerier: IAudioSessionQuerier;
  RemoteSession: IRemoteAudioSession;
  SessionControl: IAudioSessionControl;
  dummy: IUnknown;
  MaxSession, Sessionpid: UINT;
  hr, i: integer;
begin
  Result := False;
  SessionManager := nil;
  SessionQuery := nil;
  SessionQuerier := nil;
  SessionControl := nil;

  // get AudioSessionManager
  hr := AudioDevice.Activate(IID_IAudioSessionManager, CLSCTX_ALL, nil,
    IUnknown(SessionManager));
  if Succeeded(hr) then
  begin
    // get SessionQuery
    hr := SessionManager.QueryInterface(IID_IAudioSessionQuery, SessionQuery);
    if Succeeded(hr) then
    begin
      // get SessionQuerier
      hr := SessionQuery.GetQueryInterface(SessionQuerier);
      if Succeeded(hr) then
      begin
        // check sessions
        hr := SessionQuerier.GetNumSessions(MaxSession);
        if Succeeded(hr) then
          if MaxSession > 0 then
            for i := 0 to MaxSession - 1 do
            begin
              dummy := nil;
              hr := SessionQuerier.QuerySession(i, dummy);
              RemoteSession := nil;
              if Succeeded(hr) then
              begin
                // get RemoteSession
                hr := dummy.QueryInterface(IID_IRemoteAudioSession,
                  RemoteSession);
                if Succeeded(hr) then
                begin
                  // compare ProcessID
                  if Succeeded(RemoteSession.GetProcessID(Sessionpid)) then
                    if Sessionpid <> 0 then
                      if Sessionpid = pid then
                      begin
                        Result := Succeeded
                          (RemoteSession.QueryInterface(IID_ISimpleAudioVolume,
                          SimpleAudio));
                        if Succeeded
                          (dummy.QueryInterface(IID_IAudioSessionControl,
                          SessionControl)) then
                          SessionControl.GetGroupingParam(@currguid);
                        SessionControl := nil;
                      end;
                  if Result then
                    Break;
                end;
              end;
            end;
        RemoteSession := nil;
        SessionQuerier := nil;
      end;
      SessionQuery := nil;
    end;
    SessionManager := nil;
  end;
end;

end.
