unit MMD;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Winapi.ActiveX, System.Win.ComObj, MMSystem;

const
  IID_IMMDeviceEnumerator: TGUID = '{A95664D2-9614-4F35-A746-DE8DB63617E6}';
  CLASS_IMMDeviceEnumerator: TGUID = '{BCDE0395-E52F-467C-8E3D-C4579291692E}';
  IID_IAudioMeterInformation: TGUID = '{C02216F6-8C67-4B5B-9D00-D008E73E0064}';
  eConsole = $00000000;

  eRender  = $00000000;
  eCapture = $00000001;
  eAll     = $00000002;
  EDataFlow_enum_count = $00000003;

type
  EDATAFLOW = TOleEnum;
  EROLE = TOleEnum;

  IMMDevice = interface(IUnknown)
    ['{D666063F-1587-4E43-81F1-B948E807363F}']
    function Activate(const iid: TGUID; const dwClsCtx: UINT; const pActivationParams: PPropVariant; out ppInterface: IUnknown)
      : HRESULT; stdcall;
  end;

  IMMDeviceCollection = interface(IUnknown)
    ['{0BD7A1BE-7A1A-44DB-8397-CC5392387B5E}']
  end;

  IMMDeviceEnumerator = interface(IUnknown)
    ['{A95664D2-9614-4F35-A746-DE8DB63617E6}']
    function EnumAudioEndpoints(const dataFlow: EDATAFLOW; const dwStateMask: DWORD; out ppDevices: IMMDeviceCollection): HRESULT; stdcall;
    function GetDefaultAudioEndpoint(const dataFlow: EDATAFLOW; const role: EROLE; out ppEndpoint: IMMDevice): HRESULT; stdcall;
  end;

  IAudioMeterInformation = interface(IUnknown)
    ['{C02216F6-8C67-4B5B-9D00-D008E73E0064}']
    function GetPeakValue(out pfPeak: Single): HRESULT; stdcall;
    function GetMeteringChannelCount(out pnChannelCount: UINT): HRESULT; stdcall;
    function GetChannelsPeakValues(u32ChannelCount: UINT; out afPeakValues: pSingle): HRESULT; stdcall;
    function QueryHardwareSupport(out pdwHardwareSupportMask: UINT): HRESULT; stdcall;
  end;

var
  device : IMMDevice;
  deviceEnumerator : IMMDeviceEnumerator;
  Render : IAudioMeterInformation = nil;
  Capture : IAudioMeterInformation = nil;

implementation

initialization
  CoCreateInstance(CLASS_IMMDeviceEnumerator, nil, CLSCTX_ALL, IID_IMMDeviceEnumerator, deviceEnumerator);

  deviceEnumerator.GetDefaultAudioEndpoint(eRender, eConsole, device);
  device.Activate(IID_IAudioMeterInformation, CLSCTX_ALL, nil, IUnknown(Render));

  deviceEnumerator.GetDefaultAudioEndpoint(eCapture, eConsole, device);
  device.Activate(IID_IAudioMeterInformation, CLSCTX_ALL, nil, IUnknown(Capture));
end.
