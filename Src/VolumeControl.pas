unit VolumeControl;

interface

uses
  WinVersion, MMDevApi, MMSystem,
  SysUtils, Classes, ActiveX;

type
  TGetMicVolumeFunction = function :single;
  TSetMicVolumeFunction = procedure (AVolume:single);

  TGetSpeakerVolumeFunction = function :single;
  TSetSpeakerVolumeFunction = procedure (AVolume:single);

var
  GetMicVolume : TGetMicVolumeFunction;
  SetMicVolume : TSetMicVolumeFunction;

  GetSpeakerVolume : TGetSpeakerVolumeFunction;
  SetSpeakerVolume : TSetSpeakerVolumeFunction;

implementation

Type
	TDeviceName = (MasterOut, Microphone, WaveOut, Synth);

var
  WinVersion : TWinVersion;
  MicVolume : IAudioEndpointVolume = nil;
  SpeakerVolume : IAudioEndpointVolume = nil;

function GetVolume(DN:TDeviceName):word;
var
  hMix: HMIXER;
  mxlc: MIXERLINECONTROLS;
  mxcd: TMIXERCONTROLDETAILS;
  vol: TMIXERCONTROLDETAILS_UNSIGNED;
  mxc: MIXERCONTROL;
  mxl: TMixerLine;
  intRet: Integer;
  nMixerDevs: Integer;
begin
  // Check if Mixer is available
  nMixerDevs := mixerGetNumDevs();
  if (nMixerDevs < 1) then Exit;

  // open the mixer
  intRet := mixerOpen(@hMix, 0, 0, 0, 0);
  if intRet = MMSYSERR_NOERROR then
  begin
    case DN of
      MasterOut: mxl.dwComponentType := MIXERLINE_COMPONENTTYPE_DST_SPEAKERS;
      Microphone: mxl.dwComponentType := MIXERLINE_COMPONENTTYPE_SRC_MICROPHONE;
      WaveOut: mxl.dwComponentType := MIXERLINE_COMPONENTTYPE_SRC_WAVEOUT;
      Synth: mxl.dwComponentType := MIXERLINE_COMPONENTTYPE_SRC_SYNTHESIZER;
    end;

    mxl.cbStruct := SizeOf(mxl);

    // get line info
    intRet := mixerGetLineInfo(hMix, @mxl, MIXER_GETLINEINFOF_COMPONENTTYPE);

    if intRet = MMSYSERR_NOERROR then begin
      FillChar(mxlc, SizeOf(mxlc), 0);
      mxlc.cbStruct := SizeOf(mxlc);
      mxlc.dwLineID := mxl.dwLineID;
      mxlc.dwControlType := MIXERCONTROL_CONTROLTYPE_VOLUME;
      mxlc.cControls := 1;
      mxlc.cbmxctrl := SizeOf(mxc);

      mxlc.pamxctrl := @mxc;
      intRet := mixerGetLineControls(hMix, @mxlc,
        MIXER_GETLINECONTROLSF_ONEBYTYPE);

      if intRet = MMSYSERR_NOERROR then
      begin
        FillChar(mxcd, SizeOf(mxcd), 0);
        mxcd.dwControlID := mxc.dwControlID;
        mxcd.cbStruct := SizeOf(mxcd);
        mxcd.cMultipleItems := 0;
        mxcd.cbDetails := SizeOf(vol);
        mxcd.paDetails := @vol;
        mxcd.cChannels := 1;

        intRet := mixerGetControlDetails(hMix, @mxcd,
          MIXER_SETCONTROLDETAILSF_VALUE);

        Result := vol.dwValue;

        if intRet <> MMSYSERR_NOERROR then
          raise Exception.Create('GetControlDetails Error');
      end else begin
        raise Exception.Create('GetLineInfo Error');
      end;
    end;

    intRet := mixerClose(hMix);
  end;
end;

procedure SetVolume(DN: TDeviceName; Value: Word);
var
  hMix: HMIXER;
  mxlc: MIXERLINECONTROLS;
  mxcd: TMIXERCONTROLDETAILS;
  vol: TMIXERCONTROLDETAILS_UNSIGNED;
  mxc: MIXERCONTROL;
  mxl: TMixerLine;
  intRet: Integer;
  nMixerDevs: Integer;
begin
  // Check if Mixer is available
  nMixerDevs := mixerGetNumDevs();
  if (nMixerDevs < 1) then Exit;

  // open the mixer
  intRet := mixerOpen(@hMix, 0, 0, 0, 0);
  if intRet = MMSYSERR_NOERROR then begin
    case DN of
      MasterOut: mxl.dwComponentType := MIXERLINE_COMPONENTTYPE_DST_SPEAKERS;
      Microphone: mxl.dwComponentType := MIXERLINE_COMPONENTTYPE_SRC_MICROPHONE;
      WaveOut: mxl.dwComponentType := MIXERLINE_COMPONENTTYPE_SRC_WAVEOUT;
      Synth: mxl.dwComponentType := MIXERLINE_COMPONENTTYPE_SRC_SYNTHESIZER;
    end;

    mxl.cbStruct := SizeOf(mxl);

    // get line info
    intRet := mixerGetLineInfo(hMix, @mxl, MIXER_GETLINEINFOF_COMPONENTTYPE);

    if intRet = MMSYSERR_NOERROR then begin
      FillChar(mxlc, SizeOf(mxlc), 0);
      mxlc.cbStruct := SizeOf(mxlc);
      mxlc.dwLineID := mxl.dwLineID;
      mxlc.dwControlType := MIXERCONTROL_CONTROLTYPE_VOLUME;
      mxlc.cControls := 1;
      mxlc.cbmxctrl := SizeOf(mxc);

      mxlc.pamxctrl := @mxc;
      intRet := mixerGetLineControls(hMix, @mxlc,
        MIXER_GETLINECONTROLSF_ONEBYTYPE);

      if intRet = MMSYSERR_NOERROR then begin
        FillChar(mxcd, SizeOf(mxcd), 0);
        mxcd.dwControlID := mxc.dwControlID;
        mxcd.cbStruct := SizeOf(mxcd);
        mxcd.cMultipleItems := 0;
        mxcd.cbDetails := SizeOf(vol);
        mxcd.paDetails := @vol;
        mxcd.cChannels := 1;

        vol.dwValue := Value;

        intRet := mixerSetControlDetails(hMix, @mxcd,
          MIXER_SETCONTROLDETAILSF_VALUE);

        if intRet <> MMSYSERR_NOERROR then
          raise Exception.Create('SetControlDetails Error');
      end else begin
        raise Exception.Create('GetLineInfo Error');
      end;
    end;
    intRet := mixerClose(hMix);
  end;
end;

function GetMicVolumeXP: single;
begin
  Result := GetVolume(Microphone) / $FFFF;
end;

procedure SetMicVolumeXP(AVolume:single);
begin
  SetVolume(Microphone, Round(AVolume * $FFFF));
end;

function GetSpeakerVolumeXP:single;
begin
  Result := GetVolume(MasterOut) / $FFFF;
end;

procedure SetSpeakerVolumeXP(AVolume:single);
begin
  SetVolume(MasterOut, Round(AVolume * $FFFF));
end;

function GetMicVolumeVista:single;
begin
  if MicVolume = nil then
    raise Exception.Create('GetMicVolumeVista: MicVolume = nil');

  MicVolume.GetMasterVolumeLevelScalar(Result);
end;

procedure SetMicVolumeVista(AVolume:single);
begin
  if MicVolume = nil then
    raise Exception.Create('SetMicVolumeVista: MicVolume = nil');

  MicVolume.SetMasterVolumeLevelScalar(AVolume, nil);
end;

function GetSpeakerVolumeVista:single;
begin
  if SpeakerVolume = nil then
    raise Exception.Create('GetSpeakerVolumeVista: SpeakerVolume = nil');

  SpeakerVolume.GetMasterVolumeLevelScalar(Result);
end;

procedure SetSpeakerVolumeVista(AVolume:single);
begin
  if SpeakerVolume = nil then
    raise Exception.Create('SetSpeakerVolumeVista: SpeakerVolume = nil');

  SpeakerVolume.SetMasterVolumeLevelScalar(AVolume, nil);
end;

var
  deviceEnumerator : IMMDeviceEnumerator;
  defaultDevice : IMMDevice;

initialization
  WinVersion := GetWinVersion;

  if WinVersion >= wvWinVista then begin
    CoCreateInstance(CLASS_IMMDeviceEnumerator, nil, CLSCTX_INPROC_SERVER, IID_IMMDeviceEnumerator, deviceEnumerator);

    deviceEnumerator.GetDefaultAudioEndpoint(eRender, eConsole, defaultDevice);
    defaultDevice.Activate(IID_IAudioEndpointVolume, CLSCTX_INPROC_SERVER, nil, IUnknown(SpeakerVolume));

    deviceEnumerator.GetDefaultAudioEndpoint(eCapture, eConsole, defaultDevice);
    defaultDevice.Activate(IID_IAudioEndpointVolume, CLSCTX_INPROC_SERVER, nil, IUnknown(MicVolume));

    GetMicVolume := GetMicVolumeVista;
    SetMicVolume := SetMicVolumeVista;

    GetSpeakerVolume := GetSpeakerVolumeVista;
    SetSpeakerVolume := SetSpeakerVolumeVista;
  end else begin
    GetMicVolume := GetMicVolumeXP;
    SetMicVolume := SetMicVolumeXP;

    GetSpeakerVolume := GetSpeakerVolumeXP;
    SetSpeakerVolume := SetSpeakerVolumeXP;
  end;
end.
