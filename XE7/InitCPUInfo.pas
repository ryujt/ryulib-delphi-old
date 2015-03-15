{
  FastLib 에서 CPUInfo 구하는 부분을 카피.
}

unit InitCPUInfo;

interface
uses windows;

type
  TCPUFeature = (cfCX8,cfCMOV,cfMMX,cfMMX2,cfSSE,cfSSE2,cf3DNow,cf3DNow2);
  TCPUFeatureSet = set of TCPUFeature;

  TCPUInfo = record
    VendorID: string[12];
    Features: TCPUFeatureSet;
    CPUCount,Family,Model: Byte;
  end;

var
  CPUInfo: TCPUInfo;

implementation

procedure GetCPUInfo;
  function HasCPUID:LongBool;
  asm
    pushfd
    pop  eax
    mov  ecx,eax
    xor  eax,$00200000
    push eax
    popfd
    pushfd
    pop  eax
    xor  eax,ecx
  end;

  procedure CPUID(Flag:DWord;var Signature,Features:DWord);
  asm
    push ebx
    push esi
    push edi
    mov  esi,edx
    mov  edi,ecx
    db   $0F,$A2 // cpuid
    mov  [esi],eax
    mov  [edi],edx
    pop  edi
    pop  esi
    pop  ebx
  end;

  function GetVendorID(VendorID:ShortString):DWord;
  asm
    push edi
    push ebx
    push eax
    xor  eax,eax
    db   $0F,$A2 // cpuid
    pop  edi
    mov  [edi],Byte(12)
    inc  edi
    push ecx
    push edx
    mov  ecx,3
    @loop:
      mov [edi],bl
      inc edi
      mov [edi],bh
      inc edi
      shr ebx,16
      mov [edi],bl
      inc edi
      mov [edi],bh
      inc edi
      pop ebx
      dec ecx
    jnz @loop
    pop edi
  end;
var
  SysInfo: TSystemInfo;
  Signature,Features: DWord;
begin
  if HasCPUID then
  begin
    if GetVendorID(CPUInfo.VendorID) > 0 then
    begin
      // standard features
      CPUID(1,Signature,Features);
      CPUInfo.Family:=(Signature shr 8)and $0F;
      CPUInfo.Model:=(Signature shr 4)and $0F;
      if (Features and(1 shl 8))<>0 then include(CPUInfo.Features,cfCX8);
      if (Features and(1 shl 15))<>0 then include(CPUInfo.Features,cfCMOV);
      if (Features and(1 shl 23))<>0 then include(CPUInfo.Features,cfMMX);
      if (Features and(1 shl 25))<>0 then include(CPUInfo.Features,cfSSE);
      if (Features and(1 shl 26))<>0 then include(CPUInfo.Features,cfSSE2);
      // extended features
      CPUID($80000000,Signature,Features);
      if Signature > $80000000 then
      begin
        CPUID($80000001,Signature,Features);
        if (Features and(1 shl 22))<>0 then include(CPUInfo.Features,cfMMX2);
        if (Features and(1 shl 31))<>0 then include(CPUInfo.Features,cf3DNow);
        if (Features and(1 shl 30))<>0 then include(CPUInfo.Features,cf3DNow2);
      end;
    end;
  end;
  GetSystemInfo(SysInfo);
  CPUInfo.CPUCount:=SysInfo.dwNumberOfProcessors;
end;

begin
  GetCPUInfo;
end.
