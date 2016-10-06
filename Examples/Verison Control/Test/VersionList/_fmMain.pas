unit _fmMain;

interface

uses
  VersionList,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdHTTP;

type
  TfmMain = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    IdHTTP: TIdHTTP;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FServerList : TVersionList;
    FLocalList : TVersionList;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
var
  iIndex, Loop : Integer;
  FileName : string;
  fsData : TFileStream;
begin
  FServerList.LoadFromURL('http://192.168.40.128/VersionList.ini');
  FLocalList.LoadFromFile('C:\ezMov\VersionList.ini');

  for Loop := 0 to FServerList.Count-1 do begin
    FileName := FServerList.FileNames[Loop];

    iIndex := FLocalList.IndexOfFileName(FileName);
                                                                
    if (iIndex = -1) or (FServerList.Items[Loop].Version > FLocalList.Items[iIndex].Version) then begin
      ForceDirectories(FServerList.Items[Loop].Path);
      fsData := TFileStream.Create(FServerList.Items[Loop].Path+FServerList.Items[Loop].FileName, fmCreate);
      try
        IdHTTP.Get('http://192.168.40.128/'+FileName, fsData);
      finally
        fsData.Free;
      end;
      // Todo : 로컬의 버전 정보 갱신
    end;
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FServerList := TVersionList.Create;
  FLocalList := TVersionList.Create;
end;

end.
