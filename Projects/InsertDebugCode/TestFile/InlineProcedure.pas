unit _fmMain;

interface

uses
  InsertDebugCode,
  Disk,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    btExecute: TButton;
    moResult: TMemo;
    // Make error intentionaly, but this must not raise exception.
    procedure (Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FOldPath : string;
  private
    FInsertDebugCode : TInsertDebugCode;
    procedure on_FInsertDebugCode_Work(Sender:TObject; const AValue:string);
  public
  end;

var
  fmMain: TfmMain;

implementation

uses
  SelectPathDlg;


procedure ParseURL(
  const url: ThtString;
  out Proto, User, Pass, Host, Port, Path: ThtString);

  // Find the count'th occurence of string s in string t.
  // If count < 0 then look from the back
  // Thanx to Fran?is PIETTE

  function Posn(const s, t: ThtString; Count: Integer): Integer;
  var
    i, h: Integer;
    u: ThtString;
  begin
    if Count > 0 then
    begin
      u := t;
      Result := Length(u);
      for i := 1 to Count do
      begin
        h := Pos(s, u);
        if h > 0 then
          u := Copy(u, h + 1, Length(u))
        else
        begin
          u := '';
          Inc(Result);
        end;
      end;
      Result := Result - Length(u);
    end
    else if Count < 0 then
    begin
      Result := 0;
      // BG, 21.08.2011: just a little optimizing:
      // - cannot match, if copy of t is shorter than s: start with i at Length(t) - Length(s) + 1.
      // - cannot match, if 1st char already does not match: skip the string copy and pos.
      for i := Length(t) - Length(s) + 1 downto 1 do
        if t[i] = s[1] then
        begin
          u := Copy(t, i, Length(t));
          h := Pos(s, u);
          if (h <> 0) and ((h + i) <> Result) then
          begin
            Result := h + i - 1;
            Inc(Count);
            if Count = 0 then
              break;
          end;
        end;
      if Count <> 0 then
        Result := 0;
    end
    else
      Result := 0;
  end;

var
  p, q: Integer;
  s: ThtString;
begin
  proto := '';
  User := '';
  Pass := '';
  Host := '';
  Port := '';
  Path := '';

  if Length(url) < 1 then
    Exit;

  p := pos('://', url);
  if p = 0 then
  begin
    if (url[1] = '/') then
    begin
      { Absolute path without protocol specified }
      proto := 'http';
      p := 1;
      if (Length(url) > 1) and (url[2] <> '/') then
      begin
                { Relative path }
        Path := Copy(url, 1, Length(url));
        Exit;
      end;
    end
    else if lowercase(Copy(url, 1, 5)) = 'http:' then
    begin
      proto := 'http';
      p := 6;
      if (Length(url) > 6) and (url[7] <> '/') then
      begin
        { Relative path }
        Path := Copy(url, 6, Length(url));
        Exit;
      end;
    end
    else if lowercase(Copy(url, 1, 7)) = 'mailto:' then
    begin
      proto := 'mailto';
      p := pos(':', url);
    end
    else if lowercase(Copy(url, 1, 5)) = 'data:' then
    begin
      proto := 'data';
      p := pos(':', url);
    end;
  end
  else
  begin
    proto := Copy(url, 1, p - 1);
    inc(p, 2);
  end;
  s := Copy(url, p + 1, Length(url));

  p := pos('/', s);
  if p = 0 then
    p := Length(s) + 1;
  Path := Copy(s, p, Length(s));
  s := Copy(s, 1, p - 1);

  p := Posn(':', s, -1);
  if p > Length(s) then
    p := 0;
  q := Posn('@', s, -1);
  if q > Length(s) then
    q := 0;
  if (p = 0) and (q = 0) then
  begin { no user, password or port }
    Host := s;
    Exit;
  end
  else if q < p then
  begin { a port given }
    Port := Copy(s, p + 1, Length(s));
    Host := Copy(s, q + 1, p - q - 1);
    if q = 0 then
      Exit; { no user, password }
    s := Copy(s, 1, q - 1);
  end
  else
  begin
    Host := Copy(s, q + 1, Length(s));
    s := Copy(s, 1, q - 1);
  end;
  p := pos(':', s);
  if p = 0 then
    User := s
  else
  begin
    User := Copy(s, 1, p - 1);
    Pass := Copy(s, p + 1, Length(s));
  end;
end;

{$R *.dfm}

procedure TfmMain.btExecuteClick(Sender: TObject);
var
  sPath : string;
begin 
  sPath := Trim( SelectPath(FOldPath) );

  if sPath = '' then begin
    if Tag = 1 then begin
      A;
      B; C;
    end;

    begin
      Test;
    end;

    Exit;    
  end;

  moResult.Clear;

  FOldPath := sPath;

  FInsertDebugCode.Execute( sPath );
end;

procedure TfmMain.FormCreate(Sender: TObject);
  procedure Test;
  begin
    A;
    B;
    C;
  end;
begin
  FOldPath := GetExecPath;

  FInsertDebugCode := TInsertDebugCode.Create;
  FInsertDebugCode.OnWork := on_FInsertDebugCode_Work;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
  procedure Test;
    procedure Test2;
    begin
      if Tag = 1 then begin
        A;
        B; C;
      end;

      begin
        Test;
      end;

      Exit;   
    end;
  begin
    A;
    B;
  end;
begin
  FreeAndNil(FInsertDebugCode);
end;

procedure TfmMain.on_FInsertDebugCode_Work(Sender: TObject;
  const AValue: string);
begin
  moResult.Lines.Add( 'OnWork - ' + AValue );
end;

end.