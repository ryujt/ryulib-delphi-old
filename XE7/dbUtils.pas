unit dbUtils;

interface

uses
  Classes, SysUtils, DB;

function FieldToString(Fields:TFields; FieldName:string; Default:string=''):string;
function FieldToInt(Fields:TFields; FieldName:string; Default:integer=0):integer;
procedure FieldToStream(Field:TField; Stream:TStream); overload;
procedure FieldToStream(Fields:TFields; FieldName:string; Stream:TStream); overload;

implementation

function FieldToString(Fields:TFields; FieldName:string; Default:string):string;
var
  Field : TField;
begin
  Field := Fields.FindField(FieldName);
  if Field = nil then Result := Default
  else Result := Field.AsString;
end;

function FieldToInt(Fields:TFields; FieldName:string; Default:integer):integer;
var
  Field : TField;
begin
  Field := Fields.FindField(FieldName);
  if Field = nil then Result := Default
  else Result := StrToIntDef(Field.AsString, Default);
end;

procedure FieldToStream(Field:TField; Stream:TStream);
var
  BlobField : TBlobField absolute Field;
begin
  BlobField.SaveToStream(Stream);
end;

procedure FieldToStream(Fields:TFields; FieldName:string; Stream:TStream);
var
  Field : TField;
begin
  Field := Fields.FindField(FieldName);
  if Field = nil then Exit;
  
  FieldToStream(Field, Stream);
end;

end.
