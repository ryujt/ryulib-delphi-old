unit HtmlParser;

interface

uses
  HtmlScanner, HtmlTagScanner, HtmlElement,
  Generics.Collections,
  SysUtils, Classes;

type
  THtmlParser = class
  private
    FElementStack : TList<THtmlElement>;
    FCurrentElement : THtmlElement;
  private
    FHtmlScanner : THtmlScanner;
    procedure on_Text(Sender:TObject; const AString:string);
    procedure on_Tag(Sender:TObject; const AString:string);
  private
    FHtmlTagScanner : THtmlTagScanner;
  private
    FRoot: THtmlElement;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(const AText:string);
  public
    property Root : THtmlElement read FRoot;
  end;

implementation

{ THtmlParser }

procedure THtmlParser.Add(const AText: string);
begin
  FHtmlScanner.Execute(AText);
end;

procedure THtmlParser.Clear;
begin
  FRoot.Clear;
  FCurrentElement := FRoot;
end;

constructor THtmlParser.Create;
begin
  inherited;

  FElementStack := TList<THtmlElement>.Create;

  FRoot := THtmlElement.Create;
  FCurrentElement := FRoot;

  FHtmlScanner := THtmlScanner.Create;
  FHtmlScanner.OnText := on_Text;
  FHtmlScanner.OnTag := on_Tag;

  FHtmlTagScanner := THtmlTagScanner.Create;
end;

destructor THtmlParser.Destroy;
begin
  FreeAndNil(FElementStack);
  FreeAndNil(FRoot);
  FreeAndNil(FHtmlScanner);
  FreeAndNil(FHtmlTagScanner);

  inherited;
end;

procedure THtmlParser.on_Tag(Sender: TObject; const AString: string);
var
  Element : THtmlElementBase;
begin
  FHtmlTagScanner.Execute(AString);

  if Copy(FHtmlTagScanner.Elements.Text, 1, 1) = '/' then  begin
    if ('/' + FCurrentElement.TagName) <> LowerCase(FHtmlTagScanner.Elements[0]) then
      raise Exception.Create(Format('Tag </%s> is required but <%s> is comming.', [FCurrentElement.TagName, FHtmlTagScanner.Elements[0]]));

    if FElementStack.Count > 0 then begin
      FCurrentElement := FElementStack[FElementStack.Count-1];
      FElementStack.Delete(FElementStack.Count-1);
    end;

  end else begin
    Element := THtmlElementBase.CreateElement(FCurrentElement, FHtmlTagScanner.Elements);
    FCurrentElement.Add(Element);

    if Element is THtmlElement then begin
      FElementStack.Add(FCurrentElement);
      FCurrentElement := Element as THtmlElement;
    end;
  end;
end;

procedure THtmlParser.on_Text(Sender: TObject; const AString: string);
var
  Str : string;
begin
  // TODO: юс╫ц
  Str := AString;
  Str := StringReplace(Str, #13, '', [rfReplaceAll]);
  Str := StringReplace(Str, #10, '', [rfReplaceAll]);

  FCurrentElement.Add(THtmlText.Create(FCurrentElement, Str));
end;

end.
