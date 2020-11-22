unit OurGame;

{$mode objfpc}
interface

uses
  SysUtils, Classes, Sorts, CalcUtils,
  Models, ProcessUtils;

type

  TOurGame = class;

  TElementType = (etBlock, etEntity, etItem);

  TEnvironment = class;
  TElementCreator = class;

  TEnvironmentElementAttribute = record
    ID : PtrUInt;
    Name : AnsiString;
    DefaultValue : PtrInt;
  end;

  { TEnvironmentElement }
                                //TODO: replace TOurGame with TEnvironment
  TEnvironmentElement = class abstract
  private
    fCreator : TElementCreator;
  protected
    function GetEnvironment : TEnvironment;
  public              {TODO}
    function GetAttributeID(const Name : AnsiString) : Integer; virtual;
    function GetAttributeValue(const {%H-}ID : PtrUInt) : PtrInt; virtual;
    function GetAttributeAsObject(const {%H-}ID : PtrUInt) : TObject; virtual;
    function HasAttribute(const {%H-}ID : PtrUInt) : Boolean; virtual;

    function GetID : integer;       
    property ID : integer read getID;
    function GetTextID : AnsiString;      
    property TextID : AnsiString read GetTextID;
    function GetSubID : integer; virtual;

    property Creator : TElementCreator read fCreator;
    constructor Create(MyCreator : TElementCreator);
  end;

  { TElementCreator }

  TElementCreator = class abstract
  private
    fID : integer;
    fEnvironment : TEnvironment;
    procedure SetID(const NewID : integer);
  public
    procedure AfterLoading; virtual;
    function GetType : TElementType; virtual; abstract;
    function CreateElement(const Coords : TVector3; const SubID : integer = 0) : TEnvironmentElement; virtual; abstract;
    function getTextID : ansistring; virtual;
    function GetID : integer;
    property Environment : TEnvironment read fEnvironment;
    property ID : integer read fID write SetID;
    constructor Create(AnEnvironment : TEnvironment);
  end;

  TRegisterCreatorMethod = procedure(Creator : TElementCreator) of object;

  { TEnvironment }

  TEnvironment = class
  type
    TCreatorSort = class(specialize TStaticSort<TElementCreator>)
    public
      class function Compare(const a, b: TValue): integer; override;
    end;
    TCreatorBSearch = class(specialize TStaticBSearch<TElementCreator, AnsiString>)
    public
      class function Compare(const a: TValue; const b: TKey): integer; override;
    end;
    TAttributeSearcher = class(specialize TStaticBSearch<TEnvironmentElementAttribute, AnsiString>)
    public
      class function Compare(const a: TValue; const b: TKey): integer; override;
    end;
  const
    {$Include Preprocesor/AttributeList.inc}
  private                  
    FRemote : Boolean;
    fIDList : array of TElementCreator;
    fIDCount : Integer;
    fGame : TOurGame;
    class var NullAttribute : TEnvironmentElementAttribute;
    function GetElement(const ID : Integer): TElementCreator;
    procedure RegisterCreator(Creator : TElementCreator);
  public
    property Remote : Boolean read FRemote write FRemote; //decides if it is a client (true) or server (false)
    property Game : TOurGame read fGame;
    property IDCount : Integer read fIDCount;
    property Elements[const ID : Integer] : TElementCreator read GetElement;

    class function GetAttribute(const Name : AnsiString) : TEnvironmentElementAttribute; overload;
    class function GetAttribute(const ID : PtrUInt) : TEnvironmentElementAttribute; overload;
    class function ExistsAttribute(const Name : AnsiString) : Boolean; overload;
    class function ExistsAttribute(const ID : PtrUInt) : Boolean; overload;

    function GetCreator(const ID : integer) : TElementCreator;
    function GetID(const Name: AnsiString): Integer;
    function GetIDCount: Integer;

    class constructor Create;
    constructor Create(TheGame : TOurGame);
    destructor Destroy; override;
  end;

  { TOurGame }

  TOurGame = class
  private
    fTextures : TTextureManager;
    fEnvironment : TEnvironment;
  public
    property Environment : TEnvironment read fEnvironment;
    property Textures : TTextureManager read fTextures;
    function GetEnvironment: TEnvironment;

    constructor Create;
    destructor Destroy; override;
  end;

implementation
             
uses
  {$include Preprocesor/EnvironmentUnits.inc};

function ElementTypeToString(const et : TElementType) : AnsiString;
begin
  case et of
  etBlock: Result :=  'BLOCK  ';
  etEntity: Result := 'ENTITY ';
  etItem: Result :=   'ITEM   ';
    else  Result :=   'UNKNOWN';
  end;
end;

{ TEnvironment.TAttributeSearcher }

class function TEnvironment.TAttributeSearcher.Compare(const a: TValue;
  const b: TKey): integer;
begin
  Result := AnsiCompareText(a.Name, b);
end;

{ TEnvironmentElement }

function TEnvironmentElement.GetEnvironment: TEnvironment;
begin
  Result := fCreator.Environment;
end;

function TEnvironmentElement.GetAttributeID(const Name: AnsiString): Integer;
begin
  Result := GetEnvironment.GetAttribute(Name).ID;
end;

function TEnvironmentElement.GetAttributeValue(const ID: PtrUInt): PtrInt;
begin
  Result := GetEnvironment.GetAttribute(ID).DefaultValue;
end;

function TEnvironmentElement.GetAttributeAsObject(const ID: PtrUInt): TObject;
begin
  Result := TObject(GetAttributeValue(ID));
end;

function TEnvironmentElement.HasAttribute(const ID: PtrUInt): Boolean;
begin
  Result := False;
end;

function TEnvironmentElement.GetID : integer;
begin
  Result := Creator.getID;
end;

function TEnvironmentElement.GetTextID: ansistring;
begin
  Result := Creator.getTextID;
end;

function TEnvironmentElement.GetSubID: integer;
begin
  Result := 0;
end;

constructor TEnvironmentElement.Create(MyCreator: TElementCreator);
begin
  fCreator := MyCreator;
end;

{ TEnvironment.TCreatorBSearch }

class function TEnvironment.TCreatorBSearch.Compare(const a: TValue;
  const b: TKey): integer;
begin
  Result := CompareStr(UpperCase(a.GetTextID), UpperCase(b));
end;

{ TElementCreator.TCreatorSort }

class function TEnvironment.TCreatorSort.Compare(const a, b: TValue): integer;
begin
  Result := CompareStr(UpperCase(a.GetTextID), UpperCase(b.GetTextID));
end;

{ TEnvironment }

function TEnvironment.GetElement(const ID : Integer): TElementCreator;
begin
  if (ID<low(fIDList)) or (ID>High(fIDList)) then
    exit(nil);
  Result := fIDList[ID];
end;

procedure TEnvironment.RegisterCreator(Creator: TElementCreator);
begin
  Inc(fIDCount);
  setlength(fIDList, fIDCount);
  fIDList[fIDCount-1] := Creator;
  writeln('Loading: ', Creator.getTextID);
end;

class function TEnvironment.GetAttribute(const Name: AnsiString
  ): TEnvironmentElementAttribute;
var
  i : Integer;
begin
  i := TAttributeSearcher.BSearch(AttributeList, Name);
  if ExistsAttribute(i) then
    Result := AttributeList[i]
    else
    Result := NullAttribute;
end;

class function TEnvironment.GetAttribute(const ID: PtrUInt): TEnvironmentElementAttribute;
begin
   if ExistsAttribute(ID) then
     Result := AttributeList[ID]
     else
     Result := NullAttribute;
end;

class function TEnvironment.ExistsAttribute(const Name: AnsiString): Boolean;
begin
  Result := ExistsAttribute(TAttributeSearcher.BSearch(AttributeList, Name));
end;

class function TEnvironment.ExistsAttribute(const ID: PtrUInt): Boolean;
begin
  Result := (ID {%H-}>= Low(AttributeList)) and (ID <= High(AttributeList));
end;

function TEnvironment.GetCreator(const ID: integer): TElementCreator;
begin
  Result := GetElement(ID);
end;

function TEnvironment.GetID(const Name: AnsiString): Integer;
begin
  Result := TCreatorBSearch.BSearch(fIDList, Name);
end;

function TEnvironment.GetIDCount: Integer;
begin
  Result := fIDCount;
end;

class constructor TEnvironment.Create;
begin
  if not ExistsAttribute('NullAttribute') then
    RaiseException('Cannot find "NullAttribute"');
  NullAttribute := GetAttribute('NullAttribute');
end;

constructor TEnvironment.Create(TheGame: TOurGame);
var
  i : Integer;
begin
  fGame := TheGame;
  fIDCount:=0;
  SetLength(fIDList, fIDCount);
  {$include Preprocesor/EnvironmentRegister.inc}
  TCreatorSort.Merge(fIDList);
  for i := 0 to fIDCount-1 do
    fIDList[i].AfterLoading;
end;

destructor TEnvironment.Destroy;
var
  i : integer;
begin
  for i := 0 to fIDCount-1 do
    fIDList[i].Free;
  fIDCount:=0;
  SetLength(fIDList, fIDCount);
  inherited Destroy;
end;

{ TElementCreator }

procedure TElementCreator.SetID(const NewID: integer);
begin
  if fID = -1 then //works only when fID = -1
    fID := NewID;
end;

procedure TElementCreator.AfterLoading;
begin
  //Do nothing?
end;

function TElementCreator.getTextID: ansistring;
var
  cs : AnsiString;
  i : integer;
begin
  cs := ClassName;
  Result := Copy(cs, 2, length(cs)-1);
  i := Pos('CREATOR', Result);
  if i > 0 then
    delete(Result, i, length('CREATOR'));
end;

function TElementCreator.GetID: integer;
begin
  Result := fID;
end;

constructor TElementCreator.Create(AnEnvironment: TEnvironment);
begin
  fID:=0;
  fEnvironment:=AnEnvironment;
end;

{ TOurGame }

function TOurGame.GetEnvironment: TEnvironment;
begin
  Result := fEnvironment;
end;

constructor TOurGame.Create;
begin
  fTextures := TTextureManager.Create();
  fEnvironment := TEnvironment.Create(Self);
  writeln('OurGame was started');
end;

destructor TOurGame.Destroy;
begin
  fEnvironment.Free;
  fTextures.Free;
  inherited Destroy;
end;

end.
