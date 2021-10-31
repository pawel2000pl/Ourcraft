unit LightCubes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LightTypes, OurConstants, TinyHashData, CalcUtils, Incrementations, JenkinsHash;

type
    { TLightCube }

  TLightCube = class abstract
  public
    //do not checks ranges
    procedure Clear; virtual; abstract;
    procedure SetValue(const x, y, z : Integer; const Value : TLight); virtual; abstract;
    function GetValue(const x, y, z : Integer) : TLight; virtual; abstract;
    property Values[const x, y, z : Integer] : TLight read GetValue write SetValue; default;
    constructor Create; virtual;
  end;
  TLightCubeClass = class of TLightCube;

  { TThickLightCube }

  TThickLightCube = class(TLightCube)
  private
    FData :  array[0..ChunkSize-1, 0..ChunkSize-1, 0..ChunkSize-1] of TLight;
  public
    procedure InitZeros;
    procedure Clear; override;
    procedure SetValue(const x, y, z: Integer; const Value: TLight); override;
    function GetValue(const x, y, z: Integer): TLight; override;
  end;

  { TZeroedThickLightCube }

  TZeroedThickLightCube = class(TThickLightCube)
  public
    constructor Create; override;
  end;

  { TCoordKeyLightValueMap }

  TCoordKeyLightValueMap = class(specialize TTinyHashKeyMap<TIntVector3, TLight>)
  public
     function TryFindFirst(const Key: TIntVector3; var Value: TLongLight): Boolean; overload;
     function CreateHash(const Key: TIntVector3): PtrUInt; override;
  end;

  { TRareLightCube }

  TRareLightCube = class(TLightCube)
  private
    FData : TCoordKeyLightValueMap;
  public
    function Count : Integer;

    procedure Clear; override;
    procedure SetValue(const x, y, z: Integer; const Value: TLight); override;
    function GetValue(const x, y, z: Integer): TLight; override;

    constructor Create; override;
    destructor Destroy; override;
  end;

  { TQueryLightCube }

  TQueryLightCube = class(TLightCube)
  type
    TGetLightFunction = function(const x, y, z : Integer) : TLight of object;
    TSetLightProcedure = procedure(const x, y, z : Integer; const Value : TLight) of object;
    TClearProcedure = procedure of object;

  private
    FSetLight : TSetLightProcedure;
    FGetLight : TGetLightFunction;
    FClear : TClearProcedure;

    function ZeroLight(const {%H-}x, {%H-}y, {%H-}z : Integer) : TLight;
    procedure NullSet(const {%H-}x, {%H-}y, {%H-}z : Integer; const {%H-}Value : TLight);
  public
    procedure Clear; override;
    procedure SetValue(const x, y, z: Integer; const Value: TLight); override;
    function GetValue(const x, y, z: Integer): TLight; override;

    constructor Create(const AGetLight : TGetLightFunction; const ASetLight : TSetLightProcedure = nil; const AClear : TClearProcedure = nil);
    constructor Create; override;
  end;


implementation

{ TCoordKeyLightValueMap }

function TCoordKeyLightValueMap.TryFindFirst(const Key: TIntVector3; var Value: TLongLight): Boolean;
var
  v : TLight;
begin
  v := AsLightZero;
  Result := TryFindFirst(Key, v);
  Value := v;
end;

function TCoordKeyLightValueMap.CreateHash(const Key: TIntVector3): PtrUInt;
begin
  Exit(JenkinsHash.jenkins_one_at_a_time_hash(@Key, SizeOf(Key)));
end;

{ TQueryLightCube }

function TQueryLightCube.ZeroLight(const x, y, z: Integer): TLight;
begin
  Exit(AsLightZero);
end;

procedure TQueryLightCube.NullSet(const x, y, z: Integer; const Value: TLight);
begin
  // do nothing
end;

procedure TQueryLightCube.Clear;
begin
  FClear();
end;

procedure TQueryLightCube.SetValue(const x, y, z: Integer; const Value: TLight);
begin
  FSetLight(x, y, z, Value);
end;

function TQueryLightCube.GetValue(const x, y, z: Integer): TLight;
begin
  Exit(FGetLight(x, y, z));
end;

constructor TQueryLightCube.Create(const AGetLight: TGetLightFunction;
  const ASetLight: TSetLightProcedure; const AClear: TClearProcedure);
begin
  Create;
  FGetLight:=AGetLight;
  FSetLight:=ASetLight;
  FClear:=AClear;
end;

constructor TQueryLightCube.Create;
begin
  inherited Create;
  FSetLight:=@NullSet;
  FGetLight:=@ZeroLight;
end;

{ TZeroedThickLightCube }

constructor TZeroedThickLightCube.Create;
begin
  inherited AfterConstruction;
  InitZeros;
end;

{ TThickLightCube }

procedure TThickLightCube.InitZeros;
begin
  Clear;
end;

procedure TThickLightCube.Clear;
begin
  FillDWord(FData, SizeOf(FData) shr 2, 0);
end;

procedure TThickLightCube.SetValue(const x, y, z: Integer; const Value: TLight);
begin
  FData[x, y, z] := Value;
end;

function TThickLightCube.GetValue(const x, y, z: Integer): TLight;
begin
  Exit(FData[x, y, z]);
end;

{ TRareLightCube }

function TRareLightCube.Count: Integer;
begin
  FData.BeginRead;
  try
     Result := FData.Count;
  finally
    FData.EndRead;
  end;
end;

procedure TRareLightCube.Clear;
begin
  FData.BeginWrite;
  try
     FData.Clear;
  finally
    FData.EndWrite;
  end;
end;

procedure TRareLightCube.SetValue(const x, y, z: Integer; const Value: TLight);
var
  Coord : TIntVector3;
begin
  FData.BeginWrite;
  try
     Coord := IntVector3(x, y, z);
     FData.RemoveAllKeys(Coord);
     FData.Add(Coord, Value);
  finally
    FData.EndWrite;
  end;
end;

function TRareLightCube.GetValue(const x, y, z: Integer): TLight;
var
  i : TLight;
begin
  Result := AsLightZero;
  FData.BeginRead;
  try
    for i in FData.FindAll(IntVector3(x, y, z)) do
        UpdateIfGreater(Result, i);
  finally
    FData.EndRead;
  end;
end;

constructor TRareLightCube.Create;
begin
  inherited Create;
  FData := TCoordKeyLightValueMap.Create;
end;

destructor TRareLightCube.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

{ TLightCube }

constructor TLightCube.Create;
begin
  inherited;
end;

end.

