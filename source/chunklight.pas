unit ChunkLight;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, LightTypes, OurConstants, TinyHashData, CalcUtils, TextureMode, ThreeDimensionalArrayOfBoolean, Incrementations;

type
  TChunkLight = class;

  TNeightborQuery = function(const x, y, z : Integer) : TChunkLight of object;
  TUpdatedLightEvent = procedure of object;
  TUpdatedBlockLight = procedure(const x, y, z : Integer) of object;

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

  { TChunkLight }

  TChunkLight = class
  type
    TDepthResistance = array[TTextureMode] of Integer;
  const
    DefaultDepthResistance : TDepthResistance = (1, 1, 1, 1, 1, 1);
  private
    FLocker : TMultiReadExclusiveWriteSynchronizer;
    FOnBlockLightUpdate: TUpdatedBlockLight;
    FOnLightUpdate: TUpdatedLightEvent;
    FValue, FSource, FResistance : TLightCube;
    FOnNeightborQuery: TNeightborQuery;
    FWriteCount : LongWord;

    FDepthResistance : TDepthResistance;

    function GetDepthResistance(const Side : TTextureMode): Integer;
    function GetOnNeightborQuery: TNeightborQuery;
    procedure SetDepthResistance(const Side : TTextureMode; const AValue: Integer);
    procedure SetOnBlockLightUpdate(AValue: TUpdatedBlockLight);
    procedure SetOnLightUpdate(AValue: TUpdatedLightEvent);
    procedure SetOnNeightborQuery(AValue: TNeightborQuery);
    function EmptyNeightborQuery(const {%H-}x, {%H-}y, {%H-}z : Integer) : TChunkLight;

  public
    property Value : TLightCube read FValue;
    property Source : TLightCube read FSource;
    property Resistance : TLightCube read FResistance;

    property DepthResistance[const Side : TTextureMode] : Integer read GetDepthResistance write SetDepthResistance;

    procedure BeginWrite(const sync : Boolean = True);
    procedure EndWrite(const sync : Boolean = True);
    procedure BeginRead;
    procedure EndRead;
    function Editing : Boolean;

    property OnNeightborQuery : TNeightborQuery read GetOnNeightborQuery write SetOnNeightborQuery;
    property OnLightUpdate : TUpdatedLightEvent read FOnLightUpdate write SetOnLightUpdate;
    property OnBlockLightUpdate : TUpdatedBlockLight read FOnBlockLightUpdate write SetOnBlockLightUpdate;

    procedure AddLight(const x, y, z: integer; LightLevel: TLongLight; const maxDepth: integer = ADD_LIGHT_DEFAULT_DEPTH; const Force: boolean = False);
    procedure RelightArea(const x1, y1, z1, x2, y2, z2 : integer);
    procedure RelightBlock(const x, y, z : integer);
    procedure RelightAll;

    constructor Create(AValue, ASource, AResistance : TLightCube; const ANeightborQuery : TNeightborQuery = nil; const ALightUpdate : TUpdatedLightEvent = nil; ABlockLighUpdate : TUpdatedBlockLight = nil);
    destructor Destroy; override;
  end;

  { TChunkLightSet }

  TChunkLightSet = class(specialize TTinyHashSet<TChunkLight>)
  public
    function CreateHash(const Key: TChunkLight): PtrUInt; override;
  end;

  ECriticalSectionException = class(Exception);

implementation

{ TChunkLightSet }

function TChunkLightSet.CreateHash(const Key: TChunkLight): PtrUInt;
begin
  Exit((QWord(Key) xor (QWord(Key) shl 32) or (1 shl 63)) mod 2147483647);
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
begin
  FData.BeginWrite;
  try
     FData.RemoveAllKeys(IntVector3(x, y, z));
     FData.Add(IntVector3(x, y, z), Value);
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

{ TChunkLight }

procedure TChunkLight.SetOnNeightborQuery(AValue: TNeightborQuery);
begin
  if (AValue = nil) or (not Assigned(AValue)) then
     AValue := @EmptyNeightborQuery;
  FOnNeightborQuery:=AValue;
end;

function TChunkLight.GetOnNeightborQuery: TNeightborQuery;
begin
  If FOnNeightborQuery = @EmptyNeightborQuery then
     Exit(nil)
  else
     Exit(FOnNeightborQuery);
end;

function TChunkLight.GetDepthResistance(const Side : TTextureMode): Integer;
begin
  Exit(FDepthResistance[Side]);
end;

procedure TChunkLight.SetDepthResistance(const Side : TTextureMode; const AValue: Integer);
begin
   FDepthResistance[Side] := AValue;
end;

procedure TChunkLight.SetOnBlockLightUpdate(AValue: TUpdatedBlockLight);
begin
  if FOnBlockLightUpdate=AValue then Exit;
  FOnBlockLightUpdate:=AValue;
end;

procedure TChunkLight.SetOnLightUpdate(AValue: TUpdatedLightEvent);
begin
  if FOnLightUpdate=AValue then Exit;
  FOnLightUpdate:=AValue;
end;

function TChunkLight.EmptyNeightborQuery(const x, y, z: Integer): TChunkLight;
begin
  Exit(nil);
end;

procedure TChunkLight.BeginWrite(const sync: Boolean);
begin
  InterlockedIncrement(FWriteCount);  
  if Sync then
    FLocker.Beginwrite;
end;

procedure TChunkLight.EndWrite(const sync: Boolean);
begin
  if Sync then
    FLocker.Endwrite;
  if (InterlockedDecrement(FWriteCount) = 0) and (FOnLightUpdate <> nil) then
     FOnLightUpdate;
end;

procedure TChunkLight.BeginRead;
begin
  FLocker.Beginread;
end;

procedure TChunkLight.EndRead;
begin
  FLocker.Endread;
end;

function TChunkLight.Editing: Boolean;
begin
  Exit(FWriteCount>0);
end;

procedure TChunkLight.AddLight(const x, y, z: integer; LightLevel: TLongLight; const maxDepth: integer; const Force: boolean);
var
  Side : TTextureMode;
  OldLight : TLongLight;    
  c : TChunkLight;
begin        
  c := OnNeightborQuery(x, y, z);
  if c = nil then
     Exit;
  if c <> self then
  begin
     c.AddLight(x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask, LightLevel, maxDepth, Force);
     Exit;
  end;

  BeginWrite(False);
  try
    LightLevel := max(LightLevel - Resistance[x, y, z], Source[x, y, z]);

    OldLight := Value[x, y, z];

    if Force then
      UpdateIfGreater(LightLevel, OldLight)
    else
      if (maxDepth < 0) or (OldLight >= LightLevel) then
        exit;

    Value[x, y, z] := max(OldLight, LightLevel);
    if FOnBlockLightUpdate <> nil then
       FOnBlockLightUpdate(x, y, z);

    LightLevel := LightLevel-LENGTH_LIGHT_RESISTANCE;
    if (maxDepth > 0) and (LightLevel.Value > 0) then
      for side := Low(TTextureMode) to High(TTextureMode) do
        AddLight(x + TextureModeSidesI[side][axisX], y + TextureModeSidesI[side][axisY], z + TextureModeSidesI[side][axisZ], LightLevel, maxDepth - DepthResistance[side], False);
  finally
    EndWrite(False);
  end;
end;

procedure TChunkLight.RelightArea(const x1, y1, z1, x2, y2, z2: integer);
type
    TLightColors = set of TLightColor;

var
  buf : TThreeDimensionalSignedArrayOfBoolean;
  minX, minY, minZ, maxX, maxY, maxZ : integer;
  UsedChunks : TChunkLightSet;

  function UseChunk(c : TChunkLight) : TChunkLight;
  begin
    if UsedChunks.Contain(c) then
    begin
       c.BeginWrite(False);
       UsedChunks.Add(c);
    end;
    Exit(c);
  end;

  procedure RemoveLight(const Coord : TIntVector3; const LightLevel : TLongLight; Colors : TLightColors);
  var
    OldLight : TLongLight;
    lc : TLightColor;
    side : TTextureMode;
    c : TChunkLight;
  begin
    if buf.DataByVector[Coord] or ((Coord[axisX] > x1) and (Coord[axisX] < x2) and (Coord[axisY] > y1) and
      (Coord[axisY] < y2) and (Coord[axisZ] > z1) and (Coord[axisZ] < z2)) then
      exit;
    c := OnNeightborQuery(Coord[axisX], Coord[axisY], Coord[axisZ]);
    if (c = nil) then
      exit;
    UseChunk(c);

    UpdateIfGreater(maxX, Coord[axisX]);
    UpdateIfGreater(maxY, Coord[axisY]);
    UpdateIfGreater(maxZ, Coord[axisZ]);
    UpdateIfLesser(minX, Coord[axisX]);
    UpdateIfLesser(minY, Coord[axisY]);
    UpdateIfLesser(minZ, Coord[axisZ]);

    OldLight := c.Value[Coord[axisX] and ChunkSizeMask, Coord[axisY] and ChunkSizeMask, Coord[axisZ] and ChunkSizeMask];
    if FOnBlockLightUpdate <> nil then
       FOnBlockLightUpdate(Coord[axisX] and ChunkSizeMask, Coord[axisY] and ChunkSizeMask, Coord[axisZ] and ChunkSizeMask);

    for lc in Colors do
      if OldLight[lc] > LightLevel[lc] then
        Exclude(Colors, lc);

    if Colors = [] then
      exit
    else
      buf.DataByVector[Coord] := True;

    c.Value[Coord[axisX] and ChunkSizeMask, Coord[axisY] and ChunkSizeMask, Coord[axisZ] and ChunkSizeMask] := AsLightZero;

    for lc := low(TLightColor) to High(TLightColor) do
      if OldLight[lc] = 0 then
        Exclude(Colors, lc);

    if Colors <> [] then
      for side := Low(TTextureMode) to High(TTextureMode) do
        RemoveLight(Coord + TextureModeSidesI[side], OldLight-1, Colors);
  end;

var
  side : TTextureMode;
  x, y, z, nx, ny, nz, d : integer;
  v : TLongLight;
  c, c2 : TChunkLight;
begin
  UsedChunks := TChunkLightSet.Create(7);
  try
    buf := TThreeDimensionalSignedArrayOfBoolean.Create(x1, y1, z1);
    minX := x1;
    minY := y1;
    minZ := z1;
    maxX := x2;
    maxY := y2;
    maxZ := z2;

    for x := x1 to x2 do
      for y := y1 to y2 do
        for z := z1 to z2 do
          if ((x > x1) and (x < x2) and (y > y1) and (y < y2) and (z > z1) and (z < z2)) then
          begin
            c := OnNeightborQuery(x, y, z);
            if c <> nil then
               UseChunk(c).Value[x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask] := AsLightZero;
            buf[x, y, z] := True;
          end
          else
            RemoveLight(IntVector3(x, y, z), TLongLight(AsLightMax)+1, [lcRed, lcBlue, lcGreen]);

    d := max(ADD_LIGHT_DEFAULT_DEPTH, maxX+maxY+maxZ-minX-minY-minZ);
    for x := minX to maxX do
      for y := minY to maxY do
        for z := minZ to maxZ do
          if buf[x, y, z] then
          begin
            c := OnNeightborQuery(x, y, z);
            if c = nil then
              Continue;
            v := AsLight(LENGTH_LIGHT_RESISTANCE);
            for side := Low(TTextureMode) to High(TTextureMode) do
            begin
              nx := x + TextureModeSidesI[side][axisX];
              ny := y + TextureModeSidesI[side][axisY];
              nz := z + TextureModeSidesI[side][axisZ];
              if buf[nx, ny, nz] then
                Continue;
              c2 := OnNeightborQuery(nx, ny, nz);
              if c2 <> nil then
                UpdateIfGreater(v, c2.Value[nx and ChunkSizeMask, ny and ChunkSizeMask, nz and ChunkSizeMask]);
            end;
            UseChunk(c).AddLight(x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask, v - LENGTH_LIGHT_RESISTANCE, d, True);
          end;

    buf.Free;
  finally
    for c in UsedChunks.GetValueEnumerator do
        c.EndWrite(False);
    UsedChunks.Free;
  end;
end;

procedure TChunkLight.RelightBlock(const x, y, z: integer);
begin
  RelightArea(x, y, z, x, y, z);
end;

procedure TChunkLight.RelightAll;
begin
  RelightArea(0, 0, 0, ChunkSize-1, ChunkSize-1, ChunkSize-1);
end;

constructor TChunkLight.Create(AValue, ASource, AResistance: TLightCube;
  const ANeightborQuery: TNeightborQuery;
  const ALightUpdate: TUpdatedLightEvent; ABlockLighUpdate: TUpdatedBlockLight);
begin
  FDepthResistance := DefaultDepthResistance;
  OnNeightborQuery := ANeightborQuery;
  OnLightUpdate := ALightUpdate;
  FValue := AValue;
  FSource := ASource;
  FResistance := AResistance;
  FOnBlockLightUpdate := ABlockLighUpdate;
  FLocker := TMultiReadExclusiveWriteSynchronizer.Create;
  FWriteCount := 0;
end;

destructor TChunkLight.Destroy;
begin
  while FWriteCount > 0 do
    TThread.Yield;
  FSource.Free;
  FValue.Free;
  FResistance.Free;
  FLocker.Free;
  inherited Destroy;
end;

{ TCoordKeyLightValueMap }

function TCoordKeyLightValueMap.CreateHash(const Key: TIntVector3): PtrUInt;
begin
  Result := Key[AxisX] + Key[AxisY] shl ChunkSizeLog2 + Key[AxisZ] shl (ChunkSizeLog2 shl 1);
end;

end.

