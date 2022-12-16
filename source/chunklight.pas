unit ChunkLight;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, LightTypes, OurConstants, TinyHashData, CalcUtils, TextureMode, Incrementations, LightCubes, ThreeDimensionalArrayOfAnything;

type

  TThreeDimensionalSignedArrayOfTLight = specialize TThreeDimensionalSignedArrayOfAnything<TLight>;
  TThreeDimensionalSignedArrayOfBoolean = specialize TThreeDimensionalSignedArrayOfAnything<Boolean>;

  TChunkLight = class;

  TNeightborQuery = function(const x, y, z : Integer) : TChunkLight of object;
  TUpdatedLightEvent = procedure of object;
  TUpdatedBlockLight = procedure(const x, y, z : Integer) of object;

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
    function NeightborQuery(const x, y, z : Integer) : TChunkLight;
    function NeightborQuery(const Coord : TIntVector3) : TChunkLight; overload;
  public
    property Value : TLightCube read FValue;
    property Source : TLightCube read FSource;
    property Resistance : TLightCube read FResistance;

    function GetValueExt(const x, y, z : Integer) : TLight;
    function GetSourceExt(const x, y, z : Integer) : TLight;
    function GetResistanceExt(const x, y, z : Integer) : TLight;
    function GetValueExt(const Coord : TIntVector3) : TLight; overload;
    function GetSourceExt(const Coord : TIntVector3) : TLight; overload;
    function GetResistanceExt(const Coord : TIntVector3) : TLight; overload;

    function GetValueAndSourceExt(const x, y, z : Integer) : TLight;
    function GetValueAndSourceExt(const Coord : TIntVector3) : TLight; overload;

    property DepthResistance[const Side : TTextureMode] : Integer read GetDepthResistance write SetDepthResistance;

    procedure BeginWrite(const sync : Boolean = True);
    procedure EndWrite(const sync : Boolean = True);
    procedure BeginRead;
    procedure EndRead;
    function Editing : Boolean;

    property OnNeightborQuery : TNeightborQuery read GetOnNeightborQuery write SetOnNeightborQuery;
    property OnLightUpdate : TUpdatedLightEvent read FOnLightUpdate write SetOnLightUpdate;
    property OnBlockLightUpdate : TUpdatedBlockLight read FOnBlockLightUpdate write SetOnBlockLightUpdate;

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


implementation

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

function TChunkLight.NeightborQuery(const x, y, z: Integer): TChunkLight;
begin
  Exit(FOnNeightborQuery(x, y, z));
end;

function TChunkLight.NeightborQuery(const Coord: TIntVector3): TChunkLight;
begin
  Exit(FOnNeightborQuery(Coord.x, Coord.y, Coord.z));
end;

function TChunkLight.GetValueExt(const x, y, z: Integer): TLight;
var
  c : TChunkLight;
begin
  c := FOnNeightborQuery(x, y, z);
  if c = nil then
     Exit(AsLightZero)
  else
     Exit(c.Value[x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask]);
end;

function TChunkLight.GetSourceExt(const x, y, z: Integer): TLight;
var
  c : TChunkLight;
begin
  c := FOnNeightborQuery(x, y, z);
  if c = nil then
     Exit(AsLightZero)
  else
     Exit(c.Source[x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask]);
end;

function TChunkLight.GetResistanceExt(const x, y, z: Integer): TLight;
var
  c : TChunkLight;
begin
  c := FOnNeightborQuery(x, y, z);
  if c = nil then
     Exit(AsLightZero)
  else
     Exit(c.Resistance[x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask]);
end;

function TChunkLight.GetValueExt(const Coord: TIntVector3): TLight;
begin
  Exit(GetValueExt(Coord[AxisX], Coord[AxisY], Coord[AxisZ]));
end;

function TChunkLight.GetSourceExt(const Coord: TIntVector3): TLight;
begin
  Exit(GetSourceExt(Coord[AxisX], Coord[AxisY], Coord[AxisZ]));
end;

function TChunkLight.GetResistanceExt(const Coord: TIntVector3): TLight;
begin
  Exit(GetResistanceExt(Coord[AxisX], Coord[AxisY], Coord[AxisZ]));
end;

function TChunkLight.GetValueAndSourceExt(const x, y, z: Integer): TLight;
var
  c : TChunkLight;
begin
  c := FOnNeightborQuery(x, y, z);
  if c = nil then
     Exit(AsLightZero)
  else
     Exit(Max(c.Value[x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask], c.Source[x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask]));
end;

function TChunkLight.GetValueAndSourceExt(const Coord: TIntVector3): TLight;
begin
  Exit(GetValueAndSourceExt(Coord[AxisX], Coord[AxisY], Coord[AxisZ]));
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

procedure TChunkLight.RelightArea(const x1, y1, z1, x2, y2, z2: integer);
type
    TLightColors = set of TLightColor;

var
  buf : TThreeDimensionalSignedArrayOfTLight;
  buf2 : TThreeDimensionalSignedArrayOfBoolean;
  UsedChunks : TChunkLightSet;

  procedure RemoveLight(const Coord : TIntVector3; const LightLevel : TLongLight; Colors : TLightColors);
  var
    OldLight : TLongLight;
    lc : TLightColor;
    side : TTextureMode;
    c : TChunkLight;
  begin
    if buf2.DataByVector[Coord] then
       Exit;

    c := NeightborQuery(Coord);
    if (c = nil) then
      exit;
    OldLight := c.Value[Coord[axisX] and ChunkSizeMask, Coord[axisY] and ChunkSizeMask, Coord[axisZ] and ChunkSizeMask];

    for lc in Colors do
      if OldLight[lc] > LightLevel[lc] then
        Exclude(Colors, lc);

    if Colors = [] then
      exit;

    buf.DataByVector[Coord] := AsLightZero;
    buf2.DataByVector[Coord] := True;

    for lc := low(TLightColor) to High(TLightColor) do
      if OldLight[lc] = 0 then
        Exclude(Colors, lc);

    if Colors <> [] then
      for side := Low(TTextureMode) to High(TTextureMode) do
          RemoveLight(Coord + TextureModeSidesI[side], OldLight-1, Colors);
  end;

  procedure AddLight(const Coord : TIntVector3; LightLevel: TLongLight; const maxDepth: integer; const Force: boolean);
  var
    Side : TTextureMode;
    OldLight : TLongLight;
    c : TChunkLight;
    MaskedCoord : TIntVector3;
  begin
    c := NeightborQuery(Coord);
    if c = nil then
       Exit;
    MaskedCoord := Coord.Mask(ChunkSizeMask);
    LightLevel := max(LightLevel - c.Resistance[MaskedCoord.X, MaskedCoord.Y, MaskedCoord.Z], c.Source[MaskedCoord.X, MaskedCoord.Y, MaskedCoord.Z]);

    if buf2.DataByVector[Coord] then
        OldLight := buf.DataByVector[Coord]
    else
        OldLight := c.Value[MaskedCoord.X, MaskedCoord.Y, MaskedCoord.Z];

    if Force then
      UpdateIfGreater(LightLevel, OldLight)
    else
      if (maxDepth < 0) or (OldLight >= LightLevel) then
        exit;

    buf.DataByVector[Coord] := max(OldLight, LightLevel);
    buf2.DataByVector[Coord] := True;

    LightLevel := LightLevel-LENGTH_LIGHT_RESISTANCE;
    if (maxDepth > 0) and (LightLevel.Value > 0) then
      for side := Low(TTextureMode) to High(TTextureMode) do
        AddLight(Coord + TextureModeSidesI[side], LightLevel, maxDepth - DepthResistance[side], False);
  end;

  function UseChunk(c : TChunkLight) : TChunkLight;
  begin
    if not UsedChunks.Contain(c) then
    begin
       c.BeginWrite(False);
       UsedChunks.Add(c);
    end;
    Exit(c);
  end;

  procedure ApplyChanges;
  var
    c : TChunkLight;
    coord : TIntVector3;
    Masked : TIntVector3;
  begin
    for coord in buf2 do
      if buf2.DataByVector[coord] then
      begin
        c := NeightborQuery(coord);
        if c = nil then
           Continue;
        UseChunk(c);
        Masked := coord.Mask(ChunkSizeMask);
        c.Value[Masked.x, Masked.y, Masked.z] := buf.DataByVector[coord];
        if c.FOnBlockLightUpdate <> nil then
          c.FOnBlockLightUpdate(Masked.x, Masked.y, Masked.z);
      end;

    for c in UsedChunks.GetKeyEnumerator do
        c.EndWrite(False);
  end;

var
  side : TTextureMode;
  x, y, z, d : integer;
  NewCoord : TIntVector3;
  v : TLongLight;
  coord : TIntVector3;
begin
  UsedChunks := TChunkLightSet.Create(13);

  try
    buf := TThreeDimensionalSignedArrayOfTLight.Create(x1, y1, z1);
    buf2 := TThreeDimensionalSignedArrayOfBoolean.Create(x1, y1, z1);

    for x := x1+1 to x2-1 do
      for y := y1+1 to y2-1 do
        for z := z1+1 to z2-1 do
        begin
          buf[x, y, z] := AsLightZero;
          buf2[x, y, z] := True;
        end;

    for x := x1 to x2 do
      for y := y1 to y2 do
        for z := z1 to z2 do
          if (not ((x > x1) and (x < x2) and (y > y1) and (y < y2) and (z > z1) and (z < z2))) then
            RemoveLight(IntVector3(x, y, z), TLongLight(AsLightMax)+1, [lcRed, lcBlue, lcGreen]);

    d := max(ADD_LIGHT_DEFAULT_DEPTH, x2+y2+z2-x1-y1-z1);

    for coord in buf2 do
      if buf2.DataByVector[coord] then
      begin
        if (NeightborQuery(coord) = nil) then
          Continue;
        v := AsLight(LENGTH_LIGHT_RESISTANCE);
        for side := Low(TTextureMode) to High(TTextureMode) do
        begin
          NewCoord := coord + TextureModeSidesI[side];
          if not buf2.DataByVector[NewCoord] then
            UpdateIfGreater(v, GetValueExt(NewCoord));
        end;
        AddLight(coord, v - LENGTH_LIGHT_RESISTANCE, d, True);
      end;

    ApplyChanges;
  finally
    buf.Free;
    buf2.Free;
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

{ TChunkLightSet }

function TChunkLightSet.CreateHash(const Key: TChunkLight): PtrUInt;
begin
  Exit((QWord(Key) xor (QWord(Key) shl 32) or (1 shl 63)) mod 2147483647);
end;

end.

