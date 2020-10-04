unit WorldGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, OurUtils, CalcUtils, DeterminedRandomGenerator;

type

  TWorldGeneratorSettings = record
    WorldScale : TVector3;
  end;

const
  DefaultWorldGenratorSettings : TWorldGeneratorSettings =
    (WorldScale : (1 / 64, 1 / 256, 1 / 64));

type

  TBiomeTemperature = int8;
  TBiomeHeight = int16;

  TWorldLevel = class;

  TBiome = class
  public
    procedure Register(Level : TWorldLevel); virtual; abstract;
    function GetHeight(const x, z : integer) : integer; virtual; abstract;
  end;

  { TWorldLevel }

  TWorldLevel = class
  private
    BiomesTab : array[TBiomeTemperature, TBiomeHeight] of TBiome;
    BiomesList : array of TBiome;
    procedure ProcessBiomes;
  public
    procedure RegsterBiome(Biome : TBiome; const Temperature : TBiomeTemperature;
      const Height : TBiomeHeight);
    function GetMiddleHeight : integer;
    function GetLevelHeight : Integer;
  end;


  { TWorldGenerator }

  TWorldGenerator = class(TAbstractGenerator)
  private
    FRandomGenerator : TRandomGenerator;
    FSettings : TWorldGeneratorSettings;
  public
    property RandomGenerator : TRandomGenerator read FRandomGenerator;
    property Settings : TWorldGeneratorSettings read FSettings write FSettings;

    function GetRandom(const x, z : integer; const MiddleLevel : double) : double; //0..1
    procedure Generate(const Chunk : TOurChunk); override;
    constructor Create(const Seed : QWord; const _DestroyWithWorld : boolean = True);
    destructor Destroy; override;
  end;

implementation

{ TWorldLevel }

procedure TWorldLevel.ProcessBiomes;
const
  Directions : array[0..3, 0..1] of Integer = ((1, 0), (0, 1), (-1, 0), (0, -1));
var
  i, j, k : integer;
  changes : boolean;
begin
  for i := Low(BiomesTab) to High(BiomesTab) do
    for j := Low(BiomesTab[i]) to High(BiomesTab[i]) do
      BiomesTab[i, j] := nil;
  for i := Low(BiomesList) to High(BiomesList) do
    if (BiomesList[i] <> nil) and Assigned(BiomesList[i]) then
      BiomesList[i].Register(Self);

  repeat
    changes := False;
    for i := Low(BiomesTab) to High(BiomesTab) do
      for j := Low(BiomesTab[i]) to High(BiomesTab[i]) do
        if BiomesTab[i, j] = nil then
          for k := 0 to 3 do
          if (i+Directions[k, 0] <= High(BiomesTab)) and (j+Directions[k, 1] <= High(BiomesTab[i])) and (BiomesTab[i+Directions[k, 0], j+Directions[k, 1]] <> nil) then
          begin
            BiomesTab[i, j] := BiomesTab[i+Directions[k, 0], j+Directions[k, 1]];
            Changes := True;
            break;
          end;

  until not changes;
end;

procedure TWorldLevel.RegsterBiome(Biome : TBiome; const Temperature : TBiomeTemperature;
  const Height : TBiomeHeight);
begin
   BiomesTab[Temperature, Height] := Biome;
end;

function TWorldLevel.GetMiddleHeight : integer;
begin
  //todo
end;

function TWorldLevel.GetLevelHeight: Integer;
begin
  //todo
end;

{$RangeChecks off}

{ TWorldGenerator }

function TWorldGenerator.GetRandom(const x, z : integer;
  const MiddleLevel : double) : double;
var
  hr, ha : double;
begin
  hr := RandomGenerator.LinearRandom([x * Settings.WorldScale[axisX],
    z * Settings.WorldScale[axisZ]], ExampleSeedOffset[1] + floor64(
    MiddleLevel * ExampleSeedOffset[5])) * 2 - 1;
  ha := RandomGenerator.RandomAngle(x * Settings.WorldScale[axisX],
    ExampleSeedOffset[2] + floor64(MiddleLevel * ExampleSeedOffset[4])) +
    RandomGenerator.RandomAngle(z * Settings.WorldScale[axisZ],
    ExampleSeedOffset[3] + floor64(MiddleLevel * ExampleSeedOffset[4]));
  Result := RandomGenerator.LinearRandom([x * Settings.WorldScale[axisX] + hr * cos(ha),
    MiddleLevel, z * Settings.WorldScale[axisZ] + hr * sin(ha)], ExampleSeedOffset[3]) * 2 - 1;
end;

procedure TWorldGenerator.Generate(const Chunk : TOurChunk);
var
  x, y, z : integer;
  stone, glow : TCustomCreator; //TODO: Remove
begin
  with Chunk do
  begin
    if (Position[axisY] < 1) or ((Position[axisY] = 1) and
      ((Position[axisX] + Position[axisZ]) and 1 = 0)) then
    begin
      Stone := World.OurGame.GetCreator(World.OurGame.GetID('stone'));
      glow := World.OurGame.GetCreator(World.OurGame.GetID('glowstone'));
      for x := 0 to ChunkSize - 1 do
        for y := 0 to ChunkSize shr 1 do
          for z := 0 to ChunkSize - 1 do
            SetBlockDirect(x, y, z, stone.CreateNew(0) as TBlock);

      SetBlockDirect(0, ChunkSize div 2, 0, glow.CreateNew(0) as TBlock);
    end;

    if (Position[axisX] = 2) and (Position[axisY] = 0) and (Position[axisZ] = 2) then
      for x := 0 to ChunkSize - 1 do
        for z := 0 to ChunkSize - 1 do
          SetBlockDirect(x, ChunkSize div 2 + 4, z, glow.CreateNew(0) as TBlock);
  end;
end;

constructor TWorldGenerator.Create(const Seed : QWord; const _DestroyWithWorld : boolean);
begin
  inherited Create(_DestroyWithWorld);
  FRandomGenerator := TRandomGenerator.Create(Seed);
  Settings := DefaultWorldGenratorSettings;
end;

destructor TWorldGenerator.Destroy;
begin
  FRandomGenerator.Free;
  inherited Destroy;
end;

end.
