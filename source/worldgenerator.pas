unit WorldGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, OurUtils, CalcUtils, OurGame, DeterminedRandomGenerator, ArrayOfNumber;

type

  TWorldGeneratorSettings = record
    WorldScale : TVector3;
  end;

const
  DefaultWorldGenratorSettings : TWorldGeneratorSettings =
    (WorldScale : (1 / 64, 1/64, 1 / 64));

type

  TBiomeTemperature = -16..15;    //temperatura
  TBiomeHeight = -16384..16383;   //wysokość
  TBiomeHumidity = 0..15;    //wilgotność

  TWorldLevel = class;

  TBiome = class
  public
    procedure Register(Level : TWorldLevel); virtual; abstract;
    function GetHeight(const x, z : integer) : integer; virtual; abstract;
  end;

  TBiomeTemplate = record
    Biome : TBiome;
    Temperature : TBiomeTemperature;
    Height : TBiomeHeight;
    Humidity : TBiomeHumidity;
  end;

  { TWorldLevel }

  TWorldLevel = class
  private
    BiomesTab : array[TBiomeTemperature, TBiomeHeight, TBiomeHumidity] of TBiome;
    BiomesList : array of TBiome;
    BiomeTemplates : array of TBiomeTemplate;
    procedure ProcessBiomes;
  public
    procedure RegsterBiome(Biome : TBiome; const Temperature : TBiomeTemperature;
      const Height : TBiomeHeight; const Humidity : TBiomeHumidity);
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
var
  i, j, k, m, n : integer;
  cl, ct : Integer;
  area : Integer;
  len : Double;
  Tab : array[TBiomeTemperature, TBiomeHeight, TBiomeHumidity] of TArrayOfDouble;
begin
  for i := Low(BiomesTab) to High(BiomesTab) do
    for j := Low(BiomesTab[i]) to High(BiomesTab[i]) do
      for k := low(BiomesTab[i, j]) to High(BiomesTab[i, j]) do
        BiomesTab[i, j, k] := nil;

  for i := Low(BiomesList) to High(BiomesList) do
    if (BiomesList[i] <> nil) and Assigned(BiomesList[i]) then
      BiomesList[i].Register(Self);

  cl := length(BiomesList);
  ct := length(BiomeTemplates);
  for i := Low(BiomesTab) to High(BiomesTab) do
    for j := Low(BiomesTab[i]) to High(BiomesTab[i]) do
      for k := low(BiomesTab[i, j]) to High(BiomesTab[i, j]) do
      begin
         Tab[i, j, k] := TArrayOfDouble.Create(cl);
         Area := 0;
         Len := 0;
         for m := 0 to cl-1 do
         begin
           for n := 0 to ct-1 do
           if BiomesList[m] = BiomeTemplates[n].Biome then
           begin
             Inc(Area);
             Len += hypot3(i-BiomeTemplates[n].Temperature, j-BiomeTemplates[n].Height, k-BiomeTemplates[n].Humidity);
           end;
           if Area = 0 then
             len := Infinity
             else
             len /= Area;
           Tab[i, j, k][m] := len;
         end;
      end;

  for i := Low(BiomesTab) to High(BiomesTab) do
    for j := Low(BiomesTab[i]) to High(BiomesTab[i]) do
      for k := low(BiomesTab[i, j]) to High(BiomesTab[i, j]) do
      begin
          BiomesTab[i, j, k] := BiomesList[Tab[i, j, k].GetMinIndex];
          Tab[i, j, k].Free;
      end;
end;

procedure TWorldLevel.RegsterBiome(Biome: TBiome;
  const Temperature: TBiomeTemperature; const Height: TBiomeHeight;
  const Humidity: TBiomeHumidity);
var
  h : Integer;
begin
   BiomesTab[Temperature, Height, Humidity] := Biome;
   h := length(BiomeTemplates);
   setlength(BiomeTemplates, h+1);
   BiomeTemplates[h].Biome:=Biome;
   BiomeTemplates[h].Temperature:=Temperature;
   BiomeTemplates[h].Height:=Height;
   BiomeTemplates[h].Humidity:=Humidity;
end;

function TWorldLevel.GetMiddleHeight : integer;
begin
  Result := 0;
  //todo
end;

function TWorldLevel.GetLevelHeight: Integer;
begin
  Result := 0;
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
    z * Settings.WorldScale[axisZ] + hr * sin(ha)], floor64(MiddleLevel*1024) + ExampleSeedOffset[3]);
end;

procedure TWorldGenerator.Generate(const Chunk : TOurChunk);
var
  x, y, z, h, l : integer;
  v : Double;
  stone, glow : TElementCreator; //TODO: Remove
begin
  with Chunk do
  begin
      Stone := World.Environment.GetCreator(World.Environment.GetID('stone'));
      glow := World.Environment.GetCreator(World.Environment.GetID('glowstone'));

      for x := 0 to ChunkSize - 1 do
          for z := 0 to ChunkSize-1 do
          begin
            v := 2*GetRandom(x + Position[axisX] shl ChunkSizeLog2, z + Position[axisZ] shl ChunkSizeLog2, 0)-1;
            //v := (cos(v*pi*2)/pi/2+v)/2;
            //v := sin(v*pi*2)/pi/2+v;
            //v := arctan(pi/2*v);
            //v := 2/pi*arctan(4*v*(1+intpower(2*v, 4)));
            //v := tan(v*pi/2);
            h := round(EnsureRange(v / 2 / Settings.WorldScale[axisY] - Position[axisY] shl ChunkSizeLog2, -1, ChunkSize-1));
            l := 0;
            for y := l to h do
                  if y mod 8 = 0 then
                  SetBlockDirect(x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask, glow.CreateElement(0) as TBlock)
                  else
                  SetBlockDirect(x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask, stone.CreateElement(0) as TBlock);
          end;
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
