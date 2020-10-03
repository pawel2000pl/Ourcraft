unit WorldGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, OurUtils, CalcUtils, DeterminedRandomGenerator;

type

  TWorldGeneratorSettings = record
    WorldScale : TVector3;
  end;

const
  DefaultWorldGenratorSettings : TWorldGeneratorSettings = (WorldScale : (1/64, 1/256, 1/64));

type

  { TWorldGenerator }

  TWorldGenerator = class(TAbstractGenerator)
  private
    FRandomGenerator : TRandomGenerator;
    FSettings : TWorldGeneratorSettings;
  public
    property RandomGenerator : TRandomGenerator read FRandomGenerator;
    property Settings : TWorldGeneratorSettings read FSettings write FSettings;

    function GetRandom(const x, z : Integer; const MiddleLevel : Double) : Double; //0..1
    procedure Generate(const Chunk: TOurChunk); override;
    constructor Create(const Seed : QWord; const _DestroyWithWorld: Boolean=true);
    destructor Destroy; override;
  end;

implementation

{$RangeChecks off}

{ TWorldGenerator }

function TWorldGenerator.GetRandom(const x, z: Integer; const MiddleLevel: Double): Double;
var
  hr, ha : Double;
begin
  hr := RandomGenerator.LinearRandom([x*Settings.WorldScale[axisX], z*Settings.WorldScale[axisZ]], ExampleSeedOffset[1]+floor64(MiddleLevel*ExampleSeedOffset[5]))*2-1;
  ha := RandomGenerator.RandomAngle(x*Settings.WorldScale[axisX], ExampleSeedOffset[2]+floor64(MiddleLevel*ExampleSeedOffset[4])) + RandomGenerator.RandomAngle(z*Settings.WorldScale[axisZ], ExampleSeedOffset[3]+floor64(MiddleLevel*ExampleSeedOffset[4]));
  Result := RandomGenerator.LinearRandom([x*Settings.WorldScale[axisX]+hr*cos(ha), MiddleLevel, z*Settings.WorldScale[axisZ]+hr*sin(ha)], ExampleSeedOffset[3])*2-1;
end;

procedure TWorldGenerator.Generate(const Chunk: TOurChunk);   
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

constructor TWorldGenerator.Create(const Seed: QWord;
  const _DestroyWithWorld: Boolean);
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

