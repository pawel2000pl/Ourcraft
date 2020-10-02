unit WorldGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OurUtils, CalcUtils, RandomGenerator;

type

  { TWorldGenerator }

  TWorldGenerator = class(TAbstractGenerator)
  public
    procedure Generate(const Chunk: TOurChunk); override;
    constructor Create(const _DestroyWithWorld: Boolean=true);
  end;

implementation

{ TWorldGenerator }

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

constructor TWorldGenerator.Create(const _DestroyWithWorld: Boolean);
begin
  inherited;

end;

end.

