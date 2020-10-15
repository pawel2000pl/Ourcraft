unit glowstone;

{$mode objfpc}{$H+}

interface

uses
  OurUtils, Models, CalcUtils, OurGame;

type

  { TGlowStone }

  TGlowStone = class(TBlock)
    function Clone : TBlock; override;
    procedure DrawModel(Chunk: TOurChunk; Side: TTextureMode; const Coord: TBlockCoord); override;
    function LightSource: integer; override;
  end;

  { TGlowStoneCreator }

  TGlowStoneCreator = class(TBlockCreator)
  private
    fTexture : PTextureRect;
  public
    property Texture : PTextureRect read fTexture;
    function getTextID: ansistring; override;

    procedure AfterLoading; override;
    function CreateElement(const SubID: integer=0): TEnvironmentElement; override;
  end;

procedure RegisterElementCreator(Environment : TEnvironment; Register : TRegisterCreatorMethod);


implementation

procedure RegisterElementCreator(Environment: TEnvironment;
  Register: TRegisterCreatorMethod);
begin
  Register(TGlowStoneCreator.Create(Environment));
end;

{ TGlowStoneCreator }

function TGlowStoneCreator.getTextID: ansistring;
begin
  Result:='Glowstone';
end;

procedure TGlowStoneCreator.AfterLoading;
begin
    fTexture := (Environment.Game as TOurGame).Textures.GetTexture('glowstone');
end;

function TGlowStoneCreator.CreateElement(const SubID: integer
  ): TEnvironmentElement;
begin
   Result := TGlowStone.Create(self);
end;

{ TGlowStone }

function TGlowStone.Clone: TBlock;
begin
  Result:=Creator.CreateElement(0) as TBlock;
end;

procedure TGlowStone.DrawModel(Chunk: TOurChunk; Side: TTextureMode;
  const Coord: TBlockCoord);
begin
  Chunk.GetVertexModel(side).AddWall(RealCoord(Chunk.Position, Coord), TextureStandardModeCoords[side], TextureStandardCorners, (Creator as TGlowStoneCreator).fTexture, Chunk.GetLightLevel(coord[axisX], coord[axisY], coord[axisZ]));
end;

function TGlowStone.LightSource: integer;
begin
  Result:=MAX_LIGHT_LEVEL;
end;

end.

