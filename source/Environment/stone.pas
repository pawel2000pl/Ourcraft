unit stone;

{$mode objfpc}
interface

uses
  OurUtils, Models, CalcUtils, OurGame, TextureMode;

type

  { TStone }

  TStone = class(TBlock)
  public
    procedure DrawModel(Chunk: TOurChunk; Side: TTextureMode; const Coord: TBlockCoord); override;
         //TODO: Draw
  end;

  { TStoneCreator }

  TStoneCreator = class(TBlockCreator)
  private
    fTexture : PTextureRect;
  public
    property Texture : PTextureRect read fTexture;
    function getTextID: ansistring; override;

    procedure AfterLoading; override;
    function CreateElement(const Coords: TVector3; const SubID: integer=0): TStone; override;
  end;

procedure RegisterElementCreator(Environment : TEnvironment; Register : TRegisterCreatorMethod);

implementation

procedure RegisterElementCreator(Environment: TEnvironment;
  Register: TRegisterCreatorMethod);
begin
  Register(TStoneCreator.Create(Environment));
end;

{ TStoneCreator }

function TStoneCreator.getTextID: ansistring;
begin
  Result:='Stone';
end;

procedure TStoneCreator.AfterLoading;
begin
  fTexture := (Environment.Game as TOurGame).Textures.GetTexture('stone');
end;

function TStoneCreator.CreateElement(const Coords: TVector3;
  const SubID: integer): TStone;
begin
  Result := TStone.Create(self);
end;

{ TStone }

procedure TStone.DrawModel(Chunk: TOurChunk; Side: TTextureMode; const Coord: TBlockCoord);
begin
  Chunk.GetVertexModel(side).AddWall(RealCoord(Chunk.Position, Coord), TextureStandardModeCoord[side], TextureStandardCorners, (Creator as TStoneCreator).fTexture, Chunk.GetLightedSide(Coord, Side));
end;

end.
