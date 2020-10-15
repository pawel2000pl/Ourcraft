unit stone;

{$mode objfpc}
interface

uses
  OurUtils, Models, CalcUtils, OurEnvironment;

type

  { TStone }

  TStone = class(TBlock)
  public
    function Clone : TBlock; override;
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
    function CreateElement(const SubID: integer=0): TEnvironmentElement; override;
    constructor Create(AnEnvironment: TEnvironment);
  end;

procedure RegisterElementCreator(Environment : TEnvironment; Register : TRegisterCreatorMethod);

implementation

uses
  OurGame;

procedure RegisterElementCreator(Environment: TEnvironment;
  Register: TRegisterCreatorMethod);
begin
  Register(TStoneCreator.Create(Environment));
end;

{ TStoneCreator }

function TStoneCreator.getTextID: ansistring;
begin
  Result:='stone';
end;

procedure TStoneCreator.AfterLoading;
begin
  fTexture := (Environment.Game as TOurGame).Textures.GetTexture('stone');
end;

function TStoneCreator.CreateElement(const SubID: integer): TEnvironmentElement;
begin
  Result := TStone.Create(self);
end;

constructor TStoneCreator.Create(AnEnvironment: TEnvironment);
begin
  inherited;
end;

{ TStone }

function TStone.Clone: TBlock;
begin
  Result := Creator.CreateElement(Self.getSubID) as TBlock;
end;

procedure TStone.DrawModel(Chunk: TOurChunk; Side: TTextureMode; const Coord: TBlockCoord);
begin
  Chunk.GetVertexModel(side).AddWall(RealCoord(Chunk.Position, Coord), TextureStandardModeCoords[side], TextureStandardCorners, (Creator as TStoneCreator).fTexture, Chunk.GetLightedSide(Coord, Side));
end;

end.
