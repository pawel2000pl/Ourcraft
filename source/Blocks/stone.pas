unit stone;

{$mode objfpc}
interface

uses
  OurUtils, Models, CalcUtils;

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
    function CreateNew(const SubID: integer): TObject; override;
    constructor Create(OurGame: TAbstractGame);
  end;

procedure LoadBlocks(Register : TCreatorRegister; OurGame : TAbstractGame);

implementation

uses
  OurGame;

procedure LoadBlocks(Register : TCreatorRegister; OurGame : TAbstractGame);
begin
  Register(TStoneCreator.Create(OurGame));
end;

{ TStoneCreator }

function TStoneCreator.getTextID: ansistring;
begin
  Result:='stone';
end;

procedure TStoneCreator.AfterLoading;
begin
  fTexture := (GetOurGame as TOurGame).Textures.GetTexture('stone');
end;

function TStoneCreator.CreateNew(const SubID: integer): TObject;
begin
  Result := TStone.Create(self);
end;

constructor TStoneCreator.Create(OurGame: TAbstractGame);
begin
  inherited;
end;

{ TStone }

function TStone.Clone: TBlock;
begin
  Result := Creator.CreateNew(Self.getSubID) as TBlock;
end;

procedure TStone.DrawModel(Chunk: TOurChunk; Side: TTextureMode; const Coord: TBlockCoord);
begin
  Chunk.GetVertexModel(side).AddWall(RealCoord(Chunk.Position, Coord), TextureStandardModeCoords[side], TextureStandardCorners, (Creator as TStoneCreator).fTexture, Chunk.GetLightedSide(Coord, Side));
end;

end.
