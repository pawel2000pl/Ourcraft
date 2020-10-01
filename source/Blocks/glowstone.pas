unit glowstone;

{$mode objfpc}{$H+}

interface

uses
  OurUtils, Models, CalcUtils;

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
    function CreateNew(const SubID: integer): TObject; override;
    constructor Create(OurGame: TAbstractGame);
  end;

procedure LoadBlocks(Register : TCreatorRegister; OurGame : TAbstractGame);

implementation

uses
  OurGame;

procedure LoadBlocks(Register : TCreatorRegister; OurGame : TAbstractGame);
begin
  Register(TGlowStoneCreator.Create(OurGame));
end;

{ TGlowStoneCreator }

function TGlowStoneCreator.getTextID: ansistring;
begin
  Result:='Glowstone';
end;

procedure TGlowStoneCreator.AfterLoading;
begin
    fTexture := (GetOurGame as TOurGame).Textures.GetTexture('glowstone');
end;

function TGlowStoneCreator.CreateNew(const SubID: integer): TObject;
begin
   Result := TGlowStone.Create(self);
end;

constructor TGlowStoneCreator.Create(OurGame: TAbstractGame);
begin
   inherited;
end;

{ TGlowStone }

function TGlowStone.Clone: TBlock;
begin
  Result:=Creator.CreateNew(0) as TBlock;
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

