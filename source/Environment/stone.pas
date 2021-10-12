unit stone;

{$mode objfpc}
interface

uses
  OurUtils, Models, CalcUtils, OurGame, TextureMode;

type

  { TStone }

  TStone = class(TBlock)
  public
    procedure CreateDarkModel(var DarkModel: TDarkModel; const sides: TTextureDrawSides); override;
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

procedure TStone.CreateDarkModel(var DarkModel: TDarkModel; const sides: TTextureDrawSides);
var
  side : TTextureMode;
begin
  for side in sides do
    DarkModel.AddWall(TextureStandardModeCoord[side], TextureStandardCorners, (Creator as TStoneCreator).fTexture);
end;

end.
