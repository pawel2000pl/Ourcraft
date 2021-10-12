unit glowstone;

{$mode objfpc}{$H+}

interface

uses
  OurUtils, Models, CalcUtils, OurGame, LightTypes, TextureMode;

type

  { TGlowStone }

  TGlowStone = class(TBlock)
  private
    FSubID : LongWord;
  public
    function GetSubID: integer; override;
    procedure CreateDarkModel(var DarkModel: TDarkModel; const sides: TTextureDrawSides); override;
    function LightSource: TLight; override;
    constructor Create(MyCreator: TElementCreator);
  end;

  { TGlowStoneCreator }

  TGlowStoneCreator = class(TBlockCreator)
  private
    fTexture : PTextureRect;
  public
    property Texture : PTextureRect read fTexture;
    function getTextID: ansistring; override;

    procedure AfterLoading; override;
    function CreateElement(const Coords: TVector3; const SubID: integer=0): TGlowStone; override;
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

function TGlowStoneCreator.CreateElement(const Coords: TVector3;
  const SubID: integer): TGlowStone;
var
  g : TGlowStone;
begin
   g := TGlowStone.Create(self);
   g.FSubID:=SubID;
   Exit(g);
end;

{ TGlowStone }

function TGlowStone.GetSubID: integer;
begin
  Result:=FSubID;
end;

procedure TGlowStone.CreateDarkModel(var DarkModel: TDarkModel; const sides: TTextureDrawSides);
var
  side : TTextureMode;
begin
  for side in sides do
    DarkModel.AddWall(TextureStandardModeCoord[side], TextureStandardCorners, (Creator as TGlowStoneCreator).fTexture);
end;

function TGlowStone.LightSource: TLight;
begin
  Result:=AsLight(MAX_LIGHT_LEVEL * (1 - (FSubID and 1)), MAX_LIGHT_LEVEL * (1 - ((FSubID and 2) shr 1)), MAX_LIGHT_LEVEL * (1 - ((FSubID and 4) shr 2)));
end;

constructor TGlowStone.Create(MyCreator: TElementCreator);
begin
  inherited Create(MyCreator);
  FSubID:=0;
end;

end.

