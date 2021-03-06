unit MovingBlock;

{$mode objfpc}
interface

{DO NOT USE YET}

uses
  OurUtils,
  OurGame,
  CalcUtils, Models, TextureMode, LightTypes, PhisicalBox;

type

  { TMovingBlock }

  TMovingBlock = class(TEntity)
  private
       Model : TVertexModel;
       procedure InitBoxes;
  protected
       procedure AfterMovement; override;
  public
       constructor Create(TheWorld: TOurWorld; MyCreator: TElementCreator; const APosition: TVector3);
       destructor Destroy; override;
       procedure Render; override;
       procedure Tick(const DeltaTime: QWord); override;
       procedure UpdateModel;
  end;

  { TMovingBlockCreator }

  TMovingBlockCreator = class(TEntityCreator)
  private
       fTexture : PTextureRect;
  public
    constructor Create(AnEnvironment: TEnvironment);
    destructor Destroy; override;
    function CreateElement(AWorld: TOurWorld; const Coords: TVector3; const SubID: integer=0): TEnvironmentElement; override; overload;
    function getTextID: ansistring; override;
  end;

procedure RegisterElementCreator(Environment : TEnvironment; Register : TRegisterCreatorMethod);

implementation

procedure RegisterElementCreator(Environment: TEnvironment;
  Register: TRegisterCreatorMethod);
begin
  Register(TMovingBlockCreator.Create(Environment));
end;

{ TMovingBlock }

procedure TMovingBlock.InitBoxes;
begin
  MainCollisionBox.Size := Vector3(1, 1, 1);
  SetLength(PhisicalBoxes, 1);
  PhisicalBoxes[0] := TPhisicalBox.Create;
  PhisicalBoxes[0].Size := MainCollisionBox.Size;
  PhisicalBoxes[0].Position := Position;
end;

procedure TMovingBlock.AfterMovement;
begin
  inherited AfterMovement;
  PhisicalBoxes[0].Position := GetPosition;
  PhisicalBoxes[0].Rotate := GetRotation;
  UpdateModel;
end;

constructor TMovingBlock.Create(TheWorld: TOurWorld; MyCreator: TElementCreator; const APosition: TVector3);
begin             
  InitBoxes;
  inherited Create(TheWorld, MyCreator, APosition);
  Model := TVertexModel.Create;
  UpdateModel;
  AfterMovement;
end;

destructor TMovingBlock.Destroy;
begin
  Model.Free;
  inherited Destroy;
end;

procedure TMovingBlock.Render;
begin
   Model.JustDraw;
end;

procedure TMovingBlock.Tick(const DeltaTime: QWord);
begin
   PhisicalBoxes[0].AddGlobalAcceleration(Vector3(0, -9.81, 0)); //add gravity
   inherited Tick(DeltaTime);
   Position := PhisicalBoxes[0].Position;
   Rotation := PhisicalBoxes[0].Rotate;
   //writeln(Position[AxisX]:2:2, #9, Position[AxisY]:2:2, #9, Position[AxisZ]:2:2);
   UpdateModel;
end;

procedure TMovingBlock.UpdateModel;
var
  p : TVector3;
  side : TTextureMode;
  rc : TRectangleCorners;
  rl : TRealLight;
  i : Integer;
begin
  rl := GetLightLevel(Position);
  p := Position;
  Model.Lock;
  Model.Clear;
  for side := low(TTextureMode) to High(TTextureMode) do
  begin
    for i := 0 to 3 do                  //TODO: czemu transponowane?
        rc[i] := {Transposing}(MainCollisionBox.RotationMatrix)*(TextureStandardModeCoord[side][i]+Vector3(-0.5, -0.5, -0.5));
    Model.AddWall(p, rc, TextureStandardCorners, (Creator as TMovingBlockCreator).fTexture, rl);
  end;
  Model.Unlock;
end;

{ TMovingBlockCreator }

constructor TMovingBlockCreator.Create(AnEnvironment: TEnvironment);
begin
  inherited Create(AnEnvironment);
  fTexture := (Environment.Game as TOurGame).Textures.GetTexture('stone');
end;

destructor TMovingBlockCreator.Destroy;
begin
  inherited Destroy;
end;

function TMovingBlockCreator.CreateElement(AWorld: TOurWorld;
  const Coords: TVector3; const SubID: integer): TEnvironmentElement;
begin
  Result := TMovingBlock.Create(AWorld, Self, Coords);
end;

function TMovingBlockCreator.getTextID: ansistring;
begin
  Result:='MovingBlock';
end;

end.
