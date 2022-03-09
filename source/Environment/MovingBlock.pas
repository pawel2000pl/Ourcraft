unit MovingBlock;

{$mode objfpc}
interface

uses
  SysUtils,
  OurUtils,
  OurGame,
  CalcUtils, Models, TextureMode, LightTypes, Locker;

type

  { TMovingBlock }

  TMovingBlock = class(TEntity)
  private
       Model : TVertexModel;
       PlacingBlock : TBlock;
       PlacingBlockLocker : TLocker;
       DarkModel : TDarkModel;
       FDynamicLightTimeUpdate : QWord;
       procedure InitBoxes;
       function GetPlacingBlock : TBlock;
       procedure MoveDynamicLight;
  protected
       procedure AfterMovement; override;    
       function GetDefaultBlock : TBlock; virtual; //creating
  public
       constructor Create(TheWorld: TOurWorld; MyCreator: TElementCreator; const APosition: TVector3);
       destructor Destroy; override;
       procedure Render; override;
       procedure Tick(const DeltaTime: QWord); override;
       procedure UpdateModel; override;
       procedure UpdateModelLight; override;

       procedure OnLeaveChunk(AChunk: TOurChunk); override;
       procedure OnEnterChunk(AChunk: TOurChunk); override;

       procedure SetPlacingBlock(Block : TBlock);
       procedure PlaceBlock;
       procedure FittingToGrid;
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
  StateBox.CollisionBox.Size := Vector3(1, 1, 1);
end;

function TMovingBlock.GetPlacingBlock: TBlock;
begin
  PlacingBlockLocker.Lock;
  try
    if (PlacingBlock = nil) or (not Assigned(PlacingBlock)) then
      PlacingBlock := GetDefaultBlock;
  finally
    Result := PlacingBlock;
    PlacingBlockLocker.Unlock;
  end;
end;

function TMovingBlock.GetDefaultBlock: TBlock;
begin
  Result := GetEnvironment.GetCreator(GetEnvironment.GetID('STONE')).CreateElement(Floor(Position)) as TBlock;
end;

procedure TMovingBlock.MoveDynamicLight;
var
  Info : TDynamicLightRecord;
begin                            
  Info.Value := GetPlacingBlock.LightSource;
  if (Info.Value = AsLightZero) or (Chunk = nil) or (GetTickCount64 - FDynamicLightTimeUpdate < World.DynamicLightUpdateInterval) then
     Exit;
  if Info.Value <> AsLightZero then
  begin
    Info.Coord := Round(Position);
    Chunk.MoveDynamicLightSource(Self, Info);
  end;
  FDynamicLightTimeUpdate := GetTickCount64;
end;

procedure TMovingBlock.AfterMovement;
begin
  inherited AfterMovement;
  UpdateModel;
end;

constructor TMovingBlock.Create(TheWorld: TOurWorld; MyCreator: TElementCreator; const APosition: TVector3);
begin                        
  InitBoxes;
  Model := TVertexModel.Create;
  FDynamicLightTimeUpdate := GetTickCount64;
  PlacingBlock := nil;
  PlacingBlockLocker := TLocker.Create;
  inherited Create(TheWorld, MyCreator, APosition);
  GetPlacingBlock.CreateDarkModel(DarkModel, AllTextureSides);
  UpdateModel;
  AfterMovement;
end;

destructor TMovingBlock.Destroy;
begin
  if PlacingBlock <> nil then
     FreeAndNil(PlacingBlock);
  Model.Free;
  PlacingBlockLocker.Free;
  inherited Destroy;
end;

procedure TMovingBlock.Render;
begin
  Model.Lock;
  try
     Model.JustDraw;
  finally
     Model.Unlock;
  end;
end;

procedure TMovingBlock.Tick(const DeltaTime: QWord);
begin
  if (Chunk = nil) then
     Exit;
  if SquaredHypot3(Velocity) < 1e-2 then
  begin
    FittingToGrid;
    if SquaredHypot3(Position - 0.5 - floor(Position)) < 1e-2 then
    begin
      PlaceBlock;//World.Queues.AddMethod(@PlaceBlock);
      Exit;
    end;
  end;             
  StateBox.Velocity += Vector3(0, -9.81, 0) * (DeltaTime/1000);
  MoveDynamicLight;
  UpdateModel;
end;

procedure TMovingBlock.UpdateModel;
var
  p : TVector3;
  rc : TRectangleCorners;
  rl : TRealLight;
  i, j : Integer;
  halfVector : TVector3;
begin
  if (Chunk = nil) then Exit;

  rl := max(GetLightLevel(Position), LightLevelToFloat(GetPlacingBlock.LightSource));
  p := Position;
  Model.Lock;
  try
    Model.Clear;

    halfVector := Vector3(-0.5, -0.5, -0.5);
    for i := 0 to DarkModel.AddCount-1 do
    begin
        for j := low(DarkModel.WallCorners[i]) to High(DarkModel.WallCorners[i]) do
           rc[j] := StateBox.CollisionBox.RotationMatrix*(DarkModel.WallCorners[i][j]+halfVector);
        Model.AddWall(p, rc, DarkModel.TextureCorners[i], DarkModel.TextureRects[i], rl);
    end;
  finally
    Model.Unlock;
  end;
end;

procedure TMovingBlock.UpdateModelLight;
var
  i : Integer; 
  rl : TColor3b;
  c : ^TColor3b;
begin
  rl := max(GetLightLevel(Position), LightLevelToFloat(GetPlacingBlock.LightSource));

  Model.Lock;
  try
    c := Model.ColorPtr;
    for i := 0 to Model.Count-1 do
        c[i] := rl;
  finally        
    Model.Unlock;
  end;
end;

procedure TMovingBlock.OnLeaveChunk(AChunk: TOurChunk);
begin
  AChunk.RemoveDynamicLightSource(Self);
end;

procedure TMovingBlock.OnEnterChunk(AChunk: TOurChunk);
var
  Info : TDynamicLightRecord;
begin
  Info.Value := GetPlacingBlock.LightSource;
  if Info.Value <> AsLightZero then
  begin
    Info.Coord := Round(Position);
    AChunk.AddDynamicLightSource(Self, Info);
  end;
end;

procedure TMovingBlock.SetPlacingBlock(Block: TBlock);
begin
  PlacingBlockLocker.Lock;
  try
    if PlacingBlock <> nil then
       FreeAndNil(PlacingBlock);
    PlacingBlock := Block;
  finally
    PlacingBlockLocker.Unlock;
  end;
  DarkModel := TDarkModel.Empty;
  GetPlacingBlock.CreateDarkModel(DarkModel, AllTextureSides);
end;

procedure TMovingBlock.PlaceBlock;
var
  IntPosition : TIntVector3;
begin
  PlacingBlockLocker.Lock;
  try
    IntPosition := floor(Position);
    if World.SetBlock(IntPosition[axisX], IntPosition[axisY], IntPosition[axisZ], GetPlacingBlock) then
      PlacingBlock := nil;
  finally
    PlacingBlockLocker.Unlock;
  end;
  Unregister;
  World.FreeThread.FreeObject(Self, 100);
end;

procedure TMovingBlock.FittingToGrid;
var
  delta : TVector3;
begin
  delta := floor(Position) - Position + 0.5;
  Velocity := Velocity + delta;
  Position := Position + delta/16;
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
