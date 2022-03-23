unit PlayerEntity;

{$Mode ObjFpc}

interface

uses
  SysUtils, Classes, OurGame, OurUtils, CalcUtils, GlCamera, CollisionBoxes, OurConstants;

type

  { TPlayerEntity }

  TPlayerEntity = class(TEntity)
  const
    EyePosition = 1.78;
  private
    fHeadRotation : TRotationVector;
    fCamera : TGlCamera;
    fRenderArea : TRenderArea;
    fStayOnBlock : Boolean;

    function DvOverDt(const Value : Double) : Double;
  public
    procedure Render; override;
    procedure UpdateModel; override;

    procedure RenderCameraView;

    function MaxVelocity : Double; virtual;
    function MaxJumpVelocity : Double; virtual;

    procedure OnCollisionWithBlock(Blocks: TCollisionBoxSet); override;
    procedure OnOutOfCollisionWithBlock; override;
    function StayOnBlock : Boolean; inline;
    procedure MoveAction(const ForwardValue, LeftValue : Double; const Time : Double);
    procedure MoveHeadAction(const Up, Direction : Double; const Time : Double);
    procedure JumpAction;

    constructor Create(TheWorld : TOurWorld; MyCreator : TElementCreator; const APosition : TVector3);
    procedure OnTick(const DeltaTime: QWord); override;
  end;

  { TPlayerEntityCreator }

  TPlayerEntityCreator = class(TEntityCreator)
  public
    function CreateElement(AWorld: TOurWorld; const Coords: TVector3; const SubID: integer=0): TEnvironmentElement; override; overload;
    function GetTextID: ansistring; override;
  end;
                 
procedure RegisterElementCreator(Environment : TEnvironment; Register : TRegisterCreatorMethod);

implementation

uses
  Math;

procedure RegisterElementCreator(Environment: TEnvironment; Register: TRegisterCreatorMethod);
begin
  Register(TPlayerEntityCreator.Create(Environment));
end;

{ TPlayerEntity }

function TPlayerEntity.DvOverDt(const Value: Double): Double;
begin
  Exit(Value / MaxVelocity - 1);
end;

procedure TPlayerEntity.Render;
begin
  //TODO: Model ?
end;

procedure TPlayerEntity.UpdateModel;
begin
  //TODO: Model ?
end;

procedure TPlayerEntity.RenderCameraView;
begin
  if (fCamera = nil) or (fRenderArea = nil) then
     Exit;
  fCamera.Position := StateBox.CollisionBox.Position - Vector3(0, StateBox.CollisionBox.Size[AxisY] + EyePosition, 0);
  fCamera.SetRotation(fHeadRotation[AxisX], fHeadRotation[AxisY], fHeadRotation[AxisZ]);
  fCamera.SetMatrix;
  World.OurGame.Textures.SelectTextures;
  fRenderArea.SetPosition(IntVector3(floor(fCamera.Position[axisX] / ChunkSize), floor(fCamera.Position[axisY] / ChunkSize), floor(fCamera.Position[axisZ] / ChunkSize)));
  fRenderArea.DrawBlocks;
end;

procedure TPlayerEntity.OnCollisionWithBlock(Blocks: TCollisionBoxSet);
var
  Box : TCollisionBox;
begin
  inherited OnCollisionWithBlock(Blocks);
  for box in Blocks.GetValueEnumerator do
    if Box.Position[AxisY] + Box.Size[AxisY]/2 <= StateBox.CollisionBox.Position[AxisY] - StateBox.CollisionBox.Size[AxisY]/2 then
    begin
       fStayOnBlock:=True;
       Exit;
    end;
end;

procedure TPlayerEntity.OnOutOfCollisionWithBlock;
begin
  fStayOnBlock:=False;
end;

function TPlayerEntity.StayOnBlock: Boolean;
begin
  Exit(fStayOnBlock);
end;

function TPlayerEntity.MaxVelocity: Double;
begin
  Exit(2);
end;

function TPlayerEntity.MaxJumpVelocity: Double;
begin
  Exit(4.852215988597375);
end;

procedure TPlayerEntity.MoveAction(const ForwardValue, LeftValue: Double; const Time: Double);
begin
  StateBox.Velocity += Vector3(-LeftValue, 0, -ForwardValue) * Time * Max(0, DvOverDt(StateBox.Velocity.&Length));;
end;

procedure TPlayerEntity.MoveHeadAction(const Up, Direction: Double; const Time: Double);
begin
   fHeadRotation := fHeadRotation + Vector3(Up, Direction, 0) * Time;
   fHeadRotation[AxisX] := EnsureRange(fHeadRotation[AxisX], -pi/2, pi/2);
end;

procedure TPlayerEntity.JumpAction;
begin
  if StayOnBlock then
     StateBox.Velocity[AxisY] += MaxJumpVelocity;
end;

constructor TPlayerEntity.Create(TheWorld: TOurWorld; MyCreator: TElementCreator; const APosition: TVector3);
begin
  inherited Create(TheWorld, MyCreator, APosition);
  fHeadRotation := Vector3(0, 0, 0);
  fCamera := nil;
  fRenderArea := nil;
end;

procedure TPlayerEntity.OnTick(const DeltaTime: QWord);
begin           
  if (Chunk = nil) then
     Exit;
  StateBox.Velocity += Vector3(0, -9.81, 0) * (DeltaTime);
  UpdateModel;
end;

{ TPlayerEntityCreator }

function TPlayerEntityCreator.CreateElement(AWorld: TOurWorld; const Coords: TVector3; const SubID: integer): TEnvironmentElement;
begin
  Result := TPlayerEntity.Create(AWorld, Self, Coords);
end;

function TPlayerEntityCreator.GetTextID: ansistring;
begin
  Result:='Player';
end;

end.
