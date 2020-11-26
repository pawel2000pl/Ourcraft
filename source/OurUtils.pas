unit OurUtils;

{$mode objfpc}
{$RangeChecks off}
{$ModeSwitch advancedrecords}

interface

uses
  SysUtils, Classes, Math, Models, CalcUtils, Sorts, Freerer, Queues,
  Locker, ProcessUtils, ThreeDimensionalArrayOfBoolean, Collections,
  OurGame, Incrementations;

const
  ChunkSizeLog2 = 4;
  WorldSizeLog2 = ChunkSizeLog2 + 1;
  ChunkSize = 1 shl ChunkSizeLog2;
  WorldSize = 1 shl WorldSizeLog2; //size of HashTable
  ChunkSizeMask = ChunkSize - 1;
  WorldSizeMask = WorldSize - 1;

  MAX_BLOCK_TRANSPARENCY = 15;
  MAX_LIGHT_LEVEL = 15;

type

  TEntityShape = record
    Position : TVector3; //important for rotation
    Center : TVector3;
    Size : TSizeVector;
    Rotate : TRotationVector; //through center
    Texture : PTextureRect;
    TextureCorners : TCubeTextureCorners;
  end;

  { TMovingShape }

  TMovingShape = class
  private
    fShape : TEntityShape;
    Base : TEntityShape;
    Dest : TEntityShape;
  public
    LastUpdate : QWord;
    TimeLength : QWord;
    procedure SetDestination(const es : TEntityShape);
    property Shape : TEntityShape read fShape write SetDestination;

    function CreateVertexModel(const LightLevel : integer) : TVertexModel;
    procedure Calc(const CurrentTime : QWord);  //getTickCount

    constructor Create;
    destructor Destroy; override;
  end;

  { TModel }

  TModel = class
  protected
    fShapeCount : integer;
    fShapes : array of TMovingShape;
  public
    //TODO: loading, saving, updating

    function GetShape(const index : integer) : TMovingShape;
    property Shapes[Index : integer] : TMovingShape read GetShape;
    property ShapeCount : integer read fShapeCount;
    function CreateVertexModel(const LightLevel : integer) : TVertexModel; //need free!

    constructor Create();
    destructor Destroy; override;
  end;

  TBlock = class;
  TOurChunk = class;
  TOurWorld = class;
                         
  { TAbstractGenerator }

  TAbstractGenerator = class
  private
    FDestroyWithWorld : boolean;
  public
    property DestroyWithWorld : Boolean read FDestroyWithWorld write FDestroyWithWorld;
    procedure Generate(const Chunk : TOurChunk); virtual; abstract;
    constructor Create(const _DestroyWithWorld : Boolean = true);
  end;

  { TEntity }

  TEntity = class(TEnvironmentElement)
  private
    FModel: TModel;
    FPosition: TVector3;
    FRotate: TRotationVector;
    FRotateVelocity: TRotationVector;
    FVelocity: TVector3;
    FChunk : TOurChunk;
    fWorld : TOurWorld;
    procedure SetModel(AValue: TModel);
    procedure SetPosition(AValue: TVector3);
    procedure SetRotate(AValue: TRotationVector);
    procedure SetRotateVelocity(AValue: TRotationVector);
    procedure SetVelocity(AValue: TVector3);
  public
    property Position : TVector3 read FPosition write SetPosition;
    property Velocity : TVector3 read FVelocity write SetVelocity;
    property Rotate : TRotationVector read FRotate write SetRotate;
    property RotateVelocity : TRotationVector read FRotateVelocity write SetRotateVelocity;
    property Model : TModel read FModel write SetModel;
    property Chunk : TOurChunk read FChunk;
    property World : TOurWorld read fWorld;

    procedure UpdateChunk;

    function GetID : integer; virtual; abstract;
    procedure OnTick(const DeltaTime : QWord); virtual; abstract;
    procedure OnHit(Entity : TEntity); virtual; abstract; //left click
    procedure OnOptions(Entity : TEntity); virtual; abstract; //right click
    procedure UpdateHitBoxes; virtual; abstract;

    constructor Create(TheWorld : TOurWorld; MyCreator : TElementCreator); //TODO: EntityCreator
  end;

type
  TBlockDataTag = record
    Size : integer;
    Buf : PByte;
  end;
  PBlockDataTag = ^TBlockDataTag;

  { TBlockCreator }

  TBlockCreator = class(TElementCreator)
  public
    function GetType : TElementType; override; 
    //warning: Coord for CreateElement must be floor
    function CreateBlock(const Coord : TIntVector3; const SubID : Integer) : TBlock;
  end;

  { TBlock }

  TBlock = class(TEnvironmentElement)
  private
    function GetBlockCreator: TBlockCreator;
  public
    property BlockCreator : TBlockCreator read GetBlockCreator;
    function GetTag : PBlockDataTag; virtual;

    function NeedDraw : boolean; virtual;
    function IsSolid : boolean; virtual; //not draw on every frame / regular cube
    function NeedTick : boolean; virtual;
    function NeedRandomTick : boolean; virtual;
    function HasAnimation : boolean; virtual; //draw on every frame
    function NeedAfterPut : boolean; virtual;
    function NeedNearChangeUpdate : boolean; virtual;

    //TBlockCoord is relative in-chunk coord
    procedure {%H-}DrawModel(Chunk : TOurChunk; Side : TTextureMode; const Coord : TBlockCoord); virtual; abstract;
    procedure {%H-}DrawUnsolid(Chunk : TOurChunk; const Coord : TBlockCoord); virtual; abstract;
    procedure {%H-}DrawAnimation(Chunk : TOurChunk; const Coord : TBlockCoord); virtual; abstract;
    procedure {%H-}OnTick(Chunk : TOurChunk; const Coord : TBlockCoord); virtual; abstract;
    procedure {%H-}OnRandomTick(Chunk : TOurChunk; const Coord : TBlockCoord); virtual; abstract;
    procedure {%H-}AfterPut(Chunk : TOurChunk; const Coord : TBlockCoord); virtual; abstract;
    procedure {%H-}NearChangeUpdate(Chunk : TOurChunk; const side : TTextureMode; const Coord : TBlockCoord); virtual; abstract;

    procedure SaveToStream(Stream: TStream; {%H-}Chunk : TOurChunk; const {%H-}Coord: TBlockCoord); virtual; //inherited first!
    procedure LoadFromStream(Stream: TStream; Chunk : TOurChunk; const Coord: TBlockCoord);virtual; //inherited first!

    function Transparency : integer; virtual;
    function LightSource : integer; virtual;
    function Clone(const NewCoord : TIntVector3) : TBlock; virtual;

    function GetHashCode: PtrInt; override;
  end;

  { TExtendedBlock }

  TExtendedBlock = class(TBlock)
  private
    FCoord : TBlockCoord;
    FChunk : TOurChunk;
    function FieldsNotSet : Boolean; inline;
  public
    property GetCoord : TBlockCoord read FCoord;
    property GetChunk : TOurChunk read FChunk;
    procedure SetAsChanged;
    function NeedAfterPut: boolean; override;
    procedure AfterPut(Chunk: TOurChunk; const Coord: TBlockCoord); override;
    constructor Create(MyCreator: TElementCreator);
    constructor Create(MyCreator: TElementCreator; const AChunk : TOurChunk; const MyCoord : TBlockCoord);
    constructor Create(MyCreator: TElementCreator; const World : TOurWorld; const MyAbsoluteCoord : TIntVector3);
  end;

  { TBlockList }

  TBlockList = class(specialize TCustomSet<TBlockCoord>)
  private
    FLocker : TLocker;
  public
    property Locker : TLocker read FLocker;

    procedure Add(Item: TItem); override;
    procedure Remove(const i: Integer); override;
    function GetNext(var i: integer; var Item: TItem): Boolean; overload; override;
    function IndexOf(const Item: TItem): Integer; override;

    constructor Create;
    destructor Destroy; override;
  end;

  TRenderArea = class;
  TRenderAreaChange = procedure(Area : TRenderArea) of object;

  { TRenderAreaSort }

  TRenderAreaSort = class(specialize TStaticSort<TRenderArea>)
  public
    class function Compare(const a, b : TValue) : integer; override;
  end;

  { TRenderAreaSearcher }

  TRenderAreaSearcher = class(specialize TStaticBSearch<TRenderArea, TRenderArea>)
  public
    class function Compare(const a : TValue; const b : TKey) : integer; override;
  end;

  TLightFunctions = record
    SetLight : procedure(const x, y, z, Value : integer) of object;
    GetLight : function(const x, y, z : integer) : integer of object;
    GetLightSource : function(const x, y, z : integer) : integer of object;
    GetExtLightSource : function(const x, y, z : integer) : integer of object;
    SetExtLight : procedure(const x, y, z, Value : integer) of object;
    GetExtLight : function(const x, y, z : integer) : integer of object;
  end;

  TLightFunctionKind = (lfkBlock, lfkSun);
  TRenderAreaCollection = specialize TCustomSet<TRenderArea>;

  { TOurChunk }

  TOurChunk = class(TFreeObject)
  const
    MaxDynamicBlockChanged = ChunkSize*ChunkSize*ChunkSize div 4; //powyżej tej ilości, musi być wysłany cały chunk
  private
    fLockUpdateModelLight : boolean;
    fAutoLightUpdate : boolean;
    fPosition : TIntVector3;  //Coord div ChunkSize
    fdefaultBlock : TBlockCreator;
    NeedModelLightUpdate : set of TTextureMode;
    NeedModelSolidUpdate : set of TTextureMode;
    fWorld : TOurWorld;
    fLights : array[0..ChunkSize - 1, 0..ChunkSize - 1, 0..ChunkSize - 1] of byte;
    fSunLight : array[0..ChunkSize - 1, 0..ChunkSize - 1, 0..ChunkSize - 1] of byte;
    fLoaded : boolean;
    fGenerated : boolean;
    fLighted : boolean; //first lighting up (sun)
    //Entities : array of TEntity;

    BlocksForTick : TBlockList;
    BlocksForRandomTick : TBlockList;
    UnSolid : TBlockList;
    Animated : TBlockList;

    ChangedBlocks : TBlockList;

    //TODO: Save, Load
    fNeightbors : array[TTextureMode] of TOurChunk;
    fBlocks : array[0..ChunkSize - 1, 0..ChunkSize - 1, 0..ChunkSize - 1] of TBlock;

    fModels : array[TTextureMode] of TVertexModel;
    fUnsolidModel : TVertexModel;
    fAnimationModels : TVertexModel;

    fRenderAreaCollection : TRenderAreaCollection;

    LightFunctions : array[TLightFunctionKind] of TLightFunctions;

    procedure AddLight(const x, y, z : integer; LightLevel : integer; const Functions : TLightFunctionKind; const maxDepth : integer = 32; const Force : boolean = False);

    function GetBlock(const x, y, z : integer) : TBlock;      
    procedure SetBlock(const x, y, z : integer; AValue : TBlock);
    function GetNeightbors(const Index : TTextureMode) : TOurChunk;
    procedure Load;
    procedure Generate;
  protected
    procedure Finalize(const DelayTime : QWord); override;
  public
    property RenderAreaCollection : TRenderAreaCollection read fRenderAreaCollection;

    property Loaded : boolean read fLoaded;
    property Generated : boolean read fGenerated;
    property Lighted : boolean read fLighted;
    property AutoLightUpdate : boolean read fAutoLightUpdate write fAutoLightUpdate;

    procedure UpdateNeightborLight;

    //local Coord
    function SetBlockDirectAuto(const x, y, z, ID, SubID : Integer) : Boolean; //return false, if ID is not a block
    property Blocks[const x, y, z : integer] : TBlock read GetBlock write SetBlock;
    function GetBlockDirect(const x, y, z : Integer) : TBlock;
    procedure SetBlockDirect(const x, y, z : Integer; AValue : TBlock);
    property DirectBlocks[const x, y, z : integer] : TBlock read GetBlockDirect write SetBlockDirect;
    class function IsInsert(const x, y, z : Integer) : boolean; inline;
    class function IsBorder(const x, y, z : Integer) : boolean; inline;
    procedure RegisterChangedBlock(const Coord : TBlockCoord);

    //must be in-chunk Coord
    function GetLightLevel(const x, y, z : integer) : integer;
    function GetSunLightLevel(const x, y, z : integer) : integer;
    function GetBlockLightLevel(const x, y, z : integer) : integer;
    procedure SetSunLightLevel(const x, y, z : integer; const AValue : integer);
    procedure SetBlockLightLevel(const x, y, z : integer; const AValue : integer);
    function GetBlockLightSource(const x, y, z : integer) : integer;
    function GetSunLightSource(const x, y, z : integer) : integer;
    //could be extercnal Coord
    function GetExtLightLevel(const x, y, z : integer) : integer;
    function GetExtBlockLightLevel(const x, y, z : integer) : integer;
    procedure SetExtBlockLightLevel(const x, y, z, Value : integer);
    function GetExtSunLightLevel(const x, y, z : integer) : integer;
    procedure SetExtSunLightLevel(const x, y, z, Value : integer);
    function GetExtBlockLightSource(const x, y, z : integer) : integer;
    function GetExtSunLightSource(const x, y, z : integer) : integer;

    procedure NeightborLightUpdate(const x, y, z : integer); inline;

    procedure UpdateSunLight(const x1, z1, x2, z2 : integer; const FromY : integer = ChunkSize - 1; const ToY : integer = 0; goDown : boolean = True);

    function GetSmoothLightLevel(const v : TVector3) : double; overload;
    function GetSmoothLightLevel(const v : TVector3; const side : TTextureMode) : double; overload;
    function GetLightedSide(const Coord : TBlockCoord; const mode : TTextureMode) : TLightedSide;
    procedure UpdateModelLight;
    procedure ForceUpdateModelLight;

    procedure RelightArea(const x1, y1, z1, x2, y2, z2 : integer); overload;
    procedure RelightArea(const x1, y1, z1, x2, y2, z2 : integer; const LightMode : TLightFunctionKind); overload;

    function GetVertexModel(const Side : TTextureMode) : TVertexModel; inline;
    property VertexModels[const side : TTextureMode] : TVertexModel read GetVertexModel;
    property UnsolidModel : TVertexModel read fUnsolidModel;
    property AnimationModel : TVertexModel read fAnimationModels;

    procedure UpdateVertexModels;
    procedure UpdateUnsolidModel;
    procedure UpdateAnimationModel;

    procedure Tick;
    procedure RandomTick(const Count : integer);

    procedure UpdateNeightbors(Ignore : TOurChunk = nil);
    procedure AddNeightbor(Who : TOurChunk; const from : TTextureMode);
    procedure RemoveNeightbor(const from : TTextureMode);
    property Neightbors[const Index : TTextureMode] : TOurChunk read GetNeightbors;
    function GetNeightborFromBlockCoord(const x, y, z : integer) : TOurChunk;
    //koordynaty względem chunku

    function GetEnvironment : TEnvironment;

    property Environment : TEnvironment read GetEnvironment;
    property LightLevel[const x, y, z : integer] : integer read GetLightLevel;
    property World : TOurWorld read fWorld;
    property Position : TIntVector3 read fPosition;

    procedure RelistBlocks;
    procedure RelistBlock(const x, y, z : integer);

    procedure SaveToStream(Stream : TStream);
    procedure SaveChangesToStream(Stream : TStream);
    procedure LoadFromStream(Stream : TStream);

    function GetHashCode: PtrInt; override;

    constructor Create(defaultBlock : TBlockCreator; const MyPosition : TIntVector3; const OurWorld : TOurWorld);
    destructor Destroy; override;
  end;

  TOurChunkTable = record
    Locker : TLocker;
    Table : array of TOurChunk;
    Count : integer;
  end;

  TIsVisibledPointFunction = function(const Point : TVector3; const Size : double) : boolean of object;

  { TRenderArea }

  TRenderArea = class
  private
    FOnChage : TRenderAreaChange;
    FReloadingID : QWord;
    fRepaintingID : QWord;
    fx, fy, fz : integer;
    fRay : integer;
    fOnChange : TRenderAreaChange;
    fWorld : TOurWorld;
    fBorderWidth : Double;

    FIsVisibledPointFunction : TIsVisibledPointFunction;
    function AlwaysTrue(const {%H-}Point : TVector3; const {%H-}Size : double) : boolean;
    function GetIsVisibledPointFunction : TIsVisibledPointFunction;
    procedure SetBorderWidth(AValue: Double);
    procedure SetIsVisibledPointFunction(AValue : TIsVisibledPointFunction);
    procedure SetOnChage(AValue : TRenderAreaChange);
    procedure SetRay(AValue : integer);
    procedure SetReloadingID(AValue : QWord);
    procedure setX(AValue : integer);
    procedure setY(AValue : integer);
    procedure setZ(AValue : integer);
    procedure OnChangeEvent;
  public
    property IsVisibledPointFunction : TIsVisibledPointFunction
      read GetIsVisibledPointFunction write SetIsVisibledPointFunction;
    property OnChage : TRenderAreaChange read FOnChage write SetOnChage;

    property BorderWidth : Double read fBorderWidth write SetBorderWidth;
    function GetPosition : TIntVector3;
    property World : TOurWorld read fWorld;
    property Ray : integer read fRay write SetRay;
    property X : integer read fx write setX;
    property Y : integer read fy write setY;
    property Z : integer read fz write setZ;
    procedure SetPosition(const Position : TIntVector3);

    procedure DrawBlocks;
    procedure RepaintBlocks;

    property ReloadingID : QWord read FReloadingID write SetReloadingID;

    constructor Create(OurWorld : TOurWorld);
    destructor Destroy; override;
  end;

  { TBlockInformation }

  TBlockInformation = record
    Block : TBlock;
    Chunk : TOurChunk;
    Coord : TBlockCoord;
    procedure Clear;
    function Exists : Boolean;
    procedure Init(_Block : TBlock; _Chunk : TOurChunk; const _Coord : TBlockCoord);
    function AbsoluteCoord : TIntVector3;
  end;

  { TOurWorld }

  TOurWorld = class
  private       //TODO: everything
    fChunks : array[0..WorldSize - 1, 0..WorldSize - 1, 0..WorldSize - 1] of TOurChunkTable;
    fDefaultBlock : TBlockCreator;
    fOurGame : TOurGame;
    fEnvironment : TEnvironment;

    fFreeThread : TFree;
    fQueues : TQueueManager2;
    fRenderAreaSet : TRenderAreaCollection;

    fTickDelay : QWord;
    fRandomTickSpeed : QWord;

    TickLocker : TLocker;
    UpdateRenderAreaLocker : TLocker;

    fGenerator : TAbstractGenerator;

    procedure UpdateRenderArea(Area : TRenderArea);  //todo
  public
    property RenderAreaSet : TRenderAreaCollection read fRenderAreaSet;
    property Environment : TEnvironment read fEnvironment;
    property TickDelay : Qword read fTickDelay write fTickDelay;
    property RandomTickSpeed : Qword read fRandomTickSpeed write fRandomTickSpeed;
    property OurGame : TOurGame read fOurGame;
    property Queues : TQueueManager2 read fQueues;
    property FreeThread : TFree read fFreeThread;

    function GetChunk(const ChunkX, ChunkY, ChunkZ : integer) : TOurChunk;
    function GetChunkFromBlockCoors(const x, y, z : integer) : TOurChunk;
    function GetBlock(const x, y, z : integer) : TBlock;
    procedure SetBlock(const x, y, z : integer; AValue : TBlock);       
    function GetBlockInformation(const x, y, z : integer) : TBlockInformation;

    procedure UnloadChunk(const ChunkX, ChunkY, ChunkZ : integer);
    function LoadChunk(const ChunkX, ChunkY, ChunkZ : integer) : TOurChunk;
    procedure UpdateTableLengths;
    procedure RemoveOrphanChunks;

    function AddRenderArea(const x, y, z, ray : integer) : TRenderArea;

    procedure Tick;
    procedure RandomTickAuto;
    procedure RandomTick(const Count : integer);

    property Generator : TAbstractGenerator read fGenerator;

    constructor Create(defaultBlock : TBlockCreator; Game : TOurGame; WorldGenerator : TAbstractGenerator);
    destructor Destroy; override;

  end;

function EntityShapeToTexturedCuboid(const EntityShape : TEntityShape)
  : TTexturedCuboid;
function RealCoord(const ChunkPosition : TIntVector3;
  const BlockPosition : TBlockCoord) : TVector3;

implementation

{$RangeChecks off}
{$macro on}

const
  NilBlockCoord : TBlockCoord = (255, 255, 255);

function RealCoord(const ChunkPosition : TIntVector3;
  const BlockPosition : TBlockCoord) : TVector3;
begin
  Result[axisX] := ChunkPosition[axisX] shl ChunkSizeLog2 + BlockPosition[axisX];
  Result[axisY] := ChunkPosition[axisY] shl ChunkSizeLog2 + BlockPosition[axisY];
  Result[axisZ] := ChunkPosition[axisZ] shl ChunkSizeLog2 + BlockPosition[axisZ];
end;

function EntityShapeToTexturedCuboid(const EntityShape : TEntityShape) : TTexturedCuboid;
var
  i : integer;
  side : TTextureMode;
  c : TMatrix3x3;
  a : TAxis;
  v : TVector3;
begin
  c := CreateRotateMatrix(EntityShape.Rotate);
  Result.Textures := EntityShape.TextureCorners;
  for side := low(TTextureMode) to High(TTextureMode) do
    for i := 0 to 3 do
    begin
      for a := low(TAxis) to High(TAxis) do
        v[a] := TextureStandardModeCoord[side][i][a] *
          EntityShape.Size[a] - EntityShape.Center[a];
      Result.Corners[side][i] := c * v;
    end;
end;

{ TExtendedBlock }

function TExtendedBlock.FieldsNotSet: Boolean;
begin
  Result := (FChunk = nil) or (FCoord = NilBlockCoord);
end;

procedure TExtendedBlock.SetAsChanged;
begin
  GetChunk.RegisterChangedBlock(GetCoord);
end;

function TExtendedBlock.NeedAfterPut: boolean;
begin
  Result:=true;
end;

procedure TExtendedBlock.AfterPut(Chunk: TOurChunk; const Coord: TBlockCoord);
begin
  if FieldsNotSet then
  begin
    Chunk := Chunk;
    FCoord := Coord;
  end;
end;

constructor TExtendedBlock.Create(MyCreator: TElementCreator);
begin
  FChunk := nil;
  FCoord := NilBlockCoord;
  inherited Create(MyCreator);
end;

constructor TExtendedBlock.Create(MyCreator: TElementCreator;
  const AChunk: TOurChunk; const MyCoord: TBlockCoord);
begin     
  FChunk := AChunk;
  FCoord := MyCoord;
  inherited Create(MyCreator);
end;

constructor TExtendedBlock.Create(MyCreator: TElementCreator;
  const World: TOurWorld; const MyAbsoluteCoord: TIntVector3);
begin
  FChunk := World.GetChunkFromBlockCoors(MyAbsoluteCoord[axisX], MyAbsoluteCoord[axisY], MyAbsoluteCoord[axisZ]);
  FCoord := BlockCoord(MyAbsoluteCoord[axisX] and ChunkSizeMask, MyAbsoluteCoord[axisY] and ChunkSizeMask, MyAbsoluteCoord[axisZ] and ChunkSizeMask);
  inherited Create(MyCreator);
end;

{ TBlockInformation }

procedure TBlockInformation.Clear;
begin
  Block := nil;
  Chunk := nil;
  Coord := NilBlockCoord;
end;

function TBlockInformation.Exists: Boolean;
begin
  Result := (Chunk <> nil) and (Block <> nil);
end;

procedure TBlockInformation.Init(_Block: TBlock; _Chunk: TOurChunk;
  const _Coord: TBlockCoord);
begin
  Block := _Block;
  Chunk := _Chunk;
  Coord := _Coord;
end;

function TBlockInformation.AbsoluteCoord: TIntVector3;
begin
  if Exists then
    Result := Chunk.Position * ChunkSize + Coord
    else
    Result := IntVector3(0, 0, 0);
end;

{ TBlockList }

procedure TBlockList.Add(Item: TItem);
begin
  Locker.Lock;
  inherited Add(Item);
  Locker.Unlock;
end;

procedure TBlockList.Remove(const i: Integer);
begin           
  Locker.Lock;
  inherited Remove(i);  
  Locker.Unlock;
end;

function TBlockList.GetNext(var i: integer; var Item: TItem): Boolean;
begin
  Locker.Lock;
  Result:=inherited GetNext(i, Item); 
  Locker.Unlock;
end;

function TBlockList.IndexOf(const Item: TItem): Integer;
begin           
  Locker.Lock;
  Result:=inherited IndexOf(Item); 
  Locker.Unlock;
end;

constructor TBlockList.Create;
begin
  FLocker := TLocker.Create;
  inherited Create;
end;

destructor TBlockList.Destroy;
begin
  FLocker.Free;
  inherited Destroy;
end;

{ TAbstractGenerator }

constructor TAbstractGenerator.Create(const _DestroyWithWorld: Boolean);
begin
   FDestroyWithWorld:=_DestroyWithWorld;
end;

{ TRenderAreaSearcher }

class function TRenderAreaSearcher.Compare(const a : TValue; const b : TKey) : integer;
begin
  if (TRenderArea(a).GetHashCode > TRenderArea(b).GetHashCode) then
    Result := 1
  else if (TRenderArea(a).GetHashCode < TRenderArea(b).GetHashCode) then
    Result := -1
  else
    Result := 0;
end;

{ TRenderAreaSort }

class function TRenderAreaSort.Compare(const a, b : TValue) : integer;
begin
  if (TRenderArea(a).GetHashCode > TRenderArea(b).GetHashCode) then
    Result := 1
  else if (TRenderArea(a).GetHashCode < TRenderArea(b).GetHashCode) then
    Result := -1
  else
    Result := 0;
end;

{ TRenderArea }

procedure TRenderArea.SetRay(AValue : integer);
begin
  if fRay = AValue then
    Exit;
  if AValue < 2 then
    AValue := 2
  else if AValue > 64 then
    AValue := 64;
  fRay := AValue;
  Inc(FReloadingID);
  World.Queues.AddMethod(@OnChangeEvent);
end;

procedure TRenderArea.SetReloadingID(AValue : QWord);
begin
  if FReloadingID = AValue then
    Exit;
  FReloadingID := AValue;
end;

procedure TRenderArea.SetOnChage(AValue : TRenderAreaChange);
begin
  if FOnChage = AValue then
    Exit;
  if (AValue <> nil) and Assigned(AValue) then
    FOnChage := AValue
  else
    FOnChage := nil;
end;

function TRenderArea.AlwaysTrue(const Point : TVector3; const Size : double) : boolean;
begin
  Result := True;
end;

function TRenderArea.GetIsVisibledPointFunction : TIsVisibledPointFunction;
begin
  if FIsVisibledPointFunction = @AlwaysTrue then
    Result := nil
  else
    Result := FIsVisibledPointFunction;
end;

procedure TRenderArea.SetBorderWidth(AValue: Double);
begin                              
  if AValue > 0 then
    AValue:=0;
  if fBorderWidth=AValue then Exit;
  fBorderWidth:=AValue;
  World.Queues.AddMethod(@OnChangeEvent);
end;

procedure TRenderArea.SetIsVisibledPointFunction(AValue : TIsVisibledPointFunction);
begin
  if FIsVisibledPointFunction = AValue then
    Exit;
  if (AValue <> nil) and Assigned(AValue) then
    FIsVisibledPointFunction := AValue
  else
    FIsVisibledPointFunction := @AlwaysTrue;
end;

procedure TRenderArea.setX(AValue : integer);
begin
  if fx = AValue then
    Exit;
  fx := AValue;
  Inc(FReloadingID);
  World.Queues.AddMethod(@OnChangeEvent);
end;

procedure TRenderArea.setY(AValue : integer);
begin
  if fy = AValue then
    Exit;
  fy := AValue;
  World.Queues.AddMethod(@OnChangeEvent);
end;

procedure TRenderArea.setZ(AValue : integer);
begin
  if fz = AValue then
    Exit;
  fz := AValue;
  Inc(FReloadingID);
  World.Queues.AddMethod(@OnChangeEvent);
end;

procedure TRenderArea.OnChangeEvent;
begin
  if OnChage <> nil then
    OnChage(self);
end;

function TRenderArea.GetPosition : TIntVector3;
begin
  Result := IntVector3(fx, fy, fz);
end;

procedure TRenderArea.SetPosition(const Position : TIntVector3);
begin
  if (fx = Position[axisX]) and (fy = Position[axisY]) and (fz = Position[axisZ]) then
    exit;
  fx := Position[axisX];
  fy := Position[axisY];
  fz := Position[axisZ];
  Inc(FReloadingID);
  World.Queues.AddMethod(@OnChangeEvent);
end;

procedure TRenderArea.DrawBlocks;
var
  i : integer;
  v : TIntVector3;
  c : TOurChunk;
  side : TTextureMode;
begin
  try
    MakeFog((ray - 2) shl ChunkSizeLog2, (ray - 1) shl ChunkSizeLog2,
      LightLevelToColor3f(0));
    //todo:od pogody, biomu i pory dnia
    TVertexModel.BeginDraw;

    for i := 0 to GetCoordPriorityByDistanceCount - 1 do
    begin
      if GetCoordPriorityByDistanceLength(i) > Ray then
        break;

      v := GetCoordPriorityByDistance(i);
      if not IsVisibledPointFunction(Vector3((v[axisX] + fx) shl
        ChunkSizeLog2 + ChunkSize shr 1, (v[axisY] + fy) shl
        ChunkSizeLog2 + ChunkSize shr 1, (v[axisZ] + fz) shl
        ChunkSizeLog2 + ChunkSize shr 1), ChunkSize * (sqrt(3) / 2)) then
        Continue;

      c := World.GetChunk(v[axisX] + fx, v[axisY] + fy, v[axisZ] + fz);
      if (c = nil) or (not c.Loaded) then
        Continue;

      for side in DrawedSides[sign(v[axisX]), sign(v[axisY]), sign(v[axisZ])] do
        c.GetVertexModel(side).JustDraw;
    end;
  finally
    TVertexModel.EndDraw;
  end;
  World.Queues.AddMethod(@RepaintBlocks);
end;

procedure TRenderArea.RepaintBlocks;
var
  i : integer;
  c : TOurChunk;
  v : TIntVector3;
  session : QWord;
begin
  session := PreInc(fRepaintingID);
  for i := 0 to GetCoordPriorityByDistanceCount - 1 do
  begin
    if (GetCoordPriorityByDistanceLength(i) > Ray) or (session <> fRepaintingID) then
      Break;
    v := GetCoordPriorityByDistance(i);
    c := World.GetChunk(v[axisX] + fx, v[axisY] + fy, v[axisZ] + fz);
    if c = nil then
      Continue;
    c.UpdateVertexModels();
    c.UpdateModelLight();
  end;
end;

constructor TRenderArea.Create(OurWorld : TOurWorld);
begin
  FReloadingID := 0;
  fRepaintingID := 0;
  fBorderWidth:=1.2;
  fWorld := OurWorld;
  fOnchange := nil;
  FIsVisibledPointFunction := @AlwaysTrue;
  fx := 0;
  fy := 0;
  fz := 0;
  ray := 16;
end;

destructor TRenderArea.Destroy;
begin
  fRay := -1;
  Inc(FReloadingID);
  if OnChage <> nil then
    OnChage(self);
  inherited Destroy;
end;

{ TOurWorld }

procedure TOurWorld.UpdateRenderArea(Area : TRenderArea);
var
  x, y, z, i : integer;
  c : TOurChunk;
  session : QWord;
  v : TIntVector3;
begin
  session := Area.ReloadingID;
  UpdateRenderAreaLocker.Lock;
  try
    if Area.Ray > 0 then
      for i := 0 to GetCoordPriorityByDistanceCount - 1 do
      begin
        if session <> Area.ReloadingID then
          Break;
        v := GetCoordPriorityByDistance(i);
        if GetCoordPriorityByDistanceLength(i) <= Area.Ray then
          LoadChunk(v[axisX] + Area.X, v[axisY] + Area.Y, v[axisZ] +
            Area.Z).RenderAreaCollection.Add(Area)
        else
          break;
      end;

    for x := 0 to WorldSize - 1 do
      for y := 0 to WorldSize - 1 do
        for z := 0 to WorldSize - 1 do
        begin
          i := 0;
          while i < fChunks[x, y, z].Count do
          begin
            if (Area.Ray > 0) and (Area.ReloadingID xor session > 1) then
              exit;
            c := fChunks[x, y, z].Table[i];
            if ((Area.Ray <= 0) or (hypot3(c.Position - Area.GetPosition) >
              Area.Ray + Area.BorderWidth)) and c.RenderAreaCollection.RemoveItem(Area) and (c.RenderAreaCollection.GetCount = 0) then
            begin
              UnloadChunk(fChunks[x, y, z].Table[i].Position[axisX],
                fChunks[x, y, z].Table[i].Position[axisY],
                fChunks[x, y, z].Table[i].Position[axisZ]);
            end
            else
              Inc(i);
          end;
        end;

    if session <> Area.ReloadingID then
      exit;

    UpdateTableLengths;
    if Area.Ray <= 0 then
      RenderAreaSet.RemoveItem(Area);
  finally
    UpdateRenderAreaLocker.Unlock;
  end;
end;

procedure TOurWorld.RemoveOrphanChunks;
var
  x, y, z, i : integer;
begin
  for x := 0 to WorldSize - 1 do
    for y := 0 to WorldSize - 1 do
      for z := 0 to WorldSize - 1 do
        for i := fChunks[x, y, z].Count - 1 downto 0 do
          if fChunks[x, y, z].Table[i].RenderAreaCollection.GetCount = 0 then
          begin
            UnloadChunk(fChunks[x, y, z].Table[i].Position[axisX],
              fChunks[x, y, z].Table[i].Position[axisY],
              fChunks[x, y, z].Table[i].Position[axisZ]);
            RaiseException('Found orphan chunk!', False);
          end;
end;

function TOurWorld.GetChunk(const ChunkX, ChunkY, ChunkZ : integer) : TOurChunk;
var
  x, y, z, i : integer;
begin
  x := ChunkX and WorldSizeMask;
  y := ChunkY and WorldSizeMask;
  z := ChunkZ and WorldSizeMask;
  try                      
    fChunks[x, y, z].Locker.Lock;
    for i := 0 to fChunks[x, y, z].Count - 1 do
      if (fChunks[x, y, z].Table[i].Position[axisX] = ChunkX) and
        (fChunks[x, y, z].Table[i].Position[axisY] = ChunkY) and
        (fChunks[x, y, z].Table[i].Position[axisZ] = ChunkZ) then
        exit(fChunks[x, y, z].Table[i]);
  finally
    fChunks[x, y, z].Locker.Unlock;
  end;
  Result := nil;
end;

function TOurWorld.GetChunkFromBlockCoors(const x, y, z : integer) : TOurChunk;
begin
  Result := GetChunk(integer(PtrUInt(x) shr ChunkSizeLog2),
    integer(PtrUInt(y) shr ChunkSizeLog2), integer(PtrUInt(z) shr ChunkSizeLog2));
end;

function TOurWorld.GetBlock(const x, y, z : integer) : TBlock;
var
  Chunk : TOurChunk;
begin
  Chunk := GetChunkFromBlockCoors(x, y, z);
  if Chunk = nil then
    Result := nil
  else
    Result := Chunk.GetBlockDirect(x and (ChunkSize - 1), y and
      (ChunkSize - 1), z and (ChunkSize - 1));
end;

procedure TOurWorld.SetBlock(const x, y, z : integer; AValue : TBlock);
var
  Chunk : TOurChunk;
begin
  Chunk := GetChunkFromBlockCoors(x, y, z);
  if Chunk <> nil then
    Chunk.SetBlockDirect(x and ChunkSizeMask, y and ChunkSizeMask,
      z and ChunkSizeMask, AValue);
end;

function TOurWorld.GetBlockInformation(const x, y, z: integer): TBlockInformation;
var
  c : TOurChunk;
  b : TBlock;
begin
  c := GetChunkFromBlockCoors(x, y, z);
  if c = nil then
  begin
    Result.Clear;
    exit;
  end;
  b := c.GetBlockDirect(x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask);
  Result.Init(b, c, BlockCoord(x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask));
end;

procedure TOurWorld.UnloadChunk(const ChunkX, ChunkY, ChunkZ : integer);
var
  x, y, z, i : integer;
  c : TOurChunk;
begin
  x := ChunkX and WorldSizeMask;
  y := ChunkY and WorldSizeMask;
  z := ChunkZ and WorldSizeMask;

  fChunks[x, y, z].Locker.Lock;
  try
    for i := 0 to fChunks[x, y, z].Count - 1 do
      if (fChunks[x, y, z].Table[i].Position[axisX] = ChunkX) and
        (fChunks[x, y, z].Table[i].Position[axisY] = ChunkY) and
        (fChunks[x, y, z].Table[i].Position[axisZ] = ChunkZ) then
      begin
        c := fChunks[x, y, z].Table[i];
        Dec(fChunks[x, y, z].Count);
        fChunks[x, y, z].Table[i] := fChunks[x, y, z].Table[fChunks[x, y, z].Count];
        setlength(fChunks[x, y, z].Table, fChunks[x, y, z].Count);
        exit;
      end;
  finally
    fChunks[x, y, z].Locker.Unlock;
    freeThread.FreeObject(c, 1500);
  end;
end;

function TOurWorld.LoadChunk(const ChunkX, ChunkY, ChunkZ : integer) : TOurChunk;
var
  x, y, z : integer;
begin
  Result := GetChunk(ChunkX, ChunkY, ChunkZ);
  if Result <> nil then
    exit;

  x := ChunkX and WorldSizeMask;
  y := ChunkY and WorldSizeMask;
  z := ChunkZ and WorldSizeMask;

  fChunks[x, y, z].Locker.Lock;
  try
    Inc(fChunks[x, y, z].Count);
    setlength(fChunks[x, y, z].Table, fChunks[x, y, z].Count);
    fChunks[x, y, z].Table[fChunks[x, y, z].Count - 1] :=
      TOurChunk.Create(fDefaultBlock, IntVector3(ChunkX, ChunkY, ChunkZ), self);
    Result := fChunks[x, y, z].Table[fChunks[x, y, z].Count - 1];
  finally
    fChunks[x, y, z].Locker.Unlock;
  end;

  Result.Load;
end;

procedure TOurWorld.UpdateTableLengths;
var
  x, y, z : integer;
begin
  for x := 0 to WorldSize - 1 do
    for y := 0 to WorldSize - 1 do
      for z := 0 to WorldSize - 1 do
      begin
        fChunks[x, y, z].Locker.Lock;
        setlength(fChunks[x, y, z].Table, fChunks[x, y, z].Count);
        fChunks[x, y, z].Locker.Unlock;
      end;
end;

function TOurWorld.AddRenderArea(const x, y, z, ray : integer) : TRenderArea;
var
  Area : TRenderArea;
begin
  Area := TRenderArea.Create(self);
  Area.SetPosition(IntVector3(x, y, z));
  Area.ray := ray;            
  Area.OnChage := @UpdateRenderArea;
  RenderAreaSet.Add(Area);
  Result := Area;
  Queues.AddMethod(@Area.OnChangeEvent);
end;

procedure TOurWorld.Tick;
var
  x, y, z, i : integer;
  c : TOurChunk;
begin
  if (not TickLocker.TryLock) then
  begin
    Queues.AddMethodDelay(@Tick, 1);
    exit;
  end;
  Queues.AddMethodDelay(@Tick, TickDelay);
  try
    for x := 0 to WorldSize - 1 do
      for y := 0 to WorldSize - 1 do
        for z := 0 to WorldSize - 1 do
        begin
          fChunks[x, y, z].Locker.Lock;
          i := -1;
          while PreInc(i) < fChunks[x, y, z].Count do
          begin
            c := fChunks[x, y, z].Table[i];
            if c.Finishing then
              Continue;
            fChunks[x, y, z].Locker.Unlock;
            c.Tick;
            fChunks[x, y, z].Locker.Lock;
          end;
          fChunks[x, y, z].Locker.Unlock;
        end;
  finally
    TickLocker.Unlock;
  end;
end;

procedure TOurWorld.RandomTickAuto;
begin
  Queues.AddMethodDelay(@RandomTickAuto, 1000);
  RandomTick(RandomTickSpeed);
end;

procedure TOurWorld.RandomTick(const Count : integer);
var
  x, y, z, i : integer;
begin
  for x := 0 to WorldSize - 1 do
    for y := 0 to WorldSize - 1 do
      for z := 0 to WorldSize - 1 do
        for i := fChunks[x, y, z].Count - 1 downto 0 do
          fChunks[x, y, z].Table[i].RandomTick(Count);
end;

constructor TOurWorld.Create(defaultBlock: TBlockCreator; Game: TOurGame;
  WorldGenerator: TAbstractGenerator);
var
  x, y, z : integer;
begin
  TickLocker := TLocker.Create;
  UpdateRenderAreaLocker := TLocker.Create;
  fRenderAreaSet := TRenderAreaCollection.Create;
  fFreeThread := TFree.Create;    
  fOurGame := Game;
  fEnvironment := Game.GetEnvironment;
  fGenerator := WorldGenerator;
  for x := 0 to WorldSize - 1 do
    for y := 0 to WorldSize - 1 do
      for z := 0 to WorldSize - 1 do
      begin
        fChunks[x, y, z].Count := 0;
        fChunks[x, y, z].Locker := TLocker.Create;
      end;
  fQueues := TQueueManager2.Create(1);
  fDefaultBlock := defaultBlock;
  fTickDelay := 50;
  fRandomTickSpeed := 3;
  fQueues.AddMethodDelay(@Tick, fTickDelay);
  Queues.AddMethodDelay(@RandomTickAuto, 1000);
end;

destructor TOurWorld.Destroy;
var
  x, y, z : integer;
begin
  while RenderAreaSet.GetCount > 0 do
    RenderAreaSet.Get(0).Free;
  fRenderAreaSet.Free;
  RemoveOrphanChunks;

  fQueues.Clear;
  fFreeThread.Free;
  TickLocker.Free;
  fQueues.Free;
  UpdateRenderAreaLocker.Free;

  if fGenerator.FDestroyWithWorld then
     fGenerator.Free;

  for x := 0 to WorldSize - 1 do
    for y := 0 to WorldSize - 1 do
      for z := 0 to WorldSize - 1 do
        fChunks[x, y, z].Locker.Free;
  inherited Destroy;
end;

{ TOurChunk }

function TOurChunk.GetNeightbors(const Index : TTextureMode) : TOurChunk;
begin
  Result := fNeightbors[Index];
end;

procedure TOurChunk.AddLight(const x, y, z : integer; LightLevel : integer;
  const Functions : TLightFunctionKind; const maxDepth : integer; const Force : boolean);
const
  DepthResistance : array[TLightFunctionKind, TTextureMode] of integer =
    ((1, 1, 1, 1, 1, 1), (1, 1, 1, 0, 1, 1));
var
  Side : TTextureMode;
  nx, ny, nz : integer;
  c : TOurChunk;
begin
  LightLevel := max(LightLevel - MAX_BLOCK_TRANSPARENCY +
    fBlocks[x, y, z].Transparency, LightFunctions[Functions].GetLightSource(x, y, z));

  if Force then
    UpdateIfGreater(LightLevel, LightFunctions[Functions].GetLight(x, y, z))
  else
  if (maxDepth < 0) or (LightLevel <= LightFunctions[Functions].GetLight(x, y, z)) then
    exit;

  LightFunctions[Functions].SetLight(x, y, z, LightLevel);

  for side := Low(TTextureMode) to High(TTextureMode) do
  begin
    nx := x + TextureModeSidesI[side][axisX];
    ny := y + TextureModeSidesI[side][axisY];
    nz := z + TextureModeSidesI[side][axisZ];
    c := GetNeightborFromBlockCoord(nx, ny, nz);
    if c = nil then
      Continue;
    c.AddLight(nx and ChunkSizeMask, ny and ChunkSizeMask, nz and
      ChunkSizeMask, LightLevel - 1, Functions, maxDepth -
      DepthResistance[Functions, side], False);
  end;
end;

function TOurChunk.GetBlockLightSource(const x, y, z : integer) : integer;
begin
  Result := fBlocks[x, y, z].LightSource;
end;

function TOurChunk.GetSunLightSource(const x, y, z : integer) : integer;
begin
  Result := fSunLight[x, y, z];
end;

function TOurChunk.GetBlock(const x, y, z : integer) : TBlock;
var
  c : TOurChunk;
begin
  c := GetNeightborFromBlockCoord(x, y, z);
  if (c <> nil) then
    Result := c.GetBlockDirect(x and ChunkSizeMask, y and ChunkSizeMask,
      z and ChunkSizeMask)
  else
    Result := nil;
end;

procedure TOurChunk.SetBlock(const x, y, z : integer; AValue : TBlock);
var
  c : TOurChunk;
begin
  c := GetNeightborFromBlockCoord(x, y, z);
  if (c <> nil) then
    c.SetBlockDirect(x and ChunkSizeMask, y and ChunkSizeMask, z and
      ChunkSizeMask, AValue);
end;

procedure TOurChunk.Load;
begin
  if fLoaded or Finishing then
    exit;
  //todo: loading from file
  Generate;
  ChangedBlocks.Clear;
  fLoaded := True;
end;

procedure TOurChunk.Generate;
begin
  if fGenerated or Finishing then
    exit;
  if not World.Generator.ClassNameIs(TAbstractGenerator.ClassName) then
  begin
    AutoLightUpdate := False;
    World.Generator.Generate(Self);
    AutoLightUpdate := True;
  end;
  fGenerated := True;
  RelightArea(0, 0, 0, ChunkSize - 1, ChunkSize - 1, ChunkSize - 1);
end;

procedure TOurChunk.Finalize(const DelayTime : QWord);
var
  side : TTextureMode;
begin
  inherited Finalize(DelayTime);
  World.Queues.DequeueObject(self);
  for side := low(TTextureMode) to High(TTextureMode) do
    if fNeightbors[side] <> nil then
      fNeightbors[side].RemoveNeightbor(OppositeSide[side]);
end;

procedure TOurChunk.UpdateNeightborLight;
var
  c : TOurChunk;
  i, j, k : integer;
begin
  for i := -1 to 1 do
    for j := -1 to 1 do
      for k := -1 to 1 do
      begin
        c := GetNeightborFromBlockCoord(i shl ChunkSizeLog2 + 1,
          j shl ChunkSizeLog2 + 1, k shl ChunkSizeLog2 + 1);
        if c <> nil then
          World.Queues.AddMethod(@c.UpdateModelLight);
      end;
end;

function TOurChunk.SetBlockDirectAuto(const x, y, z, ID, SubID: Integer): Boolean;
var
  Creator : TElementCreator;
begin
  Creator := Environment.GetCreator(ID);
  if (Creator <> nil) and (Creator.GetType = etBlock) and (Creator is TBlockCreator) then
    SetBlockDirect(x, y, z, (Creator as TBlockCreator).CreateBlock(IntVector3(x, y, z) + Position, SubID))
    else
    Exit(False);
  Result := True;
end;

function TOurChunk.GetBlockDirect(const x, y, z: Integer): TBlock;
begin
  try
    Result := fBlocks[x, y, z];
  except
    RaiseException('Error while getting block: ' + x.ToString +
      #32 + y.toString + #32 + z.toString + ' in chunk: 0x' +
      IntToHex(QWord(Self), 16), True);
  end;
end;

procedure TOurChunk.SetBlockDirect(const x, y, z: Integer; AValue: TBlock);
var
  side : TTextureMode;
  fb : TBlock;
  c : TOurChunk;
  coord : TBlockCoord;
begin
  try
    fb := fBlocks[x, y, z];
    if AValue = nil then
      fBlocks[x, y, z] := fdefaultBlock.CreateElement(Vector3(x, y, z), 0) as TBlock
    else
      fBlocks[x, y, z] := AValue;

    RelistBlock(x, y, z);

    if fAutoLightUpdate then
      RelightArea(x, y, z, x, y, z);

    if fBlocks[x, y, z].NeedAfterPut then
      fBlocks[x, y, z].AfterPut(Self, BlockCoord(x, y, z));

    NeedModelSolidUpdate := AllTextureSides;
    for side := low(TTextureMode) to High(TTextureMode) do
    begin
      c := GetNeightborFromBlockCoord(x + TextureModeSidesI[side][axisX],
        y + TextureModeSidesI[side][axisY], z + TextureModeSidesI[side][axisZ]);
      if c <> nil then
      begin
        coord := BlockCoord((x + TextureModeSidesI[side][axisX]) and
          ChunkSizeMask, (y + TextureModeSidesI[side][axisY]) and
          ChunkSizeMask, (z + TextureModeSidesI[side][axisZ]) and ChunkSizeMask);
        if c.fBlocks[coord[axisX], coord[axisY], coord[axisZ]].NeedNearChangeUpdate then
          c.fBlocks[coord[axisX], coord[axisY], coord[axisZ]].NearChangeUpdate(c,
            OppositeSide[side], coord);
        include(c.NeedModelSolidUpdate, OppositeSide[side]);
      end;
    end;

    if fb <> nil then
    begin
      if fb.ID <> fdefaultBlock.ID then
        World.FreeThread.FreeObject(fb)
      else
        World.FreeThread.FreeObject(fb, 10);
    end;
    if Loaded then
      RegisterChangedBlock(BlockCoord(x, y, z));
  except
    RaiseException('Error while setting block: ' + x.ToString + #32 +
      y.toString + #32 + z.toString + ' in chunk: ' + IntToHex(QWord(Self), 16), True);
  end;
end;

class function TOurChunk.IsInsert(const x, y, z: Integer): boolean;
begin
  Result := (x >= 0) and (x < ChunkSize) and (y >= 0) and (y < ChunkSize) and
    (z >= 0) and (z < ChunkSize);
end;

class function TOurChunk.IsBorder(const x, y, z: Integer): boolean;
begin
  Result := IsInsert(x, y, z) and
    (not ((x > 0) and (x < ChunkSize - 1) and (y > 0) and (y < ChunkSize - 1) and
    (z > 0) and (z < ChunkSize - 1)));
end;

procedure TOurChunk.RegisterChangedBlock(const Coord: TBlockCoord);
begin
  if ChangedBlocks.GetCount < MaxDynamicBlockChanged then
     ChangedBlocks.Add(Coord);
end;

function TOurChunk.GetExtLightLevel(const x, y, z : integer) : integer;
var
  c : TOurChunk;
begin
  c := GetNeightborFromBlockCoord(x, y, z);
  if c = nil then
    exit(0);
  Result := c.GetLightLevel(x and ChunkSizeMask, y and ChunkSizeMask,
    z and ChunkSizeMask);
end;

function TOurChunk.GetExtBlockLightLevel(const x, y, z : integer) : integer;
var
  c : TOurChunk;
begin
  c := GetNeightborFromBlockCoord(x, y, z);
  if c = nil then
    exit(0);
  Result := c.GetBlockLightLevel(x and ChunkSizeMask, y and ChunkSizeMask,
    z and ChunkSizeMask);
end;

procedure TOurChunk.SetExtBlockLightLevel(const x, y, z, Value : integer);
var
  c : TOurChunk;
begin
  c := GetNeightborFromBlockCoord(x, y, z);
  if c = nil then
    exit;
  c.SetBlockLightLevel(x and ChunkSizeMask, y and ChunkSizeMask,
    z and ChunkSizeMask, Value);
  NeedModelLightUpdate := AllTextureSides;
end;

function TOurChunk.GetExtSunLightLevel(const x, y, z : integer) : integer;
var
  c : TOurChunk;
begin
  c := GetNeightborFromBlockCoord(x, y, z);
  if c = nil then
    exit(0);
  Result := c.GetSunLightLevel(x and ChunkSizeMask, y and ChunkSizeMask,
    z and ChunkSizeMask);
end;

procedure TOurChunk.SetExtSunLightLevel(const x, y, z, Value : integer);
var
  c : TOurChunk;
begin
  c := GetNeightborFromBlockCoord(x, y, z);
  if c = nil then
    exit;
  c.SetSunLightLevel(x and ChunkSizeMask, y and ChunkSizeMask, z and
    ChunkSizeMask, Value);
  NeedModelLightUpdate := AllTextureSides;
end;

function TOurChunk.GetExtBlockLightSource(const x, y, z : integer) : integer;
var
  c : TOurChunk;
begin
  c := GetNeightborFromBlockCoord(x, y, z);
  if c <> nil then
    Result := c.fBlocks[x and ChunkSizeMask, y and ChunkSizeMask,
      z and ChunkSizeMask].LightSource
  else
    Result := 0;
end;

function TOurChunk.GetExtSunLightSource(const x, y, z : integer) : integer;
var
  c : TOurChunk;
begin
  c := GetNeightborFromBlockCoord(x, y, z);
  if c <> nil then
    Result := c.fSunLight[x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask]
  else
    Result := MAX_LIGHT_LEVEL;
end;

procedure TOurChunk.NeightborLightUpdate(const x, y, z : integer);
begin
  try
    if (x = 0) and (Neightbors[tmSouth] <> nil) then
      include(Neightbors[tmSouth].NeedModelLightUpdate, tmNorth)
    else if (x = ChunkSize - 1) and (Neightbors[tmNorth] <> nil) then
      include(Neightbors[tmNorth].NeedModelLightUpdate, tmSouth);
    if (y = 0) and (Neightbors[tmDown] <> nil) then
      include(Neightbors[tmDown].NeedModelLightUpdate, tmUp)
    else if (y = ChunkSize - 1) and (Neightbors[tmUp] <> nil) then
      include(Neightbors[tmUp].NeedModelLightUpdate, tmDown);
    if (z = 0) and (Neightbors[tmWest] <> nil) then
      include(Neightbors[tmWest].NeedModelLightUpdate, tmEast)
    else if (z = ChunkSize - 1) and (Neightbors[tmEast] <> nil) then
      include(Neightbors[tmEast].NeedModelLightUpdate, tmWest);
  except
    RaiseException('Neightbor light update error', false);
  end;
end;

function TOurChunk.GetLightLevel(const x, y, z : integer) : integer;
begin
  Result := max(GetSunLightLevel(x, y, z) div 4, GetBlockLightLevel(x, y, z));
end;

function TOurChunk.GetSunLightLevel(const x, y, z : integer) : integer;
begin
  Result := (fLights[x, y, z] and $F0) shr 4;
end;

function TOurChunk.GetBlockLightLevel(const x, y, z : integer) : integer;
begin
  Result := fLights[x, y, z] and $0F;
end;

procedure TOurChunk.SetSunLightLevel(const x, y, z : integer; const AValue : integer);
begin
  fLights[x, y, z] := (fLights[x, y, z] and $0F) or ((AValue and $F) shl 4);
  NeedModelLightUpdate := AllTextureSides;
  NeightborLightUpdate(x, y, z);
end;

procedure TOurChunk.SetBlockLightLevel(const x, y, z : integer; const AValue : integer);
begin
  fLights[x, y, z] := (fLights[x, y, z] and $F0) or (AValue and $F);
  NeedModelLightUpdate := AllTextureSides;
  NeightborLightUpdate(x, y, z);
end;

procedure TOurChunk.UpdateSunLight(const x1, z1, x2, z2 : integer;
  const FromY : integer; const ToY : integer; goDown : boolean);
var
  x, y, z, UpValue, StopLoop, mv, MinY : integer;
  c : TOurChunk;
begin
  if not Generated then
    exit;
  minY := ChunkSize;
  for x := x1 to x2 do
    for z := z1 to z2 do
    begin
      if FromY = ChunkSize - 1 then
      begin
        c := Neightbors[tmUp];
        if (c <> nil) and c.Lighted then
          UpValue := c.fSunLight[x, 0, z]
        else
          upValue := MAX_LIGHT_LEVEL;
      end
      else
        UpValue := fSunLight[x, FromY + 1, z];

      StopLoop := 0;
      mv := 1;
      for y := FromY downto 0 do
      begin
        {$define NewValue := UpValue}
        NewValue := max(0, UpValue - MAX_BLOCK_TRANSPARENCY +
          fBlocks[x, y, z].Transparency);
        if (fSunLight[x, y, z] = NewValue) and (y < ToY) then
        begin
          StopLoop := y + 1;
          goDown := False;
          break;
        end;
        mv := max(NewValue, mv);
        fSunLight[x, y, z] := NewValue;
        {$undef NewValue}
      end;
      UpdateIfLesser(minY, StopLoop);
    end;

  UpdateIfLesser(MinY, FromY);
  RelightArea(x1, MinY, z1, x2, FromY, z2, lfkSun);
  fLighted := True;

  if goDown then
  begin
    c := Neightbors[tmDown];
    if c <> nil then
      c.UpdateSunLight(x1, z1, x2, z2, ChunkSize - 1, ChunkSize - 1);
  end;
end;

function TOurChunk.GetSmoothLightLevel(const v : TVector3) : double;
var
  cv : integer;
  x, y, z : integer;
  fx : function(const x, y, z : integer) : integer of object;
begin
  cv := 0;
  x := floor(v[axisX]);
  y := floor(v[axisY]);
  z := floor(v[axisZ]);

  if (x > 0) and (x < ChunkSize) and (y > 0) and (y < ChunkSize) and
    (z > 0) and (z < ChunkSize) then
    fx := @GetLightLevel
  else
    fx := @GetExtLightLevel;

  cv := max(max(max(fx(x, y, z), fx(x - 1, y, z)),
    max(fx(x, y - 1, z), fx(x, y, z - 1))),
    max(max(fx(x - 1, y - 1, z), fx(x - 1, y, z - 1)),
    max(fx(x, y - 1, z - 1), fx(x - 1, y - 1, z - 1))));

  Result := LightLevelToFloat(cv);
end;

function TOurChunk.GetSmoothLightLevel(const v : TVector3;
  const side : TTextureMode) : double;
var
  cv, i : integer;
  x, y, z : integer;
  fx : function(const x, y, z : integer) : integer of object;
begin
  cv := 0;
  x := floor(v[axisX]);
  y := floor(v[axisY]);
  z := floor(v[axisZ]);

  if (x > 0) and (x < ChunkSize) and (y > 0) and (y < ChunkSize) and
    (z > 0) and (z < ChunkSize) then
    fx := @GetLightLevel
  else
    fx := @GetExtLightLevel;

  cv := 0;
  for i := 0 to 3 do
    cv := max(cv, fx(trunc(x + TextureStandardModeCoord[side, i, axisX]) -
      1, trunc(y + TextureStandardModeCoord[side, i, axisY]) - 1,
      trunc(z + TextureStandardModeCoord[side, i, axisZ]) - 1));

  Result := LightLevelToFloat(cv);
end;

function TOurChunk.GetLightedSide(const Coord : TBlockCoord;
  const mode : TTextureMode) : TLightedSide;
var
  i : integer;
begin
  for i := 0 to 3 do
    Result[i] := GetSmoothLightLevel(TIntVector3(Coord) +
      TextureStandardModeCoord[mode][i], mode);
end;

procedure TOurChunk.UpdateModelLight; //smoothlight shadown
var
  side : TTextureMode;
  v : ^TVector3;
  cl : ^TColor3b;
  i : integer;
begin
  if fLockUpdateModelLight or (NeedModelLightUpdate = []) then
    exit;
  for side in NeedModelLightUpdate do
  begin
    fModels[side].Lock;
    exclude(NeedModelLightUpdate, side);
    v := fModels[side].VertexPtr;
    cl := fModels[side].ColorPtr;
    for i := 0 to fModels[side].Count - 1 do
    begin
      cl[i].r := SingleToByte(GetSmoothLightLevel(v[i] - Position * ChunkSize, side));
      cl[i].g := cl[i].r;
      cl[i].b := cl[i].g;
    end;
    fModels[side].Unlock;
  end;
end;

procedure TOurChunk.ForceUpdateModelLight;
begin
  NeedModelLightUpdate := AllTextureSides;
  fLockUpdateModelLight := False;
  UpdateModelLight;
end;

procedure TOurChunk.RelightArea(const x1, y1, z1, x2, y2, z2 : integer);
begin
  fLockUpdateModelLight := True;
  RelightArea(x1, y1, z1, x2, y2, z2, lfkBlock);
  UpdateSunLight(x1, z1, x2, z2, y2, y1);
  fLockUpdateModelLight := False;
  World.Queues.AddMethod(@UpdateModelLight);
end;

procedure TOurChunk.RelightArea(const x1, y1, z1, x2, y2, z2 : integer;
  const LightMode : TLightFunctionKind);
var
  buf : TThreeDimensionalSignedArrayOfBoolean;
  minX, minY, minZ, maxX, maxY, maxZ : integer;

  procedure DoIt(const Coord : TIntVector3; const LightLevel : integer);
  var
    OldLight : integer;
    side : TTextureMode;
    c : TOurChunk;
  begin
    if buf.DataByVector[Coord] or ((Coord[axisX] > x1) and (Coord[axisX] < x2) and
      (Coord[axisY] > y1) and (Coord[axisY] < y2) and (Coord[axisZ] > z1) and
      (Coord[axisZ] < z2)) then
      exit;
    c := GetNeightborFromBlockCoord(Coord[axisX], Coord[axisY], Coord[axisZ]);
    if (c = nil) then
      exit;

    UpdateIfGreater(maxX, Coord[axisX]);
    UpdateIfGreater(maxY, Coord[axisY]);
    UpdateIfGreater(maxZ, Coord[axisZ]);
    UpdateIfLesser(minX, Coord[axisX]);
    UpdateIfLesser(minY, Coord[axisY]);
    UpdateIfLesser(minZ, Coord[axisZ]);

    OldLight := c.LightFunctions[LightMode].GetLight(Coord[axisX] and
      ChunkSizeMask, Coord[axisY] and ChunkSizeMask, Coord[axisZ] and ChunkSizeMask);
    if (OldLight > LightLevel) then
      exit;

    buf.DataByVector[Coord] := True;
    c.LightFunctions[LightMode].SetLight(Coord[axisX] and ChunkSizeMask,
      Coord[axisY] and ChunkSizeMask, Coord[axisZ] and ChunkSizeMask, 0);

    if OldLight > 0 then
      for side := Low(TTextureMode) to High(TTextureMode) do
        DoIt(Coord + TextureModeSidesI[side], OldLight - 1);
  end;

var
  side : TTextureMode;
  x, y, z, nx, ny, nz, v : integer;
  c : TOurChunk;
begin
  buf := TThreeDimensionalSignedArrayOfBoolean.Create(x1, y1, z1);
  minX := x1;
  minY := y1;
  minZ := z1;
  maxX := x2;
  maxY := y2;
  maxZ := z2;

  for x := x1 to x2 do
    for y := y1 to y2 do
      for z := z1 to z2 do
        if ((x > x1) and (x < x2) and (y > y1) and (y < y2) and
          (z > z1) and (z < z2)) then
        begin
          LightFunctions[LightMode].SetExtLight(x, y, z, 0);
          buf[x, y, z] := True;
        end
        else
          DoIt(IntVector3(x, y, z), MAX_LIGHT_LEVEL + 1);

  for x := minX to maxX do
    for y := minY to maxY do
      for z := minZ to maxZ do
        if buf[x, y, z] then
        begin
          c := GetNeightborFromBlockCoord(x, y, z);
          if c = nil then
            Continue;
          v := 1;
          for side := Low(TTextureMode) to High(TTextureMode) do
          begin
            nx := x + TextureModeSidesI[side][axisX];
            ny := y + TextureModeSidesI[side][axisY];
            nz := z + TextureModeSidesI[side][axisZ];
            if buf[nx, ny, nz] then
              Continue;
            UpdateIfGreater(v, LightFunctions[LightMode].GetExtLight(nx, ny, nz));
          end;
          c.AddLight(x and ChunkSizeMask, y and ChunkSizeMask, z and
            ChunkSizeMask, v - 1, LightMode, 32, True);
        end;

  buf.Free;
end;

function TOurChunk.GetVertexModel(const Side : TTextureMode) : TVertexModel;
begin
  Result := fModels[side];
end;

procedure TOurChunk.UpdateVertexModels;
var
  side : TTextureMode;
  x, y, z : integer;
  d, b : TBlock;
  BlockFunction : function(const x, y, z : integer) : TBlock of object;
begin
  if (NeedModelSolidUpdate = []) or Finishing or (not Loaded) then
    exit;

  for side in NeedModelSolidUpdate do
  begin
    try
      fModels[side].Lock;
      exclude(NeedModelSolidUpdate, side);
      exclude(NeedModelLightUpdate, side);
      fModels[side].Clear;
      for x := 0 to ChunkSize - 1 do
        for y := 0 to ChunkSize - 1 do
          for z := 0 to ChunkSize - 1 do
          begin
            d := GetBlockDirect(x, y, z);
            if (d.NeedDraw) then
            begin
              if (x = 0) or (x = ChunkSize - 1) or (y = 0) or
                (y = ChunkSize - 1) or (z = 0) or (z = ChunkSize - 1) then
                BlockFunction := @GetBlock
              else
                BlockFunction := @GetBlockDirect;
              b := BlockFunction(x + TextureModeSidesI[side][axisX],
                y + TextureModeSidesI[side][axisY], z + TextureModeSidesI[side][axisZ]);

              if (b <> nil) and (b.Transparency > 0) then
                d.DrawModel(self, side, BlockCoord(x, y, z));
            end;
          end;
    finally
      fModels[side].Unlock;
    end;
  end;
  //UpdateModelLight;
end;

procedure TOurChunk.UpdateUnsolidModel;
var
  coord : TBlockCoord;
  i : Integer;
begin
  fUnsolidModel.Clear;
  i := 0;
  while UnSolid.GetNext(i, coord{%H-}) do
    GetBlockDirect(coord[axisX], coord[axisY], coord[axisZ]).DrawUnsolid(self, coord);
end;

procedure TOurChunk.UpdateAnimationModel;
var
  coord : TBlockCoord;
  i : Integer;
begin
  fAnimationModels.Clear;
  i := 0;
  while Animated.GetNext(i, coord{%H-}) do
    GetBlockDirect(coord[axisX], coord[axisY], coord[axisZ]).DrawAnimation(self,
      coord);
end;

procedure TOurChunk.Tick;
var
  coord : TBlockCoord; 
  i : Integer;
begin                     
  i := 0;
  while BlocksForTick.GetNext(i, coord{%H-}) do
    GetBlockDirect(coord[axisX], coord[axisY], coord[axisZ]).OnTick(self, coord);
end;

procedure TOurChunk.RandomTick(const Count : integer);
var
  coord : TBlockCoord;
  i : integer;
begin
  if (BlocksForRandomTick.Count = 0) or Finishing then
    exit;
  for i := 0 to Count - 1 do
  begin
    coord := BlocksForRandomTick.Get(random(BlocksForRandomTick.Count));
    if coord <> NilBlockCoord then
      GetBlockDirect(coord[axisX], coord[axisY], coord[axisZ]).OnTick(self, coord);
  end;
end;

procedure TOurChunk.UpdateNeightbors(Ignore : TOurChunk);
var
  side : TTextureMode;
begin
  for side := low(TTextureMode) to High(TTextureMode) do
  begin
    fNeightbors[side] := World.GetChunk(Position[axisX] +
      TextureModeSidesI[side][axisX], Position[axisY] +
      TextureModeSidesI[side][axisY], Position[axisZ] + TextureModeSidesI[side][axisZ]);
    if fNeightbors[side] = Ignore then
      fNeightbors[side] := nil;
    if fNeightbors[side] <> nil then
      fNeightbors[side].AddNeightbor(self, OppositeSide[side]);
  end;
end;

procedure TOurChunk.AddNeightbor(Who : TOurChunk; const from : TTextureMode);
begin
  if fNeightbors[from] = who then
    exit;
  fNeightbors[from] := Who;
  include(NeedModelSolidUpdate, from);
end;

procedure TOurChunk.RemoveNeightbor(const from : TTextureMode);
begin
  if fNeightbors[from] = nil then
    exit;
  fNeightbors[from] := nil;
  //include(NeedModelSolidUpdate, from); //not need?
end;

function TOurChunk.GetNeightborFromBlockCoord(const x, y, z : integer) : TOurChunk;
begin
  if IsInsert(x, y, z) then
    exit(self);

  if (x < 0) and (Neightbors[tmSouth] <> nil) then
  begin
    Result := Neightbors[tmSouth].GetNeightborFromBlockCoord(x + ChunkSize, y, z);
    if Result <> nil then
      exit;
  end;
  if (x >= ChunkSize) and (Neightbors[tmNorth] <> nil) then
  begin
    Result := Neightbors[tmNorth].GetNeightborFromBlockCoord(x - ChunkSize, y, z);
    if Result <> nil then
      exit;
  end;
  if (y < 0) and (Neightbors[tmDown] <> nil) then
  begin
    Result := Neightbors[tmDown].GetNeightborFromBlockCoord(x, y + ChunkSize, z);
    if Result <> nil then
      exit;
  end;
  if (y >= ChunkSize) and (Neightbors[tmUp] <> nil) then
  begin
    Result := Neightbors[tmUp].GetNeightborFromBlockCoord(x, y - ChunkSize, z);
    if Result <> nil then
      exit;
  end;
  if (z < 0) and (Neightbors[tmWest] <> nil) then
  begin
    Result := Neightbors[tmWest].GetNeightborFromBlockCoord(x, y, z + ChunkSize);
    if Result <> nil then
      exit;
  end;
  if (z >= ChunkSize) and (Neightbors[tmEast] <> nil) then
  begin
    Result := Neightbors[tmEast].GetNeightborFromBlockCoord(x, y, z - ChunkSize);
    if Result <> nil then
      exit;
  end;

  Result := nil;
end;

function TOurChunk.GetEnvironment: TEnvironment;
begin
  Result := World.Environment;
end;

procedure TOurChunk.RelistBlocks;
var
  x, y, z : integer;
begin
  for z := 0 to ChunkSize - 1 do
    for y := 0 to ChunkSize - 1 do
      for x := 0 to ChunkSize - 1 do
        RelistBlock(x, y, z);
end;

procedure TOurChunk.RelistBlock(const x, y, z : integer);
var
  Block : TBlock;
begin
  Block := GetBlockDirect(x, y, z);
  if Block = nil then
    exit;

  if Block.NeedTick then
    BlocksForTick.Add(BlockCoord(x, y, z))
  else
    BlocksForTick.RemoveItem(BlockCoord(x, y, z));

  if Block.NeedRandomTick then
    BlocksForRandomTick.Add(BlockCoord(x, y, z))
  else
    BlocksForRandomTick.RemoveItem(BlockCoord(x, y, z));

  if Block.IsSolid then
    UnSolid.RemoveItem(BlockCoord(x, y, z))
  else
    UnSolid.Add(BlockCoord(x, y, z));

  if Block.HasAnimation then
    Animated.Add(BlockCoord(x, y, z))
  else
    Animated.RemoveItem(BlockCoord(x, y, z));
end;

procedure TOurChunk.SaveToStream(Stream: TStream);
var
  x, y, z : Integer;
begin
  Stream.WriteByte(0);
  Stream.WriteBuffer(FPosition, SizeOf(FPosition));
  for x := 0 to ChunkSize-1 do
    for y := 0 to ChunkSize-1 do
      for z := 0 to ChunkSize-1 do
        DirectBlocks[x, y, z].LoadFromStream(Stream, Self, BlockCoord(x, y, z));
  ChangedBlocks.Clear;
end;

procedure TOurChunk.SaveChangesToStream(Stream: TStream);
var
  i : Integer;
  c : TBlockCoord;
begin
  if ChangedBlocks.GetCount > MaxDynamicBlockChanged then
     SaveToStream(Stream)
     else
     begin
       if ChangedBlocks.GetCount = 0 then
          exit;
       Stream.WriteByte(1); 
       Stream.WriteBuffer(FPosition, SizeOf(FPosition));
       i := 0;
       while ChangedBlocks.GetNext(i, c) do
       begin
          Stream.WriteBuffer(c, SizeOf(c));
          DirectBlocks[c[axisX], c[axisY], c[axisZ]].SaveToStream(Stream, Self, c);
       end;
       ChangedBlocks.Clear;
     end;
  c := NilBlockCoord;
  Stream.WriteBuffer(c, SizeOf(c));
end;

procedure TOurChunk.LoadFromStream(Stream: TStream);  
var
  x, y, z : Integer;
  mode : Integer;
  c : TBlockCoord;
begin
  mode := Stream.ReadByte;
  Stream.ReadBuffer(FPosition, SizeOf(FPosition));
  if mode = 0 then
  begin
    for x := 0 to ChunkSize-1 do
      for y := 0 to ChunkSize-1 do
        for z := 0 to ChunkSize-1 do
          DirectBlocks[x, y, z].SaveToStream(Stream, Self, BlockCoord(x, y, z));
  end
  else
  begin
    while true do
    begin
        Stream.ReadBuffer(c, SizeOf(c));
        if c = NilBlockCoord then
           break;
        DirectBlocks[c[axisX], c[axisY], c[axisZ]].LoadFromStream(Stream, Self, c);
    end;
  end;
end;

function TOurChunk.GetHashCode: PtrInt;
var
  buf : array[0..ChunkSize*ChunkSize*ChunkSize] of QWord;
  i, x, y, z : Integer;
begin
  i := 0;
  for x := 0 to ChunkSize-1 do
    for y := 0 to ChunkSize-1 do
      for z := 0 to ChunkSize-1 do
          buf[PostInc(i)] := GetBlockDirect(x, y, z).GetHashCode;
  buf[High(buf)] := inherited GetHashCode;
  Result:=ModuloBuf(@buf[0], sizeof(buf), 1);
end;

constructor TOurChunk.Create(defaultBlock : TBlockCreator;
  const MyPosition : TIntVector3; const OurWorld : TOurWorld);
var
  x, y, z : integer;
  c : TOurChunk;
  side : TTextureMode;
begin
  inherited Create;

  LightFunctions[lfkBlock].GetLight := @GetBlockLightLevel;
  LightFunctions[lfkBlock].SetLight := @SetBlockLightLevel;
  LightFunctions[lfkBlock].GetLightSource := @GetBlockLightSource;
  LightFunctions[lfkBlock].GetExtLight := @GetExtBlockLightLevel;
  LightFunctions[lfkBlock].SetExtLight := @SetExtBlockLightLevel;
  LightFunctions[lfkBlock].GetExtLightSource := @GetExtBlockLightSource;
  LightFunctions[lfkSun].GetLight := @GetSunLightLevel;
  LightFunctions[lfkSun].SetLight := @SetSunLightLevel;
  LightFunctions[lfkSun].GetLightSource := @GetSunLightSource;
  LightFunctions[lfkSun].GetExtLight := @GetExtSunLightLevel;
  LightFunctions[lfkSun].SetExtLight := @SetExtSunLightLevel;
  LightFunctions[lfkSun].GetExtLightSource := @GetExtSunLightSource;

  NeedModelLightUpdate := AllTextureSides;
  NeedModelSolidUpdate := AllTextureSides;
  fLoaded := False;
  fGenerated := False;
  fdefaultBlock := defaultBlock;
  fPosition := MyPosition;
  fWorld := OurWorld;
  fAutoLightUpdate := True;
  fLockUpdateModelLight := False;
  fLighted := False;

  BlocksForRandomTick := TBlockList.Create;
  BlocksForTick := TBlockList.Create;
  UnSolid := TBlockList.Create;
  Animated := TBlockList.Create;
  ChangedBlocks := TBlockList.Create;

  fRenderAreaCollection := TRenderAreaCollection.Create;

  for side := low(TTextureMode) to High(TTextureMode) do
    fModels[side] := TVertexModel.Create;

  for x := 0 to ChunkSize - 1 do
    for y := 0 to ChunkSize - 1 do
      for z := 0 to ChunkSize - 1 do
      begin
        fBlocks[x, y, z] := defaultBlock.CreateElement(Vector3(x, y, z), 0) as TBlock;
        fLights[x, y, z] := 0;
        fSunLight[x, y, z] := 0;
      end;

  UpdateNeightbors();

  for x := -1 to 1 do
    for y := -1 to 1 do
      for z := -1 to 1 do
      begin
        c := GetNeightborFromBlockCoord(x * ChunkSize + 1, y *
          ChunkSize + 1, z * ChunkSize + 1);
        if (c <> nil) then
        begin
          c.NeedModelLightUpdate := AllTextureSides;
          world.Queues.AddMethod(@c.UpdateModelLight);
        end;
      end;

  fUnsolidModel := TVertexModel.Create;
  fAnimationModels := TVertexModel.Create;

end;

destructor TOurChunk.Destroy;
var
  side : TTextureMode;
  x, y, z : integer;
begin
  if not Finishing then
    Finalize(0);

  World.Queues.DequeueObject(self);

  fRenderAreaCollection.Free;

  for x := 0 to ChunkSize - 1 do
    for y := 0 to ChunkSize - 1 do
      for z := 0 to ChunkSize - 1 do
        fBlocks[x, y, z].Free;

  for side := low(TTextureMode) to High(TTextureMode) do
    fModels[side].Free;
  fUnsolidModel.Free;
  fAnimationModels.Free;

  ChangedBlocks.Free;
  BlocksForRandomTick.Free;
  BlocksForTick.Free;
  UnSolid.Free;
  Animated.Free;
  inherited Destroy;
end;

{ TBlockCreator }

function TBlockCreator.GetType: TElementType;
begin
  Result := etBlock;
end;

function TBlockCreator.CreateBlock(const Coord: TIntVector3;
  const SubID: Integer): TBlock;
begin
  Result := CreateElement(Coord, SubID) as TBlock;
end;

{ TBlock }

function TBlock.GetBlockCreator: TBlockCreator;
begin
  Result := Creator as TBlockCreator;
end;

function TBlock.GetTag: PBlockDataTag;
begin
  Result := nil;
end;

function TBlock.NeedDraw : boolean;
begin
  Result := True;
end;

function TBlock.IsSolid : boolean;
begin
  Result := True;
end;

function TBlock.NeedTick : boolean;
begin
  Result := False;
end;

function TBlock.NeedRandomTick : boolean;
begin
  Result := False;
end;

function TBlock.HasAnimation : boolean;
begin
  Result := False;
end;

function TBlock.NeedAfterPut: boolean;
begin
  Result := False;
end;

function TBlock.NeedNearChangeUpdate : boolean;
begin
  Result := False;
end;

procedure TBlock.SaveToStream(Stream: TStream; Chunk: TOurChunk;
  const Coord: TBlockCoord);
begin
  Stream.WriteDWord(GetID);
  Stream.WriteDWord(GetSubID);
end;

procedure TBlock.LoadFromStream(Stream: TStream; Chunk: TOurChunk; const Coord: TBlockCoord);
var
  StreamPosition : Integer;
  ReadedID, ReadedSubID : Integer;
begin
  StreamPosition := Stream.Position;
  ReadedID := Stream.ReadDWord;
  ReadedSubID:=Stream.ReadDWord;
  if (ReadedID <> GetID) or (ReadedSubID <> GetSubID) then
  begin
     Stream.Position:=StreamPosition;
     //warning: this class could be destroyed after this line
     if not Chunk.SetBlockDirectAuto(Coord[axisX], Coord[axisY], Coord[axisZ], ReadedID, ReadedSubID) then
        RaiseException('Invalid ID in stream', True);
     Chunk.GetBlockDirect(Coord[axisX], Coord[axisY], Coord[axisZ]).LoadFromStream(Stream, Chunk, Coord);
  end;
end;

function TBlock.Transparency : integer;
begin
  Result := 0;
end;

function TBlock.LightSource : integer;
begin
  Result := 0;
end;

function TBlock.Clone(const NewCoord: TIntVector3): TBlock;
begin
  Result := Creator.CreateElement(NewCoord, getSubID) as TBlock;
end;

function TBlock.GetHashCode: PtrInt;
var
  buf : array[0..2] of QWord;
begin
  buf[0] := GetID;
  buf[1] := GetSubID;
  buf[2] := inherited GetHashCode;
  Result:= ModuloBuf(@buf[0], SizeOf(buf), 1);
end;

{ TEntity }

procedure TEntity.SetModel(AValue: TModel);
begin
  if FModel=AValue then Exit;
  FModel:=AValue;
end;

procedure TEntity.SetPosition(AValue: TVector3);
begin
  if FPosition=AValue then Exit;
  FPosition:=AValue;
end;

procedure TEntity.SetRotate(AValue: TRotationVector);
begin
  if FRotate=AValue then Exit;
  FRotate:=AValue;
end;

procedure TEntity.SetRotateVelocity(AValue: TRotationVector);
begin
  if FRotateVelocity=AValue then Exit;
  FRotateVelocity:=AValue;
end;

procedure TEntity.SetVelocity(AValue: TVector3);
begin
  if FVelocity=AValue then Exit;
  FVelocity:=AValue;
end;

procedure TEntity.UpdateChunk;
var
  x, y, z : Integer;
begin
  x := floor(FPosition[axisX]);
  y := floor(FPosition[axisY]);
  z := floor(FPosition[axisZ]);
  World.GetChunkFromBlockCoors(x, y, z);
end;

constructor TEntity.Create(TheWorld: TOurWorld; MyCreator: TElementCreator);
begin
  fWorld := TheWorld;
  inherited Create(MyCreator);
end;

{ TMovingShape }

function TMovingShape.CreateVertexModel(const LightLevel : integer) : TVertexModel;
var
  side : TTextureMode;
  c : TTexturedCuboid;
  vm : TVertexModel;
begin
  vm := TVertexModel.Create;

  c := EntityShapeToTexturedCuboid(fShape);

  for side := low(TTextureMode) to High(TTextureMode) do
    vm.AddWall(fShape.Position, c.Corners[side], c.Textures[side],
      fShape.Texture, LightLevel);
  Result := vm;
end;

procedure TMovingShape.SetDestination(const es : TEntityShape);
begin
  Base := fShape;
  Dest := es;
  LastUpdate := GetTickCount64;
end;

procedure TMovingShape.Calc(const CurrentTime : QWord);
var
  a : double;
begin
  if (CurrentTime - LastUpdate >= TimeLength) then
  begin
    fShape := Dest;
    exit;
  end;
  a := TimeLength / (CurrentTime - LastUpdate + 0.001);
  fShape.Position := Base.Position + (Dest.Position - Base.Position) * a;
  fShape.Rotate := Base.Rotate + (Dest.Rotate - Base.Rotate) * a;
  fShape.Size := Base.Size + (Dest.Size - Base.Size) * a;
  fShape.Center := Base.Center + (Dest.Size - Base.Size) * a;
  fShape.Texture := Dest.Texture;
  fShape.TextureCorners := Dest.TextureCorners;
end;

constructor TMovingShape.Create;
begin
  inherited Create;
  Base.Position := Vector3(0, 0, 0);
  Base.Rotate := Vector3(0, 0, 0);
  Base.Size := Vector3(0, 0, 0);
end;

destructor TMovingShape.Destroy;
begin
  inherited Destroy;
end;

constructor TModel.Create();
begin
  inherited Create;
  fShapeCount := 0;
  setlength(fShapes, 0);
end;

destructor TModel.Destroy;
var
  i : integer;
begin
  for i := 0 to fShapeCount - 1 do
    fShapes[i].Free;
  fShapeCount := 0;
  setlength(fShapes, 0);
  inherited Destroy;
end;

function TModel.GetShape(const index : integer) : TMovingShape;
begin
  if index < fShapeCount then
    Result := fShapes[index]
  else
    Result := nil;
end;

function TModel.CreateVertexModel(const LightLevel : integer) : TVertexModel;
var
  i : integer;
  vm : TVertexModel;
begin
  vm := TVertexModel.Create;
  for i := 0 to ShapeCount - 1 do
    vm.AddVertexModelAndFree(fShapes[i].CreateVertexModel(LightLevel));
  Result := vm;
end;

end.
