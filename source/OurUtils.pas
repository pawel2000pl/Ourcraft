unit OurUtils;

{$mode objfpc}
{$RangeChecks off}
{$ModeSwitch advancedrecords}
{$Interfaces CORBA}
{$IfNDef DEBUGBUILD} {$Optimization AUTOINLINE} {$endif}

interface

uses
  SysUtils, Classes, Math, strutils, Models, CalcUtils, Sorts, Freerer, Queues,
  Locker, ThreeDimensionalArrayOfBoolean, TinyHashData, OurGame,
  Incrementations, CustomSaver, SaverPaths, LightTypes, NearestVectors,
  TextureMode, CollisionBoxes, AsyncMicroTimer, AsyncMilliTimer, ProcessUtils,
  FastLZ77;

const
  ChunkSizeLog2 = 4;
  WorldSizeLog2 = ChunkSizeLog2 + 1;
  ChunkSize = 1 shl ChunkSizeLog2;
  WorldSize = 1 shl WorldSizeLog2; //size of HashTable
  ChunkSizeMask = ChunkSize - 1;
  WorldSizeMask = WorldSize - 1;

  MAX_BLOCK_TRANSPARENCY = MAX_LIGHT_LEVEL;
  LIGHT_STEPS_UNTIL_BLACK = 15;
  LENGTH_LIGHT_RESISTANCE = ((1+MAX_LIGHT_LEVEL) div (1+LIGHT_STEPS_UNTIL_BLACK));

  MAX_TICK_DELTA_TIME = 1000;

  MaxBlockTransparency : TLight = (MAX_BLOCK_TRANSPARENCY, MAX_BLOCK_TRANSPARENCY, MAX_BLOCK_TRANSPARENCY);

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
  TEntity = class;

  { TAbstractGenerator }

  TAbstractGenerator = class
    abstract
  private
    FDestroyWithWorld : boolean;
  public
    property DestroyWithWorld : boolean read FDestroyWithWorld write FDestroyWithWorld;
    procedure Generate(const Chunk : TOurChunk); virtual; abstract;
    constructor Create(const _DestroyWithWorld : boolean = True);
  end;

  { TEntityCreator }

  TEntityCreator = class(TElementCreator)
    public
      function GetType: TElementType; override;
      function CreateElement(const Coords: TVector3; const SubID: integer=0): TEnvironmentElement; override; deprecated;
      function CreateElement(AWorld : TOurWorld; const Coords: TVector3; const SubID: integer=0): TEnvironmentElement; virtual; abstract; overload;
  end;

  Pjmp_buf = ^jmp_buf;

  TCollisionBoxSet = specialize TTinyHashKeyMap<TIntVector3, TCollisionBox>;

  { TEntity }

  TEntity = class(TEnvironmentElement)
  private
    FChunk : TOurChunk;
    fWorld : TOurWorld;
    FRotation: TRotationVector;
    FLock : TMultiReadExclusiveWriteSynchronizer;
    FUnregistered : Boolean;

    function GetVelocity: TVector3;
    procedure SetVelocity(AValue: TVector3);
    procedure SwitchChunk(NewChunk : TOurChunk);
  protected
    StateBox : TVelocityBox;
    procedure SetWorld(AValue: TOurWorld); virtual;
    procedure SetPosition(const AValue: TVector3); virtual;
    procedure SetRotation(const AValue: TRotationVector); virtual;                
    function GetRotation: TRotationVector;
    procedure SetRotation(const AValue: TRotationVector; const UpdateBoxes : Boolean);
    function GetPosition : TVector3; virtual;
    procedure SetPosition(const AValue : TVector3; const UpdateBoxes : Boolean);
    procedure AfterMovement; virtual;
    function GetLightLevel(const Point : TVector3) : TRealLight;
    function Unregistered : Boolean; inline;
    procedure Unregister;
  public
    procedure AutoSwitchChunk;
    procedure LockForReading;
    procedure UnlockFromReading;
    procedure LockForWriting;
    procedure UnlockFromWriting;

    procedure OnLeaveChunk(AChunk : TOurChunk); virtual;
    procedure OnEnterChunk(AChunk : TOurChunk); virtual;

    property Position : TVector3 read GetPosition write SetPosition;
    property Rotation : TRotationVector read GetRotation write SetRotation;
    property Velocity : TVector3 read GetVelocity write SetVelocity;

    procedure StopMovement; virtual; ///(v=0)

    property Chunk : TOurChunk read FChunk;
    property World : TOurWorld read fWorld write SetWorld;

    procedure Tick(const DeltaTime : QWord); virtual;

    //automatic checking collision with blocks
    function CheckCollisionsWithBlock : Boolean; virtual;
    //automatic checking collisons with entities
    function CheckCollisionsWithEntity : Boolean; virtual;
    //automatic computing movement
    function AutoComputeCollisions : Boolean; virtual;

    procedure OnCollisionWithBlock(Blocks : TCollisionBoxSet); virtual;
    procedure OnCollisionWithEntity; virtual;
    procedure OnBeInBlock(Blocks : TCollisionBoxSet; const DeltaTime : Double); virtual;

    function MovementResistance : Double; virtual;

    procedure UpdateModel; virtual; abstract;
    procedure UpdateModelLight; virtual;

    function GetBlockCollisionBox(const LocalBlockPosition: TIntVector3; out CollisionBox : TCollisionBox): Boolean;
    function FindCollision(const FromTime, ToTime : Double; Blocks : TCollisionBoxSet; var Where : TVector3; const MaxDepth : Integer = 8) : Double;
    procedure ComputeMovement(const DeltaTime : Double); virtual;

    function GetID : integer; virtual;
    procedure OnTick(const {%H-}DeltaTime : QWord); virtual;
    procedure OnHit({%H-}Entity : TEntity); virtual;  //left click
    procedure OnOptions({%H-}Entity : TEntity); virtual; //right click

    procedure Render; virtual; abstract;

    constructor Create(TheWorld : TOurWorld; MyCreator : TElementCreator; const APosition : TVector3);
    constructor Create(MyCreator : TElementCreator; const APosition : TVector3);
    destructor Destroy; override;
  end;


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
    function CreateBlock(const Coord : TIntVector3; const SubID : integer) : TBlock;
  end;

  { TBlock }

  TBlock = class(TEnvironmentElement)
  private
    function GetBlockCreator : TBlockCreator;
  public
    property BlockCreator : TBlockCreator read GetBlockCreator;
    function GetTag : PBlockDataTag; virtual;

    function NeedDraw : boolean; virtual;
    function IsSolid : boolean; virtual; //not draw on every frame and regular cube
    function NeedTick : boolean; virtual;
    function NeedRandomTick : boolean; virtual;
    function HasAnimation : boolean; virtual; //draw on every frame
    function NeedAfterPut : boolean; virtual;
    function NeedNearChangeUpdate : boolean; virtual;

    function HasCollisionBox : Boolean; virtual; //False for air
    function IsFluid : Boolean; virtual;
    function Friction : Double; virtual;
    function Resilence : Double; virtual;
    function ResilenceMinVelocity : Double; virtual;
    function GetCollisionBox({%H-}Chunk : TOurChunk; const Coord : TBlockCoord) : TCollisionBox; virtual;

    procedure CreateDarkModel(var {%H-}DarkModel : TDarkModel; const {%H-}sides : TTextureDrawSides); virtual;

    ///TBlockCoord is relative in-chunk coord
    procedure DrawModel({%H-}Chunk : TOurChunk; {%H-}Side : TTextureMode; const {%H-}Coord : TBlockCoord; {%H-}VertexModel : TVertexModel); virtual;
    procedure DrawUnsolid({%H-}Chunk : TOurChunk; const {%H-}Coord : TBlockCoord; {%H-}VertexModel : TVertexModel); virtual;
    procedure DrawAnimation({%H-}Chunk : TOurChunk; const {%H-}Coord : TBlockCoord; {%H-}VertexModel : TVertexModel); virtual;
    procedure OnTick({%H-}Chunk : TOurChunk; const {%H-}Coord : TBlockCoord; const {%H-}DeltaTime : QWord); virtual;
    procedure OnRandomTick({%H-}Chunk : TOurChunk; const {%H-}Coord : TBlockCoord); virtual;
    procedure AfterPut({%H-}Chunk : TOurChunk; const {%H-}Coord : TBlockCoord); virtual;
    procedure NearChangeUpdate({%H-}Chunk : TOurChunk; const {%H-}side : TTextureMode; const {%H-}Coord : TBlockCoord); virtual;

    ///inherited first!
    procedure SaveToStream(Stream : TStream; {%H-}Chunk : TOurChunk; const {%H-}Coord : TBlockCoord); virtual;
    ///inherited first! / retunrs if loading could be continued
    function LoadFromStream(Stream : TStream; Chunk : TOurChunk; const Coord : TBlockCoord) : boolean; virtual;

    function Transparency : TLight; virtual;
    function LightSource : TLight; virtual;
    function Clone(const NewCoord : TIntVector3) : TBlock; virtual;

    function GetDataHashCode : PtrInt;
  end;

  { TExtendedBlock }

  TExtendedBlock = class(TBlock)
  private
    FCoord : TBlockCoord;
    FChunk : TOurChunk;
    function FieldsNotSet : boolean; inline;
  public
    property GetCoord : TBlockCoord read FCoord;
    property GetChunk : TOurChunk read FChunk;
    procedure SetAsChanged;
    function NeedAfterPut : boolean; override;
    procedure AfterPut(Chunk : TOurChunk; const Coord : TBlockCoord); override;
    constructor Create(MyCreator : TElementCreator);
    constructor Create(MyCreator : TElementCreator; const AChunk : TOurChunk; const MyCoord : TBlockCoord);
    constructor Create(MyCreator : TElementCreator; const World : TOurWorld; const MyAbsoluteCoord : TIntVector3);
  end;

  { TBlockList }

  TBlockList = specialize TTinyHashSet<TBlockCoord>;

  TRenderArea = class;
  TRenderAreaChange = procedure(Area : TRenderArea) of object;

  { TRenderAreaSort }

  TRenderAreaSort = class(specialize TStaticSort<TRenderArea>)
  {$if (FPC_VERSION >= 3) and (FPC_RELEASE >= 2)}
  type
      TValue = TRenderArea;
  {$ENDIF}
  public
    class function Compare(const a, b : TValue) : integer; override;
  end;

  { TRenderAreaSearcher }

  TRenderAreaSearcher = class(specialize TStaticBSearch<TRenderArea, TRenderArea>) 
  {$if (FPC_VERSION >= 3) and (FPC_RELEASE >= 2)}
  type
      TValue = TRenderArea;
      TKey = TValue;
  {$ENDIF}
  public
    class function Compare(const a : TValue; const b : TKey) : integer; override;
  end;

  TLightFunctions = record
    SetLight : procedure(const x, y, z : Integer; const Value : TLight) of object;
    GetLight : function(const x, y, z : integer) : TLight of object;
    GetLightSource : function(const x, y, z : integer) : TLight of object;
    GetExtLightSource : function(const x, y, z : integer) : TLight of object;
    SetExtLight : procedure(const x, y, z : Integer; const Value : TLight) of object;
    GetExtLight : function(const x, y, z : integer) : TLight of object;
  end;

  TLightFunctionKind = (lfkBlock, lfkSun, lfkDynamic);

  { TRenderAreaCollection }

  TRenderAreaCollection = specialize TTinyHashSet<TRenderArea>;

  TEntitySet = specialize TTinyHashSet<TEntity>;

  { EDirectBlockAccessException }

  EDirectBlockAccessException = class(Exception)
  private
     FException : Exception;
     FChunk : TOurChunk;
     FBlockCoord : TIntVector3;
  public
     property RaisedException : Exception read FException;
     property Chunk : TOurChunk read FChunk;
     property BlockCoord : TIntVector3 read FBlockCoord;
     constructor Create(const AMessage : AnsiString; AChunk : TOurChunk; const ABlockCoord : TIntVector3; AnException : Exception);
  end;

  TDynamicLightRecord = record
    Coord : TIntVector3;
    Value : TLight;
  end;

  TDynamicLightSourceMap = specialize TTinyHashKeyMap<TEntity, TDynamicLightRecord>;
  TDynamicLightCoordMap = specialize TTinyHashKeyMap<TIntVector3, TLight>;

  { TOurChunk }

  TOurChunk = class(TFreeObject)
  const
    MaxDynamicBlockChanged = ChunkSize * ChunkSize * ChunkSize div 4; //powyżej tej ilości, musi być wysłany cały chunk
  private
    fLockUpdateModelLight : TLocker;
    fAutoLightUpdate : boolean;
    fPosition : TIntVector3;  //Coord div ChunkSize
    fdefaultBlock : TBlockCreator;
    NeedModelLightUpdate : set of TTextureMode;
    NeedModelSolidUpdate : set of TTextureMode;
    fWorld : TOurWorld;

    fDynamicLightSources : TDynamicLightSourceMap;
    fDynamicLightCoords : TDynamicLightCoordMap; //it contains ABSOLUTE values
    fDynamicLightValue : array[0..ChunkSize - 1, 0..ChunkSize - 1, 0..ChunkSize - 1] of TLight;
    fBlockLightValue : array[0..ChunkSize - 1, 0..ChunkSize - 1, 0..ChunkSize - 1] of TLight;
    fSunLightValue : array[0..ChunkSize - 1, 0..ChunkSize - 1, 0..ChunkSize - 1] of TLight;
    fSunLightSource : array[0..ChunkSize - 1, 0..ChunkSize - 1, 0..ChunkSize - 1] of TLight;

    fLoaded : boolean;
    fGenerated : boolean;
    fModifiedAfterLoading : boolean;
    fLighted : boolean; //first lighting up (sun)

    fEntities : TEntitySet;

    BlocksForTick : TBlockList;
    BlocksForRandomTick : TBlockList;
    UnSolid : TBlockList;
    Animated : TBlockList;

    ChangedBlocks : TBlockList;

    fNeightbors : array[TTextureMode] of TOurChunk;
    fBlocks : array[0..ChunkSize - 1, 0..ChunkSize - 1, 0..ChunkSize - 1] of TBlock;

    fModels : array[TTextureMode] of TVertexModel;
    fUnsolidModel : TVertexModel;
    fAnimationModels : TVertexModel;

    fRenderAreaCollection : TRenderAreaCollection;
    fLocker : TMultiReadExclusiveWriteSynchronizer;

    LightFunctions : array[TLightFunctionKind] of TLightFunctions;

    procedure AddLight(const x, y, z : integer; LightLevel : TLongLight; const Functions : TLightFunctionKind;
      const maxDepth : integer = 32; const Force : boolean = False);
    function GetNeightbors(const Index : TTextureMode) : TOurChunk;
    procedure Load;
    procedure Save(const Force : Boolean = False);
    procedure Generate;
    procedure LoadEverything(Stream : TStream);
    procedure LoadChanges(Stream : TStream);

    procedure RemoveDynamicLight;
    procedure LockNeightborLightUpdate;
    procedure UnLockNeightborLightUpdate;

  protected
    procedure AddAgainNeightborsDynamicLight;
    procedure Finalize(const DelayTime : QWord); override;
    procedure RemoveNeightborsDynamicLight;

  public
    procedure UpdateEntitiesLight;
    property Entities : TEntitySet read fEntities;
    property RenderAreaCollection : TRenderAreaCollection read fRenderAreaCollection;

    procedure BeginRead;
    procedure EndRead;
    procedure BeginWrite;
    procedure EndWrite;

    property Loaded : boolean read fLoaded;
    property Generated : boolean read fGenerated;
    property Lighted : boolean read fLighted;
    property AutoLightUpdate : boolean read fAutoLightUpdate write fAutoLightUpdate;

    procedure UpdateNeightborLight;

    //local Coord                              
    function GetBlock(const x, y, z : integer) : TBlock;
    procedure SetBlock(const x, y, z : integer; AValue : TBlock);
    function SetBlockDirectAuto(const x, y, z, ID, SubID : integer) : boolean; //return false, if ID is not a block
    property Blocks[const x, y, z : integer] : TBlock read GetBlock write SetBlock;
    function GetBlockDirect(const x, y, z : integer) : TBlock;
    procedure SetBlockDirect(const x, y, z : integer; AValue : TBlock);
    property DirectBlocks[const x, y, z : integer] : TBlock read GetBlockDirect write SetBlockDirect;
    procedure RegisterChangedBlock(const Coord : TBlockCoord);  
    class function IsInsert(const x, y, z : integer) : boolean; static; inline;
    class function IsBorder(const x, y, z : integer) : boolean; static; inline;

    //must be in-chunk Coord
    function GetLightLevel(const x, y, z : integer) : TRealLight;
    function GetSunLightLevel(const x, y, z : integer) : TLight;
    function GetBlockLightLevel(const x, y, z : integer) : TLight;
    function GetDynamicLightLevel(const x, y, z : Integer) : TLight;

  private
    procedure SetSunLightLevel(const x, y, z : Integer; const Value : TLight);
    procedure SetBlockLightLevel(const x, y, z : Integer; const Value : TLight);
    procedure SetDynamicLightLevel(const x, y, z : Integer; const Value : TLight);

  public
    function GetBlockLightSource(const x, y, z : integer) : TLight;
    function GetSunLightSource(const x, y, z : integer) : TLight;
    function GetDynamicLightSource(const x, y, z : Integer) : TLight;
    //could be extercnal Coord
    function GetExtLightLevel(const x, y, z : integer) : TRealLight;
    function GetExtBlockLightLevel(const x, y, z : integer) : TLight;
    function GetExtSunLightLevel(const x, y, z : integer) : TLight;
    function GetExtDynamicLightLevel(const x, y, z : integer) : TLight;

  private
    procedure SetExtBlockLightLevel(const x, y, z : Integer; const Value : TLight);
    procedure SetExtSunLightLevel(const x, y, z : Integer; const Value : TLight);
    procedure SetExtDynamicLightLevel(const x, y, z : Integer; const Value : TLight);

  public
    function GetExtBlockLightSource(const x, y, z : integer) : TLight;
    function GetExtSunLightSource(const x, y, z : integer) : TLight;
    function GetExtDynamicLightSource(const x, y, z : integer) : TLight;

    procedure NeightborLightUpdate(const x, y, z : integer); inline;

    procedure UpdateSunLight(const x1, z1, x2, z2 : integer; const FromY : integer = ChunkSize - 1;
      const ToY : integer = 0; goDown : boolean = True);

    function GetSmoothLightLevel(const v : TVector3) : TRealLight; overload;
    function GetSmoothLightLevel(const v : TVector3; const side : TTextureMode) : TRealLight; overload;
    function GetLightedSide(const Coord : TBlockCoord; const mode : TTextureMode) : TLightedSide;
    procedure UpdateModelLight;
    procedure ForceUpdateModelLight;

    procedure RelightArea(const x1, y1, z1, x2, y2, z2 : integer); overload;
    procedure RelightArea(const x1, y1, z1, x2, y2, z2 : integer; const LightMode : TLightFunctionKind); overload;
    procedure RelightDynamicSources;
    procedure AddAgainDynamicLight;

    //need absolute coords, but they must be inside the chunk
    procedure AddDynamicLightSource(Owner : TEntity; const Values : array of TDynamicLightRecord);
    procedure MoveDynamicLightSource(Owner : TEntity; const Values : array of TDynamicLightRecord);
    procedure RemoveDynamicLightSource(Owner : TEntity);

    function GetVertexModel(const Side : TTextureMode) : TVertexModel; inline;
    property VertexModels[const side : TTextureMode] : TVertexModel read GetVertexModel;
    property UnsolidModel : TVertexModel read fUnsolidModel;
    property AnimationModel : TVertexModel read fAnimationModels;

    procedure RenderEntities;

    //TODO: externall class (GraphicsChunk)
    procedure UpdateVertexModels;
    procedure UpdateUnsolidModel;
    procedure UpdateAnimationModel;

    procedure Tick(const DeltaTime : QWord);
    procedure RandomTick(const Count : integer);

    procedure UpdateNeightbors(Ignore : TOurChunk = nil);
    procedure AddNeightbor(Who : TOurChunk; const from : TTextureMode);
    procedure RemoveNeightbor(const from : TTextureMode);
    property Neightbors[const Index : TTextureMode] : TOurChunk read GetNeightbors;
    function GetNeightborFromBlockCoord(const x, y, z : integer) : TOurChunk;
    //koordynaty względem chunku

    function GetEnvironment : TEnvironment;

    property Environment : TEnvironment read GetEnvironment;
    property LightLevel[const x, y, z : integer] : TColor3f read GetLightLevel;
    property World : TOurWorld read fWorld;
    property Position : TIntVector3 read fPosition;

    procedure RelistBlocks;
    procedure RelistBlock(const x, y, z : integer);

    procedure SaveToStream(Stream : TStream);
    procedure SaveChangesToStream(Stream : TStream);
    procedure LoadFromStream(Stream : TStream);

    function GetDataHashCode : PtrUInt;

    function GetStringCoordinatesForSaver : ansistring; overload;
    class function GetStringCoordinatesForSaver(const ChunkPosition : TIntVector3) : ansistring; static; overload;
    class function GetCoordinatesFromStringForSaver(s : ansistring) : TIntVector3; static;

    constructor Create(defaultBlock : TBlockCreator; const MyPosition : TIntVector3; const OurWorld : TOurWorld);
    destructor Destroy; override;
  end;

  TOurChunkClass = class of TOurChunk;

  TOurChunkTable = record
    Locker : TMultiReadExclusiveWriteSynchronizer;
    Table : array of TOurChunk;
    Count : integer;
  end;

  TIsVisibledPointFunction = function(const Point : TVector3; const Size : double) : boolean of object;
  TChangesInArea = procedure(const Coord : TBlockCoord; Chunk : TOurChunk) of object;

  { TRenderArea }

  TRenderArea = class
  private
    FOnChage : TRenderAreaChange;
    FOnChunkChange : TChangesInArea;
    FReloadingID : QWord;
    fRepaintingID : QWord;
    fx, fy, fz : integer;
    fRay : integer;
    fOnChange : TRenderAreaChange;
    fWorld : TOurWorld;
    fBorderWidth : double;

    FIsVisibledPointFunction : TIsVisibledPointFunction;
    function AlwaysTrue(const {%H-}Point : TVector3; const {%H-}Size : double) : boolean;
    function GetIsVisibledPointFunction : TIsVisibledPointFunction;
    procedure SetBorderWidth(AValue : double);
    procedure SetIsVisibledPointFunction(AValue : TIsVisibledPointFunction);
    procedure SetOnChage(AValue : TRenderAreaChange);
    procedure SetOnChunkChange(AValue : TChangesInArea);
    procedure SetRay(AValue : integer);
    procedure SetReloadingID(AValue : QWord);
    procedure setX(AValue : integer);
    procedure setY(AValue : integer);
    procedure setZ(AValue : integer);
    procedure OnChangeEvent;
  public
    property IsVisibledPointFunction : TIsVisibledPointFunction read GetIsVisibledPointFunction
      write SetIsVisibledPointFunction;
    property OnChunkChange : TChangesInArea read FOnChunkChange write SetOnChunkChange;
    property OnChage : TRenderAreaChange read FOnChage write SetOnChage;

    property BorderWidth : double read fBorderWidth write SetBorderWidth;
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
    function Exists : boolean;
    procedure Init(_Block : TBlock; _Chunk : TOurChunk; const _Coord : TBlockCoord);
    function AbsoluteCoord : TIntVector3;
  end;

  { TOurWorld }

  TOurWorld = class
  private
    fChunks : array[0..WorldSize - 1, 0..WorldSize - 1, 0..WorldSize - 1] of TOurChunkTable;
    PrevTickTime : QWord;

    fDefaultBlock : TBlockCreator;
    fOurGame : TOurGame;
    fEnvironment : TEnvironment;     
    fTime : Double;
    fTimeTicks : QWord;
    fLightTime : Double;

    fFreeThread : TFree;
    fQueues : TQueueManager;
    fMicroTimer : TMicroTimer;
    fMilliTimer : TMilliTimer;
    fRenderAreaSet : TRenderAreaCollection;
    FSaveAllChunks : boolean;

    fTickDelay : QWord;
    fRandomTickSpeed : QWord;
    fEntities : TEntitySet;
    fEntitiesWorkerTime : QWord;

    TickLocker : TLocker;
    UpdateRenderAreaLocker : TLocker;
    fGenerator : TAbstractGenerator;

    fSaver : TCustomSaver;

    procedure LoadFromSaver({%H-}Saver : TCustomSaver; const Path : array of ansistring; Stream : TStream);
    procedure UpdateRenderArea(Area : TRenderArea);
    procedure AddTime;
    procedure UpdateLightModel;
  public
    procedure CheckOrphanedEntities;
    class function GetLastCreatedWorld : TOurWorld;

    property SaveAllChunks : boolean read FSaveAllChunks write FSaveAllChunks;
    property Saver : TCustomSaver read fSaver;
    property RenderAreaSet : TRenderAreaCollection read fRenderAreaSet;
    property Environment : TEnvironment read fEnvironment;
    property TickDelay : Qword read fTickDelay write fTickDelay;
    property RandomTickSpeed : Qword read fRandomTickSpeed write fRandomTickSpeed;
    property OurGame : TOurGame read fOurGame;
    property Queues : TQueueManager read fQueues;
    property MicroTimer : TMicroTimer read fMicroTimer;
    property MilliTimer : TMilliTimer read fMilliTimer;
    property FreeThread : TFree read fFreeThread;
    property Time : Double read fTime;
    property LightTime : Double read fLightTime;
    property Entities : TEntitySet read fEntities;

    function Remote : Boolean;

    function GetChunk(const ChunkX, ChunkY, ChunkZ : integer) : TOurChunk;
    function GetChunkFromBlockCoors(const x, y, z : integer) : TOurChunk;
    function GetBlock(const x, y, z : integer) : TBlock;
    procedure SetBlock(const x, y, z : integer; AValue : TBlock);
    function GetBlockInformation(const x, y, z : integer) : TBlockInformation;

    procedure UnloadChunk(const ChunkX, ChunkY, ChunkZ : integer);
    function LoadChunk(const ChunkX, ChunkY, ChunkZ : integer) : TOurChunk;
    procedure UpdateTableLengths;
    procedure RemoveOrphanChunks;

    ///Automatically create single-player area
    function AddRenderArea(const x, y, z, ray : integer) : TRenderArea; overload;
    ///add a custom area
    procedure AddRenderArea(const Area : TRenderArea); overload;

    procedure Tick;
    procedure RandomTickAuto;
    procedure RandomTick(const Count : integer);
    procedure EntitiesWorker;

    property Generator : TAbstractGenerator read fGenerator;

    constructor Create(defaultBlock : TBlockCreator; Game : TOurGame; WorldGenerator : TAbstractGenerator; ASaver : TCustomSaver);
    destructor Destroy; override;

  end;

function EntityShapeToTexturedCuboid(const EntityShape : TEntityShape) : TTexturedCuboid;
function RealCoord(const ChunkPosition : TIntVector3; const BlockPosition : TBlockCoord) : TVector3;
function ChunkCoordToBlockCoord(const ChunkPosition : TIntVector3) : TIntVector3; inline;
                                                                    
function CycledShr(const i : Integer; const value : PtrUInt) : Integer; inline;
procedure FreeObject(const &Object : TObject); //for collections

implementation

{$RangeChecks off}
{$macro on}

const
  NilBlockCoord : TBlockCoord = (255, 255, 255);

var
  FLastCreatedWorld : TOurWorld = nil; //something like default

procedure FreeObject(const &Object : TObject);
begin
  &Object.Free;
end;

function CycledShr(const i : Integer; const value : PtrUInt) : Integer; inline;
begin
     if i < 0 then
        Exit(-1-((-1-i) shr value));
     Exit(i shr value);
end;

function RealCoord(const ChunkPosition : TIntVector3; const BlockPosition : TBlockCoord) : TVector3;
begin
  Result[axisX] := ChunkPosition[axisX] shl ChunkSizeLog2 + BlockPosition[axisX];
  Result[axisY] := ChunkPosition[axisY] shl ChunkSizeLog2 + BlockPosition[axisY];
  Result[axisZ] := ChunkPosition[axisZ] shl ChunkSizeLog2 + BlockPosition[axisZ];
end;

function ChunkCoordToBlockCoord(const ChunkPosition: TIntVector3): TIntVector3; inline;
begin
  Result[axisX] := ChunkPosition[axisX] shl ChunkSizeLog2;
  Result[axisY] := ChunkPosition[axisY] shl ChunkSizeLog2;
  Result[axisZ] := ChunkPosition[axisZ] shl ChunkSizeLog2;
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
  for side := Low(TTextureMode) to High(TTextureMode) do
    for i := 0 to 3 do
    begin
      for a := Low(TAxis) to High(TAxis) do
        v[a] := TextureStandardModeCoord[side][i][a] * EntityShape.Size[a] - EntityShape.Center[a];
      Result.Corners[side][i] := c * v;
    end;
end;

{ EDirectBlockAccessException }

constructor EDirectBlockAccessException.Create(const AMessage: AnsiString;
  AChunk: TOurChunk; const ABlockCoord: TIntVector3; AnException: Exception);
begin
  FChunk := AChunk;
  FBlockCoord := ABlockCoord;
  FException := AnException;
  inherited Create(AMessage + ': ' + FBlockCoord.X.ToString + #32 + FBlockCoord.Y.toString + #32 +
      FBlockCoord.Z.toString + ' in chunk: 0x' + IntToHex(QWord(FChunk), 16) + ' : ' +
      RaisedException.Message);
end;

{ TEntityCreator }

function TEntityCreator.GetType: TElementType;
begin
  Exit(etEntity);
end;

function TEntityCreator.CreateElement(const Coords: TVector3; const SubID: integer): TEnvironmentElement;
begin
     Exit(CreateElement(nil, Coords, SubID));
end;

{ TExtendedBlock }

function TExtendedBlock.FieldsNotSet : boolean;
begin
  Result := (FChunk = nil) or (FCoord = NilBlockCoord);
end;

procedure TExtendedBlock.SetAsChanged;
begin
  GetChunk.RegisterChangedBlock(GetCoord);
end;

function TExtendedBlock.NeedAfterPut : boolean;
begin
  Result := True;
end;

procedure TExtendedBlock.AfterPut(Chunk : TOurChunk; const Coord : TBlockCoord);
begin
  if FieldsNotSet then
  begin
    Chunk := Chunk;
    FCoord := Coord;
  end;
end;

constructor TExtendedBlock.Create(MyCreator : TElementCreator);
begin
  FChunk := nil;
  FCoord := NilBlockCoord;
  inherited Create(MyCreator);
end;

constructor TExtendedBlock.Create(MyCreator : TElementCreator; const AChunk : TOurChunk; const MyCoord : TBlockCoord);
begin
  FChunk := AChunk;
  FCoord := MyCoord;
  inherited Create(MyCreator);
end;

constructor TExtendedBlock.Create(MyCreator : TElementCreator; const World : TOurWorld; const MyAbsoluteCoord : TIntVector3);
begin
  FChunk := World.GetChunkFromBlockCoors(MyAbsoluteCoord[axisX], MyAbsoluteCoord[axisY], MyAbsoluteCoord[axisZ]);
  FCoord := BlockCoord(MyAbsoluteCoord[axisX] and ChunkSizeMask, MyAbsoluteCoord[axisY] and
    ChunkSizeMask, MyAbsoluteCoord[axisZ] and ChunkSizeMask);
  inherited Create(MyCreator);
end;

{ TBlockInformation }

procedure TBlockInformation.Clear;
begin
  Block := nil;
  Chunk := nil;
  Coord := NilBlockCoord;
end;

function TBlockInformation.Exists : boolean;
begin
  Result := (Chunk <> nil) and (Block <> nil);
end;

procedure TBlockInformation.Init(_Block : TBlock; _Chunk : TOurChunk; const _Coord : TBlockCoord);
begin
  Block := _Block;
  Chunk := _Chunk;
  Coord := _Coord;
end;

function TBlockInformation.AbsoluteCoord : TIntVector3;
begin
  if Exists then
    Result := Chunk.Position * ChunkSize + Coord
  else
    Result := IntVector3(0, 0, 0);
end;

{ TBlockList }
                           {
procedure TBlockList.Add(Item : TItem);
begin
  Locker.Lock;
  inherited Add(Item);
  Locker.Unlock;
end;

procedure TBlockList.Remove(const i : integer);
begin
  Locker.Lock;
  inherited Remove(i);
  Locker.Unlock;
end;
function TBlockList.GetNext(var i : integer; var Item : TItem) : boolean;
begin
  Locker.Lock;
  Result := inherited GetNext(i, Item);
  Locker.Unlock;
end;

function TBlockList.IndexOf(const Item : TItem) : integer;
begin
  Locker.Lock;
  Result := inherited IndexOf(Item);
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
end;              }


{ TAbstractGenerator }

constructor TAbstractGenerator.Create(const _DestroyWithWorld : boolean);
begin
  FDestroyWithWorld := _DestroyWithWorld;
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

procedure TRenderArea.SetOnChunkChange(AValue : TChangesInArea);
begin
  if FOnChunkChange = AValue then
    Exit;
  if not Assigned(AValue) then
    AValue := nil;
  FOnChunkChange := AValue;
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

procedure TRenderArea.SetBorderWidth(AValue : double);
begin
  if AValue > 0 then
    AValue := 0;
  if fBorderWidth = AValue then
    Exit;
  fBorderWidth := AValue;
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
    MakeFog((ray - 2) shl ChunkSizeLog2, (ray - 1) shl ChunkSizeLog2, LightLevelToColor3f(0));
    //todo:od pogody, biomu i pory dnia
    TVertexModel.BeginDraw;

    for i := 0 to GetCoordPriorityByDistanceCount - 1 do
    begin
      if GetCoordPriorityByDistanceLength(i) > Ray then
        break;

      v := GetCoordPriorityByDistance(i);
      if not IsVisibledPointFunction(
        Vector3((v[axisX] + fx) shl ChunkSizeLog2 + ChunkSize shr 1,
        (v[axisY] + fy) shl ChunkSizeLog2 + ChunkSize shr 1,
        (v[axisZ] + fz) shl ChunkSizeLog2 + ChunkSize shr 1),
        ChunkSize * (sqrt(3) / 2)) then
         Continue;

      c := World.GetChunk(v[axisX] + fx, v[axisY] + fy, v[axisZ] + fz);
      if (c = nil) or (not c.Loaded) then
        Continue;
      for side in DrawedSides[sign(v[axisX]), sign(v[axisY]), sign(v[axisZ])] do
        c.GetVertexModel(side).JustDraw;

      c.RenderEntities;
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
  fBorderWidth := 1.2;
  fWorld := OurWorld;
  fOnchange := nil;
  FOnChunkChange := nil;
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

procedure TOurWorld.LoadFromSaver(Saver : TCustomSaver; const Path : array of ansistring; Stream : TStream);
var
  v : TIntVector3;
  c : TOurChunk;
begin
  if Path[0] = SaverPaths.ChunkPath then
  begin
    v := TOurChunk.GetCoordinatesFromStringForSaver(Path[1]);
    c := GetChunk(v.x, v.y, v.z);
    if c <> nil then
      c.LoadFromStream(Stream);
  end;
end;

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
          LoadChunk(v[axisX] + Area.X, v[axisY] + Area.Y, v[axisZ] + Area.Z).RenderAreaCollection.UpdateFirst(Area)
        else
          break;
      end;

    for x := 0 to WorldSize - 1 do
      for y := 0 to WorldSize - 1 do
        for z := 0 to WorldSize - 1 do
        begin
          i := 0;
          fChunks[x, y, z].Locker.BeginWrite;
          try
            while i < fChunks[x, y, z].Count do
            begin
              if (Area.Ray > 0) and (Area.ReloadingID xor session > 1) then
                exit;
              c := fChunks[x, y, z].Table[i];
              if ((Area.Ray <= 0) or (hypot3(c.Position - Area.GetPosition) > Area.Ray + Area.BorderWidth)) and
                c.RenderAreaCollection.RemoveFirstKey(Area) and (c.RenderAreaCollection.Count = 0) then
              begin
                UnloadChunk(fChunks[x, y, z].Table[i].Position[axisX],
                  fChunks[x, y, z].Table[i].Position[axisY],
                  fChunks[x, y, z].Table[i].Position[axisZ]);
              end
              else
                Inc(i);
            end;
          finally
            fChunks[x, y, z].Locker.EndWrite;
          end;
        end;

    if session <> Area.ReloadingID then
      exit;

    UpdateTableLengths;
    if Area.Ray <= 0 then
      RenderAreaSet.RemoveFirstKey(Area);
  finally
    UpdateRenderAreaLocker.Unlock;
  end;
end;

procedure TOurWorld.AddTime;
const
  Freq = 1;
  TicksPerDay = 1200;
var
  k : Double;
begin
  Inc(fTimeTicks);
  fTime := fTimeTicks/(Freq*TicksPerDay);
  fMilliTimer.AddMethod(@AddTime, 1000 div Freq);
  k := -cos(2*pi*fTime);
  fLightTime := ifThen(k>0, k, 0);
  Queues.AddMethod(@UpdateLightModel);
end;

procedure TOurWorld.UpdateLightModel;
var
  x, y, z, i : integer;
begin
  for x := 0 to WorldSize - 1 do
    for y := 0 to WorldSize - 1 do
      for z := 0 to WorldSize - 1 do
      begin
        fChunks[x, y, z].Locker.BeginRead;
        try
            for i := fChunks[x, y, z].Count - 1 downto 0 do
              if fChunks[x, y, z].Table[i] <> nil then
                fChunks[x, y, z].Table[i].ForceUpdateModelLight;
        finally
          fChunks[x, y, z].Locker.EndRead;
        end;
      end;
end;

procedure TOurWorld.CheckOrphanedEntities;
var
  e : TEntity;
begin
  Entities.BeginRead;
  try
    for e in Entities.GetValueEnumerator do
        if e.Chunk = nil then
           e.AutoSwitchChunk;
  finally
    Entities.EndRead;
  end;
end;

class function TOurWorld.GetLastCreatedWorld: TOurWorld;
begin
  Exit(FLastCreatedWorld);
end;

function TOurWorld.Remote: Boolean;
begin
  Result := Environment.Remote;
end;

procedure TOurWorld.RemoveOrphanChunks;
var
  x, y, z, i : integer;
begin
  for x := 0 to WorldSize - 1 do
    for y := 0 to WorldSize - 1 do
      for z := 0 to WorldSize - 1 do
        for i := fChunks[x, y, z].Count - 1 downto 0 do
          if fChunks[x, y, z].Table[i].RenderAreaCollection.Count = 0 then
          begin
            UnloadChunk(fChunks[x, y, z].Table[i].Position[axisX],
              fChunks[x, y, z].Table[i].Position[axisY],
              fChunks[x, y, z].Table[i].Position[axisZ]);
            WriteLn('Found orphan chunk!');
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
    fChunks[x, y, z].Locker.Beginread;
    for i := 0 to fChunks[x, y, z].Count - 1 do
      if (fChunks[x, y, z].Table[i].Position[axisX] = ChunkX) and
        (fChunks[x, y, z].Table[i].Position[axisY] = ChunkY) and
        (fChunks[x, y, z].Table[i].Position[axisZ] = ChunkZ) then
        exit(fChunks[x, y, z].Table[i]);
  finally
    fChunks[x, y, z].Locker.Endread;
  end;
  Result := nil;
end;

function TOurWorld.GetChunkFromBlockCoors(const x, y, z : integer) : TOurChunk;
begin
  Result := GetChunk(CycledShr(x, ChunkSizeLog2), CycledShr(y, ChunkSizeLog2), CycledShr(z, ChunkSizeLog2));
end;

function TOurWorld.GetBlock(const x, y, z : integer) : TBlock;
var
  Chunk : TOurChunk;
begin
  Chunk := GetChunkFromBlockCoors(x, y, z);
  if Chunk = nil then
    Result := nil
  else
    Result := Chunk.GetBlockDirect(x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask);
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

function TOurWorld.GetBlockInformation(const x, y, z : integer) : TBlockInformation;
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

  fChunks[x, y, z].Locker.Beginwrite;
  try
    for i := 0 to fChunks[x, y, z].Count - 1 do
      if (fChunks[x, y, z].Table[i].Position[axisX] = ChunkX) and
        (fChunks[x, y, z].Table[i].Position[axisY] = ChunkY) and
        (fChunks[x, y, z].Table[i].Position[axisZ] = ChunkZ) then
      begin
        c := fChunks[x, y, z].Table[i];
        Dec(fChunks[x, y, z].Count);
        fChunks[x, y, z].Table[i] := fChunks[x, y, z].Table[fChunks[x, y, z].Count];
        SetLength(fChunks[x, y, z].Table, fChunks[x, y, z].Count);
        Break;
      end;
  finally
    fChunks[x, y, z].Locker.Endwrite;
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

  fChunks[x, y, z].Locker.Beginwrite;
  try
    Inc(fChunks[x, y, z].Count);
    SetLength(fChunks[x, y, z].Table, fChunks[x, y, z].Count);
    fChunks[x, y, z].Table[fChunks[x, y, z].Count - 1] :=
      TOurChunk.Create(fDefaultBlock, IntVector3(ChunkX, ChunkY, ChunkZ), self);
    Result := fChunks[x, y, z].Table[fChunks[x, y, z].Count - 1];
  finally
    fChunks[x, y, z].Locker.Endwrite;
  end;

  Queues.AddOrExecuteIfOveloaded(@Result.Load);
end;

procedure TOurWorld.UpdateTableLengths;
var
  x, y, z : integer;
begin
  for x := 0 to WorldSize - 1 do
    for y := 0 to WorldSize - 1 do
      for z := 0 to WorldSize - 1 do
      begin
        fChunks[x, y, z].Locker.Beginwrite;
        try
          SetLength(fChunks[x, y, z].Table, fChunks[x, y, z].Count);
        finally
          fChunks[x, y, z].Locker.Endwrite;
        end;
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
  Result := Area;
  AddRenderArea(Area);
end;

procedure TOurWorld.AddRenderArea(const Area: TRenderArea);
begin
  RenderAreaSet.UpdateFirst(Area);
  Queues.AddMethod(@Area.OnChangeEvent);
end;

procedure TOurWorld.Tick;
var
  x, y, z, i : integer;
  c : TOurChunk;
  t : QWord;
begin
  if (not TickLocker.TryLock) then
  begin
    MilliTimer.AddMethod(@Tick, 1);
    exit;
  end;
  MilliTimer.AddMethod(@Tick, TickDelay);

  t := PrevTickTime;
  PrevTickTime := GetTickCount64;
  t := min(MAX_TICK_DELTA_TIME, PrevTickTime-t);
  //writeln(t);

  try
    for x := 0 to WorldSize - 1 do
      for y := 0 to WorldSize - 1 do
        for z := 0 to WorldSize - 1 do
        begin
          fChunks[x, y, z].Locker.Beginread;
          i := -1;
          while PreInc(i) < fChunks[x, y, z].Count do
          begin
            c := fChunks[x, y, z].Table[i];
            if c.Finishing then
              Continue;
            fChunks[x, y, z].Locker.Endread;
            c.Tick(t);
            fChunks[x, y, z].Locker.Beginread;
          end;
          fChunks[x, y, z].Locker.Endread;
        end;                  
    CheckOrphanedEntities;
  finally
    TickLocker.Unlock;
  end;
end;

procedure TOurWorld.RandomTickAuto;
begin
  MilliTimer.AddMethod(@RandomTickAuto, 1000);
  RandomTick(RandomTickSpeed);
end;

procedure TOurWorld.RandomTick(const Count : integer);
var
  x, y, z, i : integer;
  c : TOurChunk;
begin
  for x := 0 to WorldSize - 1 do
    for y := 0 to WorldSize - 1 do
      for z := 0 to WorldSize - 1 do
      begin
        fChunks[x, y, z].Locker.BeginRead;
        i := 0;
        while i < fChunks[x, y, z].Count do
        begin
          c := fChunks[x, y, z].Table[i];
          if (c <> nil) and (not c.Finishing) then
          begin
            fChunks[x, y, z].Locker.EndRead;
            c.RandomTick(Count);    
            fChunks[x, y, z].Locker.BeginRead;
          end;
          Inc(i);
        end;   
        fChunks[x, y, z].Locker.EndRead;
      end;
end;

procedure TOurWorld.EntitiesWorker;
var
  Entity : TEntity;
  CurrentTime : QWord;
  DeltaTime : Double;
begin
  try
    CurrentTime:=GetMicroseconds;
    DeltaTime := min(1, (CurrentTime - fEntitiesWorkerTime)/1000000);
    fEntitiesWorkerTime := CurrentTime;
    Entities.BeginRead;
    try
      for Entity in Entities.GetValueEnumerator do
          Entity.ComputeMovement(DeltaTime);
    finally
      Entities.EndRead;
    end;
  finally
    MicroTimer.AddMethod(@EntitiesWorker, 8000);
  end;
end;

constructor TOurWorld.Create(defaultBlock : TBlockCreator; Game : TOurGame; WorldGenerator : TAbstractGenerator;
  ASaver : TCustomSaver);
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
  fSaver := ASaver;
  fSaver.OnLoad := @LoadFromSaver;
  fEntities := TEntitySet.Create;
  fTime := 0;
  fTimeTicks := 0;
  fLightTime := 0;
  for x := 0 to WorldSize - 1 do
    for y := 0 to WorldSize - 1 do
      for z := 0 to WorldSize - 1 do
      begin
        fChunks[x, y, z].Count := 0;
        fChunks[x, y, z].Locker := TMultiReadExclusiveWriteSynchronizer.Create;
      end;
  fQueues := TQueueManager.Create(1, 3);
  fMicroTimer := TMicroTimer.Create(fQueues);
  fMilliTimer := TMilliTimer.Create(fQueues);
  fDefaultBlock := defaultBlock;
  fTickDelay := 50;             ///////////////////////////////////////////////////////
  fRandomTickSpeed := 3;
  FSaveAllChunks := False;                
  MilliTimer.AddMethod(@Tick, fTickDelay);
  MilliTimer.AddMethod(@RandomTickAuto, 1000);
  MilliTimer.AddMethod(@AddTime, 10);     
  MilliTimer.AddMethod(@EntitiesWorker, 10);
  FLastCreatedWorld := Self;
end;

destructor TOurWorld.Destroy;
var
  x, y, z : integer;
begin
  while RenderAreaSet.Count > 0 do
    RenderAreaSet.GetFirstValue.Free;
  fRenderAreaSet.Free;

  RemoveOrphanChunks;

  while Entities.Count > 0 do
    Entities.GetFirstValue.Free;
  fEntities.BeginWrite;
  fEntities.EndWrite;
  fEntities.Free;
                               
  fMicroTimer.Free;
  fMilliTimer.Free;
  fQueues.Clear;
  fFreeThread.Free;
  TickLocker.Free;
  fQueues.Free;
  UpdateRenderAreaLocker.Free;

  if fGenerator.DestroyWithWorld then
    fGenerator.Free;
  if fSaver.DestroyWithOwner then
    fSaver.Free;

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

procedure TOurChunk.AddLight(const x, y, z: integer; LightLevel: TLongLight;
  const Functions: TLightFunctionKind; const maxDepth: integer;
  const Force: boolean);
const
  DepthResistance : array[TLightFunctionKind, TTextureMode] of integer =
    ((1, 1, 1, 1, 1, 1), (1, 1, 1, 0, 1, 1), (1, 1, 1, 1, 1, 1));
var
  Side : TTextureMode;
  nx, ny, nz : integer;
  OldLight : TLongLight;
  c : TOurChunk;
begin
  LightLevel := max(LightLevel - (MaxBlockTransparency - fBlocks[x, y, z].Transparency),
    LightFunctions[Functions].GetLightSource(x, y, z));

  OldLight := LightFunctions[Functions].GetLight(x, y, z);

  if Force then
    UpdateIfGreater(LightLevel, OldLight)
  else
    if (maxDepth < 0) or (OldLight >= LightLevel) then
      exit;

  LightFunctions[Functions].SetLight(x, y, z, max(OldLight, LightLevel));

  LightLevel := LightLevel-LENGTH_LIGHT_RESISTANCE;
  if (maxDepth > 0) and (LightLevel.Value > 0) then
    for side := Low(TTextureMode) to High(TTextureMode) do
    begin
      nx := x + TextureModeSidesI[side][axisX];
      ny := y + TextureModeSidesI[side][axisY];
      nz := z + TextureModeSidesI[side][axisZ];
      c := GetNeightborFromBlockCoord(nx, ny, nz);
      if c = nil then
        Continue;
      c.AddLight(nx and ChunkSizeMask, ny and ChunkSizeMask, nz and ChunkSizeMask, LightLevel, Functions, maxDepth - DepthResistance[Functions, side], False);
    end;
end;

function TOurChunk.GetStringCoordinatesForSaver : ansistring;
begin
  Result := GetStringCoordinatesForSaver(Position);
end;

class function TOurChunk.GetStringCoordinatesForSaver(const ChunkPosition : TIntVector3) : ansistring;
begin
  Result := IntToStr(ChunkPosition[AxisX]) + '.' + IntToStr(ChunkPosition[AxisY]) + '.' + IntToStr(ChunkPosition[AxisZ]) + '.chunk';
end;

class function TOurChunk.GetCoordinatesFromStringForSaver(s : ansistring) : TIntVector3;
begin
  Result[AxisX] := StrToIntDef(Copy2SymbDel(s, '.'), MaxInt);
  Result[AxisY] := StrToIntDef(Copy2SymbDel(s, '.'), MaxInt);
  Result[AxisZ] := StrToIntDef(Copy2SymbDel(s, '.'), MaxInt);
end;

function TOurChunk.GetBlockLightSource(const x, y, z: integer): TLight;
begin
  Result := fBlocks[x, y, z].LightSource;
end;

function TOurChunk.GetSunLightSource(const x, y, z: integer): TLight;
begin
  Result := fSunLightSource[x, y, z];
end;

function TOurChunk.GetDynamicLightSource(const x, y, z: Integer): TLight;
var
  Values : array of TLight;
  Value : TLight;
begin                            
  Result := AsLightZero;
  Values := [];
  fDynamicLightCoords.BeginRead;
  try
    fDynamicLightCoords.FindAll(IntVector3(x, y, z) + Position * ChunkSize, Values);  
  finally
    fDynamicLightCoords.EndRead;
  end;
  for Value in Values do
      UpdateIfGreater(Result, Value);
end;

function TOurChunk.GetBlock(const x, y, z : integer) : TBlock;
var
  c : TOurChunk;
begin
  c := GetNeightborFromBlockCoord(x, y, z);
  if (c <> nil) then
    Result := c.GetBlockDirect(x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask)
  else
    Result := nil;
end;

procedure TOurChunk.SetBlock(const x, y, z : integer; AValue : TBlock);
var
  c : TOurChunk;
begin
  c := GetNeightborFromBlockCoord(x, y, z);
  if (c <> nil) then
    c.SetBlockDirect(x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask, AValue);
end;

procedure TOurChunk.Load;
var
  s : array[0..1] of ansistring;
begin
  if fLoaded or Finishing then
    exit;
  s[0] := SaverPaths.ChunkPath;
  s[1] := GetStringCoordinatesForSaver;
  if World.Saver.Exists(s) then
  begin                                  
    fGenerated:=True;
    World.Saver.Load(s, @LoadFromStream);
  end
  else
  begin
    Generate;  
    fLoaded := True;
    if World.SaveAllChunks then
       Save(True);
  end;
  RelightArea(0, 0, 0, ChunkSize - 1, ChunkSize - 1, ChunkSize - 1);
  ChangedBlocks.Clear;
  fModifiedAfterLoading := False;
  s[0] := EmptyStr;
  s[1] := EmptyStr;
end;

procedure TOurChunk.Save(const Force: Boolean);
begin
  if not World.Remote then
  begin
    if fModifiedAfterLoading or (Force and Loaded and Generated) then
      World.Saver.Save([SaverPaths.ChunkPath, GetStringCoordinatesForSaver], @SaveToStream);
  end;
  fModifiedAfterLoading := False;
end;

procedure TOurChunk.Generate;
begin
  if fGenerated or Finishing then
    exit;
  AutoLightUpdate := False;
  World.Generator.Generate(Self);
  AutoLightUpdate := True;
  fGenerated := True;
end;

procedure TOurChunk.Finalize(const DelayTime : QWord);
var
  side : TTextureMode;
  Entity : TEntity;
begin
  inherited Finalize(DelayTime);
  World.Queues.DequeueObject(self);
  for side := Low(TTextureMode) to High(TTextureMode) do
    if fNeightbors[side] <> nil then
      fNeightbors[side].RemoveNeightbor(OppositeSide[side]);

  Entities.BeginRead;
  try
    for Entity in Entities.GetValueEnumerator do
        Entity.SwitchChunk(nil);
  finally
    Entities.EndRead;
  end;
  Save;
end;

procedure TOurChunk.RemoveNeightborsDynamicLight;  
var
  side : TTextureMode;
  c : TOurChunk;
begin
  for side := low(TTextureMode) to High(TTextureMode) do
  begin
      c := Neightbors[side];
      if c = nil then
        Continue;
      c.RemoveDynamicLight;
  end;
end;

procedure TOurChunk.UpdateEntitiesLight;
var
  e : TEntity;
begin
  if Entities.Count = 0 then
     Exit;
  Entities.BeginRead;
  try
    for e in Entities.GetValueEnumerator do
        e.UpdateModelLight;
  finally
    Entities.EndRead;
  end;
end;

procedure TOurChunk.BeginRead;
begin
  fLocker.BeginRead;
end;

procedure TOurChunk.EndRead;
begin
  fLocker.EndRead;
end;

procedure TOurChunk.BeginWrite;
begin
  fLocker.BeginWrite;
end;

procedure TOurChunk.EndWrite;
begin
  fLocker.EndWrite;
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
        c := GetNeightborFromBlockCoord(i shl ChunkSizeLog2 + 1, j shl ChunkSizeLog2 + 1, k shl ChunkSizeLog2 + 1);
        if c <> nil then
          World.Queues.AddMethod(@c.UpdateModelLight);
      end;
end;

function TOurChunk.SetBlockDirectAuto(const x, y, z, ID, SubID : integer) : boolean;
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

function TOurChunk.GetBlockDirect(const x, y, z : integer) : TBlock;
begin
  try
    Result := fBlocks[x, y, z];
  except
    on E : Exception do raise EDirectBlockAccessException.Create('Error while getting block', Self, IntVector3(x, y, z), E);
  end;
end;

procedure TOurChunk.SetBlockDirect(const x, y, z : integer; AValue : TBlock);
var
  side : TTextureMode;
  fb : TBlock;
  c : TOurChunk;
  coord : TBlockCoord;
  CheckCoord : TIntVector3;
  RenderArea : TRenderArea;
begin
  try  
    BeginWrite;
    try
      fb := fBlocks[x, y, z];
      if AValue = nil then
        fBlocks[x, y, z] := fdefaultBlock.CreateElement(Vector3(x, y, z), 0) as TBlock
      else
        fBlocks[x, y, z] := AValue;

      RelistBlock(x, y, z);

      if fBlocks[x, y, z].NeedAfterPut then
        fBlocks[x, y, z].AfterPut(Self, BlockCoord(x, y, z));

      fModifiedAfterLoading := True;
      Coord := BlockCoord(x, y, z);
      NeedModelSolidUpdate := AllTextureSides;
      for side := Low(TTextureMode) to High(TTextureMode) do
      begin
        c := GetNeightborFromBlockCoord(x + TextureModeSidesI[side][axisX], y + TextureModeSidesI[side][axisY],
          z + TextureModeSidesI[side][axisZ]);
        if c <> nil then
        begin
          CheckCoord := (Coord + TextureModeSidesI[side]).Mask(ChunkSizeMask);
          if c.fBlocks[CheckCoord[axisX], CheckCoord[axisY], CheckCoord[axisZ]].NeedNearChangeUpdate then
            c.fBlocks[CheckCoord[axisX], CheckCoord[axisY], CheckCoord[axisZ]].NearChangeUpdate(c, OppositeSide[side], coord);
          include(c.NeedModelSolidUpdate, OppositeSide[side]);
        end;
      end;

      if fAutoLightUpdate then
        RelightArea(x, y, z, x, y, z);

      RenderAreaCollection.BeginRead;
      try
        for RenderArea in RenderAreaCollection.GetValueEnumerator do
          if RenderArea.OnChunkChange <> nil then
             RenderArea.OnChunkChange(BlockCoord(x, y, z), Self);
      finally
          RenderAreaCollection.EndRead;
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
    finally
        EndWrite;
    end;
  except
    on E : Exception do raise EDirectBlockAccessException.Create('Error while setting block', Self, IntVector3(x, y, z), E);
  end;
end;

class function TOurChunk.IsInsert(const x, y, z : integer) : boolean;
begin
  Result := (x >= 0) and (x < ChunkSize) and (y >= 0) and (y < ChunkSize) and (z >= 0) and (z < ChunkSize);
end;

class function TOurChunk.IsBorder(const x, y, z : integer) : boolean;
begin
  Result := IsInsert(x, y, z) and (not ((x > 0) and (x < ChunkSize - 1) and (y > 0) and
    (y < ChunkSize - 1) and (z > 0) and (z < ChunkSize - 1)));
end;

procedure TOurChunk.RegisterChangedBlock(const Coord : TBlockCoord);
begin
  if ChangedBlocks.Count < MaxDynamicBlockChanged then
    ChangedBlocks.Add(Coord);
end;

function TOurChunk.GetExtLightLevel(const x, y, z: integer): TRealLight;
var
  c : TOurChunk;
begin
  c := GetNeightborFromBlockCoord(x, y, z);
  if c = nil then
    exit(LightLevelToFloat(AsLightZero));
  Result := c.GetLightLevel(x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask);
end;

function TOurChunk.GetExtBlockLightLevel(const x, y, z: integer): TLight;
var
  c : TOurChunk;
begin
  c := GetNeightborFromBlockCoord(x, y, z);
  if c = nil then
    exit(AsLightZero);
  Result := c.GetBlockLightLevel(x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask);
end;

procedure TOurChunk.SetExtBlockLightLevel(const x, y, z: Integer;
  const Value: TLight);
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

function TOurChunk.GetExtSunLightLevel(const x, y, z: integer): TLight;
var
  c : TOurChunk;
begin
  c := GetNeightborFromBlockCoord(x, y, z);
  if c = nil then
    exit(AsLightZero);
  Result := c.GetSunLightLevel(x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask);
end;

function TOurChunk.GetExtDynamicLightLevel(const x, y, z: integer): TLight;
var
  c : TOurChunk;
begin
  c := GetNeightborFromBlockCoord(x, y, z);
  if c = nil then
    exit(AsLightZero);
  Result := c.GetDynamicLightLevel(x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask);
end;

procedure TOurChunk.SetExtSunLightLevel(const x, y, z: Integer;
  const Value: TLight);
var
  c : TOurChunk;
begin
  c := GetNeightborFromBlockCoord(x, y, z);
  if c = nil then
    exit;
  c.SetSunLightLevel(x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask, Value);
  NeedModelLightUpdate := AllTextureSides;
end;

procedure TOurChunk.SetExtDynamicLightLevel(const x, y, z: Integer;
  const Value: TLight);
var
  c : TOurChunk;
begin
  c := GetNeightborFromBlockCoord(x, y, z);
  if c = nil then
    exit;
  c.SetDynamicLightLevel(x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask, Value);
  NeedModelLightUpdate := AllTextureSides;
end;

function TOurChunk.GetExtBlockLightSource(const x, y, z: integer): TLight;
var
  c : TOurChunk;
begin
  c := GetNeightborFromBlockCoord(x, y, z);
  if c <> nil then
    Result := c.fBlocks[x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask].LightSource
  else
    Result := AsLightZero;
end;

function TOurChunk.GetExtSunLightSource(const x, y, z: integer): TLight;
var
  c : TOurChunk;
begin
  c := GetNeightborFromBlockCoord(x, y, z);
  if c <> nil then
    Result := c.fSunLightSource[x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask]
  else
    Result := AsLightMax;
end;

function TOurChunk.GetExtDynamicLightSource(const x, y, z: integer): TLight;
var
  c : TOurChunk;
begin
  c := GetNeightborFromBlockCoord(x, y, z);
  if c <> nil then
    Result := c.GetDynamicLightSource(x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask)
  else
    Result := AsLightZero;
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
    raise Exception.Create('Neightbor light update error');
  end;
end;

function TOurChunk.GetLightLevel(const x, y, z: integer): TRealLight;
begin                                                                          //todo: chunk temperature
  Result := max(LightLevelToFloat(ScaleLightChannels(GetSunLightLevel(x, y, z), WarmSunLight))*World.LightTime,
         max(LightLevelToFloat(GetBlockLightLevel(x, y, z)), LightLevelToFloat(GetDynamicLightLevel(x, y, z))));
end;

function TOurChunk.GetSunLightLevel(const x, y, z: integer): TLight;
begin
  Result := fSunLightValue[x, y, z];
end;

function TOurChunk.GetBlockLightLevel(const x, y, z: integer): TLight;
begin
  Result := fBlockLightValue[x, y, z];
end;

function TOurChunk.GetDynamicLightLevel(const x, y, z: Integer): TLight;
begin
  Exit(fDynamicLightValue[x, y, z]);
end;

procedure TOurChunk.SetSunLightLevel(const x, y, z: Integer; const Value: TLight);
begin
  fSunLightValue[x, y, z] := Value;
  NeedModelLightUpdate := AllTextureSides;
  NeightborLightUpdate(x, y, z);
end;

procedure TOurChunk.SetBlockLightLevel(const x, y, z: Integer; const Value: TLight);
begin
  fBlockLightValue[x, y, z] := Value;
  NeedModelLightUpdate := AllTextureSides;
  NeightborLightUpdate(x, y, z);
end;

procedure TOurChunk.SetDynamicLightLevel(const x, y, z: Integer; const Value: TLight);
begin
  fDynamicLightValue[x, y, z] := Value;
  NeedModelLightUpdate := AllTextureSides;
  NeightborLightUpdate(x, y, z);
end;

procedure TOurChunk.UpdateSunLight(const x1, z1, x2, z2 : integer; const FromY : integer;
  const ToY : integer; goDown : boolean);
var
  x, y, z, StopLoop, MinY : integer;
  c : TOurChunk;
  mv, UpValue : TLight;
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
          UpValue := c.fSunLightSource[x, 0, z]
        else
          UpValue := AsLightMax;
      end
      else
        UpValue := fSunLightSource[x, FromY + 1, z];

      StopLoop := 0;
      mv := AsLight(1);
      for y := FromY downto 0 do
      begin
        {$define NewValue := UpValue}
        NewValue := UpValue - (MaxBlockTransparency - fBlocks[x, y, z].Transparency);
        if (fSunLightSource[x, y, z] = NewValue) and (y < ToY) then
        begin
          StopLoop := y + 1;
          goDown := False;
          break;
        end;
        mv := max(NewValue, mv);
        fSunLightSource[x, y, z] := NewValue;
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

function TOurChunk.GetSmoothLightLevel(const v: TVector3): TRealLight;
var
  cv : TRealLight;
  x, y, z : integer;
  fx : function(const x, y, z : integer) : TRealLight of object;
begin
  cv := LightLevelToFloat(AsLightZero);
  x := floor(v[axisX]);
  y := floor(v[axisY]);
  z := floor(v[axisZ]);

  if (x > 0) and (x < ChunkSize) and (y > 0) and (y < ChunkSize) and (z > 0) and (z < ChunkSize) then
    fx := @GetLightLevel
  else
    fx := @GetExtLightLevel;

  cv := max(max(max(fx(x, y, z), fx(x - 1, y, z)), max(fx(x, y - 1, z), fx(x, y, z - 1))),
    max(max(fx(x - 1, y - 1, z), fx(x - 1, y, z - 1)), max(fx(x, y - 1, z - 1), fx(x - 1, y - 1, z - 1))));

  Result := cv;
end;

function TOurChunk.GetSmoothLightLevel(const v: TVector3;
  const side: TTextureMode): TRealLight;
var
  i : integer;
  x, y, z : integer;
  fx : function(const x, y, z : integer) : TRealLight of object;
  cv : TRealLight;
begin
  cv := LightLevelToFloat(AsLightZero);
  x := floor(v[axisX]);
  y := floor(v[axisY]);
  z := floor(v[axisZ]);

  if (x > 0) and (x < ChunkSize) and (y > 0) and (y < ChunkSize) and (z > 0) and (z < ChunkSize) then
    fx := @GetLightLevel
  else
    fx := @GetExtLightLevel;

  cv := LightLevelToFloat(AsLightZero);
  for i := 0 to 3 do
    UpdateIfGreater(cv, fx(trunc(x + TextureStandardModeCoord[side, i, axisX]) - 1,
      trunc(y + TextureStandardModeCoord[side, i, axisY]) - 1, trunc(z + TextureStandardModeCoord[side, i, axisZ]) - 1));

  Result := cv;
end;

function TOurChunk.GetLightedSide(const Coord : TBlockCoord; const mode : TTextureMode) : TLightedSide;
var
  i : integer;
begin
  for i := 0 to 3 do
    Result[i] := GetSmoothLightLevel(TIntVector3(Coord) + TextureStandardModeCoord[mode][i], mode);
end;

procedure TOurChunk.UpdateModelLight; //smoothlight shadown
var
  side : TTextureMode;
  v : ^TVector3;
  cl : ^TColor3b;
  i : integer;
  l : TRealLight;
begin
  if (NeedModelLightUpdate = []) or (not fLockUpdateModelLight.TryLock) then
    exit;
  try
    for side in NeedModelLightUpdate do
    begin
      fModels[side].Lock;
      exclude(NeedModelLightUpdate, side);
      v := fModels[side].VertexPtr;
      cl := fModels[side].ColorPtr;
      for i := 0 to fModels[side].Count - 1 do
      begin
        l := GetSmoothLightLevel(v[i] - Position * ChunkSize, side);
        cl[i].r := SingleToByte(l[lcRed]);
        cl[i].g := SingleToByte(l[lcGreen]);
        cl[i].b := SingleToByte(l[lcBlue]);
      end;
      fModels[side].Unlock;
    end;
    UpdateEntitiesLight;
  finally
      fLockUpdateModelLight.Unlock;
  end;
end;

procedure TOurChunk.ForceUpdateModelLight;
begin
  NeedModelLightUpdate := AllTextureSides;
  //if fLockUpdateModelLight.IsLocked then
  //  fLockUpdateModelLight.Unlock;
  UpdateModelLight;
end;

procedure TOurChunk.RelightArea(const x1, y1, z1, x2, y2, z2 : integer);
begin
  fLockUpdateModelLight.Lock;
  try
    RelightArea(x1, y1, z1, x2, y2, z2, lfkBlock);
    UpdateSunLight(x1, z1, x2, z2, y2, y1);
  finally
    fLockUpdateModelLight.Unlock;
  end;
  World.Queues.AddMethod(@UpdateModelLight);
end;

procedure TOurChunk.RelightArea(const x1, y1, z1, x2, y2, z2 : integer; const LightMode : TLightFunctionKind);
type
    TLightColors = set of TLightColor;

var
  buf : TThreeDimensionalSignedArrayOfBoolean;
  minX, minY, minZ, maxX, maxY, maxZ : integer;

  procedure DoIt(const Coord : TIntVector3; const LightLevel : TLongLight; Colors : TLightColors);
  var
    OldLight : TLongLight;
    lc : TLightColor;
    side : TTextureMode;
    c : TOurChunk;
  begin
    if buf.DataByVector[Coord] or ((Coord[axisX] > x1) and (Coord[axisX] < x2) and (Coord[axisY] > y1) and
      (Coord[axisY] < y2) and (Coord[axisZ] > z1) and (Coord[axisZ] < z2)) then
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

    OldLight := c.LightFunctions[LightMode].GetLight(Coord[axisX] and ChunkSizeMask, Coord[axisY] and
      ChunkSizeMask, Coord[axisZ] and ChunkSizeMask);

    for lc in Colors do
      if OldLight[lc] > LightLevel[lc] then
        Exclude(Colors, lc);

    if Colors = [] then
      exit
      else
      buf.DataByVector[Coord] := True;

    c.LightFunctions[LightMode].SetLight(Coord[axisX] and ChunkSizeMask,
      Coord[axisY] and ChunkSizeMask, Coord[axisZ] and ChunkSizeMask, AsLightZero);

    for lc := low(TLightColor) to High(TLightColor) do
      if OldLight[lc] = 0 then
        Exclude(Colors, lc);

    if Colors <> [] then
      for side := Low(TTextureMode) to High(TTextureMode) do
        DoIt(Coord + TextureModeSidesI[side], OldLight-1, Colors);
  end;

var
  side : TTextureMode;
  x, y, z, nx, ny, nz, d : integer;
  v : TLongLight;
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
        if ((x > x1) and (x < x2) and (y > y1) and (y < y2) and (z > z1) and (z < z2)) then
        begin
          LightFunctions[LightMode].SetExtLight(x, y, z, AsLightZero);
          buf[x, y, z] := True;
        end
        else
          DoIt(IntVector3(x, y, z), AsLightMax+1, [lcRed, lcBlue, lcGreen]);

  d := max(32, maxX+maxY+maxZ-minX-minY-minZ);
  for x := minX to maxX do
    for y := minY to maxY do
      for z := minZ to maxZ do
        if buf[x, y, z] then
        begin
          c := GetNeightborFromBlockCoord(x, y, z);
          if c = nil then
            Continue;
          v := AsLight(LENGTH_LIGHT_RESISTANCE);
          for side := Low(TTextureMode) to High(TTextureMode) do
          begin
            nx := x + TextureModeSidesI[side][axisX];
            ny := y + TextureModeSidesI[side][axisY];
            nz := z + TextureModeSidesI[side][axisZ];
            if buf[nx, ny, nz] then
              Continue;
            UpdateIfGreater(v, LightFunctions[LightMode].GetExtLight(nx, ny, nz));
          end;
          c.AddLight(x and ChunkSizeMask, y and ChunkSizeMask, z and ChunkSizeMask, v - LENGTH_LIGHT_RESISTANCE, LightMode, d, True);
        end;

  buf.Free;
end;

procedure TOurChunk.RelightDynamicSources;
var
  c : TOurChunk;
begin
  fLockUpdateModelLight.Lock;
  LockNeightborLightUpdate;
  try
    RemoveDynamicLight;
    RemoveNeightborsDynamicLight;
    AddAgainDynamicLight;
    AddAgainNeightborsDynamicLight;
  finally                       
    fLockUpdateModelLight.Unlock;
    UnLockNeightborLightUpdate;
  end;
  World.Queues.AddMethod(@UpdateModelLight);
end;

procedure TOurChunk.AddAgainDynamicLight;
var
  Value : TDynamicLightCoordMap.TKeyValuePair;
  coord : TIntVector3;
begin
  fDynamicLightCoords.BeginRead;
  try
    for value in fDynamicLightCoords.GetKeyValueEnumerator do
    begin
      coord := Value.Key - Position * ChunkSize;
      if IsInsert(coord.x, coord.y, coord.z) then
        AddLight(coord.x, coord.y, coord.z, value.Value, lfkDynamic);
    end;
  finally
    fDynamicLightCoords.EndRead;
  end;
end;

procedure TOurChunk.AddDynamicLightSource(Owner: TEntity; const Values: array of TDynamicLightRecord);
var
  Value : TDynamicLightRecord;
begin
  fLockUpdateModelLight.Lock;
  try
    fDynamicLightSources.BeginWrite;
    try
      for Value in Values do
      begin
         fDynamicLightSources.Add(Owner, Value);
         fDynamicLightCoords.Add(Value.Coord, Value.Value);
      end;
      AddAgainDynamicLight;
    finally
      fDynamicLightSources.EndWrite;
    end;
  finally                
    fLockUpdateModelLight.Unlock;
  end;
  World.Queues.AddMethod(@UpdateModelLight);
end;

procedure TOurChunk.MoveDynamicLightSource(Owner: TEntity; const Values: array of TDynamicLightRecord);
var
  Value : TDynamicLightRecord;
begin                    
  fLockUpdateModelLight.Lock;
  fDynamicLightSources.BeginWrite;
  try
    fDynamicLightSources.RemoveAllKeys(Owner);
    for Value in Values do
       fDynamicLightSources.Add(Owner, Value);
    fDynamicLightCoords.BeginWrite;
    try
      fDynamicLightCoords.Clear;
      for value in fDynamicLightSources.GetValueEnumerator do
         fDynamicLightCoords.Add(Value.Coord, Value.Value);
    finally
      fDynamicLightCoords.EndWrite;
    end;
  finally
    fDynamicLightSources.EndWrite;   
    fLockUpdateModelLight.Unlock;
  end;
  RelightDynamicSources;
end;

procedure TOurChunk.RemoveDynamicLightSource(Owner: TEntity);
var
  Value : TDynamicLightRecord;
begin                             
  fLockUpdateModelLight.Lock;
  fDynamicLightSources.BeginWrite;
  try
    if fDynamicLightSources.RemoveAllKeys(Owner) > 0 then
    begin
      fDynamicLightCoords.BeginWrite;
      try
        fDynamicLightCoords.Clear;
        for value in fDynamicLightSources.GetValueEnumerator do
           fDynamicLightCoords.Add(Value.Coord, Value.Value);
      finally
        fDynamicLightCoords.EndWrite;
      end;
    end;
  finally
    fDynamicLightSources.EndWrite;  
    fLockUpdateModelLight.Unlock;
  end;
  RelightDynamicSources;
end;

function TOurChunk.GetVertexModel(const Side : TTextureMode) : TVertexModel;
begin
  Result := fModels[side];
end;

procedure TOurChunk.RenderEntities;
var
  e : TEntity;
begin
  Entities.BeginRead;
  try
    for e in Entities.GetValueEnumerator do
        e.Render;
  finally        
    Entities.EndRead;
  end;
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
              if (x = 0) or (x = ChunkSize - 1) or (y = 0) or (y = ChunkSize - 1) or
                (z = 0) or (z = ChunkSize - 1) then
                BlockFunction := @GetBlock
              else
                BlockFunction := @GetBlockDirect;
              b := BlockFunction(x + TextureModeSidesI[side][axisX], y +
                TextureModeSidesI[side][axisY], z + TextureModeSidesI[side][axisZ]);

              if (b <> nil) and (b.Transparency > AsLightZero) then
                d.DrawModel(self, side, BlockCoord(x, y, z), VertexModels[side]);
            end;
          end;
    finally
      fModels[side].Unlock;
    end;
  end;
  //ForceUpdateModelLight;
end;

procedure TOurChunk.UpdateUnsolidModel;
var
  coord : TBlockCoord;
begin
  fUnsolidModel.Clear;
  for coord in UnSolid.GetValueEnumerator do
    GetBlockDirect(coord[axisX], coord[axisY], coord[axisZ]).DrawUnsolid(self, coord, UnsolidModel);
end;

procedure TOurChunk.UpdateAnimationModel;
var
  coord : TBlockCoord;
begin
  fAnimationModels.Clear;
  for coord in Animated.GetValueEnumerator do
    GetBlockDirect(coord[axisX], coord[axisY], coord[axisZ]).DrawAnimation(self, coord, AnimationModel);
end;

procedure TOurChunk.Tick(const DeltaTime: QWord);
var
  coord : TBlockCoord;
  e : TEntity;
begin
  if (not Loaded) or Finishing then
    Exit;
  BeginRead;
  try
    for coord in BlocksForTick.GetValueEnumerator do
      GetBlockDirect(coord[axisX], coord[axisY], coord[axisZ]).OnTick(self, coord, DeltaTime);
    for e in Entities.GetValueEnumerator do
        e.Tick(DeltaTime);
  finally
    EndRead;
  end;
end;

procedure TOurChunk.RandomTick(const Count : integer);
var
  coord : TBlockCoord;
  i, r, h : integer;
begin          
  if not Loaded then
    Exit;
  BeginRead;
  try
    if (BlocksForRandomTick.Count = 0) or Finishing then
      exit;
    for i := 0 to Count - 1 do
    begin
      h := 0;
      r := Random(BlocksForRandomTick.Count);
      while r >= BlocksForRandomTick.GetCountOnHash(h) do
            Dec(r, BlocksForRandomTick.GetCountOnHash(PostInc(h)));
      coord := BlocksForRandomTick.GetValueByIndex(h, r);
      if coord <> NilBlockCoord then
        GetBlockDirect(coord[axisX], coord[axisY], coord[axisZ]).OnRandomTick(self, coord);
    end;
  finally
    EndRead;
  end;
end;

procedure TOurChunk.UpdateNeightbors(Ignore : TOurChunk);
var
  side : TTextureMode;
begin
  for side := Low(TTextureMode) to High(TTextureMode) do
  begin
    fNeightbors[side] := World.GetChunk(Position[axisX] + TextureModeSidesI[side][axisX],
      Position[axisY] + TextureModeSidesI[side][axisY], Position[axisZ] + TextureModeSidesI[side][axisZ]);
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
  if (self <> nil) and IsInsert(x, y, z) then   //todo: skasować sprawdzenia o nil z następnych warunków ?
    exit(self);

  if (x < 0) and (fNeightbors[tmSouth] <> nil) then
  begin
    Result := fNeightbors[tmSouth].GetNeightborFromBlockCoord(x + ChunkSize, y, z);
    if Result <> nil then
      exit;
  end;
  if (x >= ChunkSize) and (fNeightbors[tmNorth] <> nil) then
  begin
    Result := fNeightbors[tmNorth].GetNeightborFromBlockCoord(x - ChunkSize, y, z);
    if Result <> nil then
      exit;
  end;
  if (y < 0) and (fNeightbors[tmDown] <> nil) then
  begin
    Result := fNeightbors[tmDown].GetNeightborFromBlockCoord(x, y + ChunkSize, z);
    if Result <> nil then
      exit;
  end;
  if (y >= ChunkSize) and (fNeightbors[tmUp] <> nil) then
  begin
    Result := fNeightbors[tmUp].GetNeightborFromBlockCoord(x, y - ChunkSize, z);
    if Result <> nil then
      exit;
  end;
  if (z < 0) and (fNeightbors[tmWest] <> nil) then
  begin
    Result := fNeightbors[tmWest].GetNeightborFromBlockCoord(x, y, z + ChunkSize);
    if Result <> nil then
      exit;
  end;
  if (z >= ChunkSize) and (fNeightbors[tmEast] <> nil) then
  begin
    Result := fNeightbors[tmEast].GetNeightborFromBlockCoord(x, y, z - ChunkSize);
    if Result <> nil then
      exit;
  end;

  Result := nil;
end;

function TOurChunk.GetEnvironment : TEnvironment;
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
    BlocksForTick.RemoveFirstKey(BlockCoord(x, y, z));

  if Block.NeedRandomTick then
    BlocksForRandomTick.Add(BlockCoord(x, y, z))
  else
    BlocksForRandomTick.RemoveFirstKey(BlockCoord(x, y, z));

  if Block.IsSolid then
    UnSolid.RemoveFirstKey(BlockCoord(x, y, z))
  else
    UnSolid.Add(BlockCoord(x, y, z));

  if Block.HasAnimation then
    Animated.Add(BlockCoord(x, y, z))
  else
    Animated.RemoveFirstKey(BlockCoord(x, y, z));
end;

procedure TOurChunk.SaveToStream(Stream : TStream);
var
  x, y, z : integer;
  MS : TMemoryStream;
begin
  Stream.WriteByte(0);
  Stream.WriteBuffer(FPosition, SizeOf(FPosition));
  BeginRead;           
  MS := TMemoryStream.Create;
  try
    for x := 0 to ChunkSize - 1 do
      for y := 0 to ChunkSize - 1 do
        for z := 0 to ChunkSize - 1 do
          DirectBlocks[x, y, z].SaveToStream(MS, Self, BlockCoord(x, y, z));
    ChangedBlocks.Clear;  
  finally
    EndRead;
  end;
  MS.Position:=0;
  FastLZ77Stream(MS, Stream, fl77Compress);
  MS.Free;
end;

procedure TOurChunk.SaveChangesToStream(Stream : TStream);
var
  c : TBlockCoord;
  MS : TMemoryStream;
begin
  if ChangedBlocks.Count > MaxDynamicBlockChanged then
    SaveToStream(Stream)
  else
  begin
    if ChangedBlocks.Count = 0 then
      exit;
    Stream.WriteByte(1);
    Stream.WriteBuffer(FPosition, SizeOf(FPosition));
    BeginRead;
    try
      MS := TMemoryStream.Create;
      for c in ChangedBlocks.GetValueEnumerator do
      begin
        MS.WriteBuffer(c, SizeOf(c));
        DirectBlocks[c[axisX], c[axisY], c[axisZ]].SaveToStream(MS, Self, c);
      end;
      ChangedBlocks.Clear;   
    finally
      EndRead;
    end;
    MS.Position:=0;
    FastLZ77Stream(MS, Stream, fl77Compress);
    MS.Free;
  end;
  c := NilBlockCoord;
  Stream.WriteBuffer(c, SizeOf(c));
end;

procedure TOurChunk.LoadEverything(Stream : TStream);
var
  x, y, z : integer;
  MS : TMemoryStream;
begin                         
  MS := TMemoryStream.Create;
  FastLZ77Stream(Stream, MS, fl77Uncompress);  //uwaga: czy nie jest wymagane dopisanie informacji o rozmiarze zapisu?
  MS.Position:=0;
  AutoLightUpdate := False;
  BeginWrite;
  try
    for x := 0 to ChunkSize - 1 do
      for y := 0 to ChunkSize - 1 do
        for z := 0 to ChunkSize - 1 do
          DirectBlocks[x, y, z].LoadFromStream(MS, Self, BlockCoord(x, y, z));
  finally
    EndWrite;
  end;
  AutoLightUpdate := True;
  RelightArea(0, 0, 0, ChunkSize - 1, ChunkSize - 1, ChunkSize - 1); 
  ChangedBlocks.Clear;
  MS.Free;
end;

procedure TOurChunk.LoadChanges(Stream : TStream);
var
  c : TBlockCoord;
  MinCoords, MaxCoords : TIntVector3;
  a : TAxis;
  MS : TMemoryStream;
begin
  if fGenerated then
  begin
    MinCoords := IntVector3(ChunkSize - 1, ChunkSize - 1, ChunkSize - 1);
    MaxCoords := IntVector3(0, 0, 0);
  end
  else
  begin                                                               
    MinCoords := IntVector3(0, 0, 0);
    MaxCoords := IntVector3(ChunkSize - 1, ChunkSize - 1, ChunkSize - 1);
    Generate;
  end;
  MS := TMemoryStream.Create;
  FastLZ77Stream(Stream, MS, fl77Uncompress);
  MS.Position:=0;
  BeginWrite;
  try
    AutoLightUpdate := False;
    while True do
    begin
      MS.ReadBuffer(c{%H-}, SizeOf(c));
      if c = NilBlockCoord then
        break;
      DirectBlocks[c[axisX], c[axisY], c[axisZ]].LoadFromStream(MS, Self, c);
      for a := Low(TAxis) to High(TAxis) do
      begin
        UpdateIfGreater(MaxCoords[a], c[a]);
        UpdateIfLesser(MinCoords[a], c[a]);
      end;
    end;
  finally
    EndWrite;
  end;
  AutoLightUpdate := True;
  ChangedBlocks.Clear;
  MS.Free;
  RelightArea(MinCoords.X, MinCoords.Y, MinCoords.Z, MaxCoords.X, MaxCoords.Y, MaxCoords.Z);
end;

procedure TOurChunk.RemoveDynamicLight;
var
  x, y, z : Integer;
begin
  for x := 0 to ChunkSize-1 do
    for y := 0 to ChunkSize-1 do
      for z := 0 to ChunkSize-1 do
        fDynamicLightValue[x, y, z] := AsLightZero;
end;

procedure TOurChunk.LockNeightborLightUpdate;
var
  side : TTextureMode;
  c : TOurChunk;
begin
  for side := Low(TTextureMode) to High(TTextureMode) do
  begin
    c :=Neightbors[side];
    if c <> nil then
      c.fLockUpdateModelLight.Lock;
  end;
end;

procedure TOurChunk.UnLockNeightborLightUpdate;
var
  side : TTextureMode;
  c : TOurChunk;
begin
  for side := Low(TTextureMode) to High(TTextureMode) do
  begin
    c :=Neightbors[side];
    if c <> nil then
      if c.fLockUpdateModelLight.IsLocked then
        c.fLockUpdateModelLight.UnLock;
  end;
end;

procedure TOurChunk.AddAgainNeightborsDynamicLight;    
var
  side : TTextureMode;
  c : TOurChunk;
begin
  for side := low(TTextureMode) to High(TTextureMode) do
  begin
      c := Neightbors[side];
      if c = nil then
        Continue;
      c.AddAgainDynamicLight;
  end;
end;

procedure TOurChunk.LoadFromStream(Stream : TStream);
var
  mode : integer;
begin
  mode := Stream.ReadByte;
  Stream.ReadBuffer(FPosition, SizeOf(FPosition));
  case mode of
   0: LoadEverything(Stream);
   1: LoadChanges(Stream);
  end;
  fLoaded:=True;
end;

function TOurChunk.GetDataHashCode: PtrUInt;
var
  buf : array[0..ChunkSize * ChunkSize * ChunkSize - 1] of QWord;
  i, x, y, z : integer;
begin
  i := 0;
  for x := 0 to ChunkSize - 1 do
    for y := 0 to ChunkSize - 1 do
      for z := 0 to ChunkSize - 1 do
        buf[PostInc(i)] := GetBlockDirect(x, y, z).GetDataHashCode;
  Result := ModuloBuf(@buf[0], SizeOf(buf), 1);
end;

constructor TOurChunk.Create(defaultBlock : TBlockCreator; const MyPosition : TIntVector3; const OurWorld : TOurWorld);
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
  LightFunctions[lfkDynamic].GetLight := @GetDynamicLightLevel;
  LightFunctions[lfkDynamic].SetLight := @SetDynamicLightLevel;
  LightFunctions[lfkDynamic].GetLightSource := @GetDynamicLightSource;
  LightFunctions[lfkDynamic].GetExtLight := @GetExtDynamicLightLevel;
  LightFunctions[lfkDynamic].SetExtLight := @SetExtDynamicLightLevel;
  LightFunctions[lfkDynamic].GetExtLightSource := @GetExtDynamicLightSource;

  fDynamicLightSources := TDynamicLightSourceMap.Create();
  fDynamicLightCoords := TDynamicLightCoordMap.Create();

  NeedModelLightUpdate := AllTextureSides;
  NeedModelSolidUpdate := AllTextureSides;
  fLocker := TMultiReadExclusiveWriteSynchronizer.Create;
  fLoaded := False;
  fGenerated := False;
  fdefaultBlock := defaultBlock;
  fPosition := MyPosition;
  fWorld := OurWorld;
  fAutoLightUpdate := True;
  fLockUpdateModelLight := TLocker.Create;
  fLighted := False;
  fModifiedAfterLoading := False;

  BlocksForRandomTick := TBlockList.Create;
  BlocksForTick := TBlockList.Create;
  UnSolid := TBlockList.Create;
  Animated := TBlockList.Create;
  ChangedBlocks := TBlockList.Create;
  fEntities := TEntitySet.Create;

  fRenderAreaCollection := TRenderAreaCollection.Create;

  for side := Low(TTextureMode) to High(TTextureMode) do
    fModels[side] := TVertexModel.Create;

  for x := 0 to ChunkSize - 1 do
    for y := 0 to ChunkSize - 1 do
      for z := 0 to ChunkSize - 1 do
      begin
        fBlocks[x, y, z] := defaultBlock.CreateElement(Vector3(x, y, z), 0) as TBlock;
        fBlockLightValue[x, y, z] := AsLightZero;
        fSunLightValue[x, y, z] := AsLightZero;
        fSunLightSource[x, y, z] := AsLightZero;
        fDynamicLightValue[x, y, z] := AsLightZero;
      end;

  UpdateNeightbors();

  for x := -1 to 1 do
    for y := -1 to 1 do
      for z := -1 to 1 do
      begin
        c := GetNeightborFromBlockCoord(x * ChunkSize + 1, y * ChunkSize + 1, z * ChunkSize + 1);
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
  BeginWrite;
  EndWrite;

  World.Queues.DequeueObject(self);

  fRenderAreaCollection.Clear;

  for x := 0 to ChunkSize - 1 do
    for y := 0 to ChunkSize - 1 do
      for z := 0 to ChunkSize - 1 do
        fBlocks[x, y, z].Free;

  fRenderAreaCollection.Free;

  for side := Low(TTextureMode) to High(TTextureMode) do
    fModels[side].Free;
  fUnsolidModel.Free;
  fAnimationModels.Free;

  fEntities.Free;
  fDynamicLightSources.Free;
  fDynamicLightCoords.Free;
  ChangedBlocks.Free;
  BlocksForRandomTick.Free;
  BlocksForTick.Free;
  UnSolid.Free;
  Animated.Free;
  fLocker.Free;
  fLockUpdateModelLight.Free;
  inherited Destroy;
end;

{ TBlockCreator }

function TBlockCreator.GetType : TElementType;
begin
  Result := etBlock;
end;

function TBlockCreator.CreateBlock(const Coord : TIntVector3; const SubID : integer) : TBlock;
begin
  Result := CreateElement(Coord, SubID) as TBlock;
end;

{ TBlock }

function TBlock.GetBlockCreator : TBlockCreator;
begin
  Result := Creator as TBlockCreator;
end;

function TBlock.GetTag : PBlockDataTag;
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

function TBlock.NeedAfterPut : boolean;
begin
  Result := False;
end;

function TBlock.NeedNearChangeUpdate : boolean;
begin
  Result := False;
end;

function TBlock.HasCollisionBox: Boolean;
begin
  Exit(True);
end;

function TBlock.IsFluid: Boolean;
begin
  Exit(False);
end;

function TBlock.Friction: Double;
begin
  Exit(0.3); //ice will have less
end;

function TBlock.Resilence: Double;
begin
  Exit(0.03);
end;

function TBlock.ResilenceMinVelocity: Double;
begin
  Exit(1);
end;

function TBlock.GetCollisionBox(Chunk: TOurChunk; const Coord: TBlockCoord): TCollisionBox;
begin
  Result.Position := ChunkCoordToBlockCoord(Chunk.Position) + Coord + Vector3(0.5, 0.5, 0.5);
  Result.Size := Vector3(1, 1, 1);
  Result.RotationMatrix := IdentityMatrix;
end;

procedure TBlock.CreateDarkModel(var DarkModel: TDarkModel; const sides: TTextureDrawSides);
begin
  //do nothing: this is only to provide abstract error
end;

procedure TBlock.DrawModel(Chunk: TOurChunk; Side: TTextureMode; const Coord: TBlockCoord; VertexModel: TVertexModel);
var
  i : Integer;
  DarkModel : TDarkModel;
begin
  DarkModel := TDarkModel.Empty;
  CreateDarkModel(DarkModel, [side]);
  for i := 0 to DarkModel.AddCount-1 do
    VertexModel.AddWall(RealCoord(Chunk.Position, Coord), DarkModel.WallCorners[i], DarkModel.TextureCorners[i], DarkModel.TextureRects[i], Chunk.GetLightedSide(Coord, Side));
end;

procedure TBlock.DrawUnsolid(Chunk: TOurChunk; const Coord: TBlockCoord;
  VertexModel: TVertexModel);
begin
  //do nothing: this is only to provide abstract error
end;

procedure TBlock.DrawAnimation(Chunk: TOurChunk; const Coord: TBlockCoord;
  VertexModel: TVertexModel);
begin
  //do nothing: this is only to provide abstract error
end;

procedure TBlock.OnTick(Chunk: TOurChunk; const Coord: TBlockCoord;
  const DeltaTime: QWord);
begin
  //do nothing: this is only to provide abstract error
end;

procedure TBlock.OnRandomTick(Chunk: TOurChunk; const Coord: TBlockCoord);
begin
  //do nothing: this is only to provide abstract error
end;

procedure TBlock.AfterPut(Chunk: TOurChunk; const Coord: TBlockCoord);
begin
  //do nothing: this is only to provide abstract error
end;

procedure TBlock.NearChangeUpdate(Chunk: TOurChunk; const side: TTextureMode;
  const Coord: TBlockCoord);
begin
  //do nothing: this is only to provide abstract error
end;

procedure TBlock.SaveToStream(Stream : TStream; Chunk : TOurChunk; const Coord : TBlockCoord);
begin
  Stream.WriteAnsiString(GetTextID);
  Stream.WriteDWord(GetSubID);
end;

function TBlock.LoadFromStream(Stream : TStream; Chunk : TOurChunk; const Coord : TBlockCoord) : boolean;
var
  StreamPosition : integer;
  ReadedTextID : AnsiString;
  ReadedID, ReadedSubID : integer;
begin
  StreamPosition := Stream.Position;
  ReadedTextID := Stream.ReadAnsiString;
  ReadedID := Chunk.World.OurGame.Environment.GetID(ReadedTextID);
  ReadedSubID := Stream.ReadDWord;
  if (ReadedID <> GetID) or (ReadedSubID <> GetSubID) then
  begin
    Stream.Position := StreamPosition;
    //warning: this class could be destroyed after this line
    if not Chunk.SetBlockDirectAuto(Coord[axisX], Coord[axisY], Coord[axisZ], ReadedID, ReadedSubID) then
      raise Exception.Create('Invalid ID in stream: ' + ReadedTextID);
    Chunk.GetBlockDirect(Coord[axisX], Coord[axisY], Coord[axisZ]).LoadFromStream(Stream, Chunk, Coord);
    Result := False;
  end
  else
    Result := True;
end;

function TBlock.Transparency: TLight;
begin
  Result := AsLightZero;
end;

function TBlock.LightSource: TLight;
begin
  Result := AsLightZero;
end;

function TBlock.Clone(const NewCoord : TIntVector3) : TBlock;
begin
  Result := Creator.CreateElement(NewCoord, getSubID) as TBlock;
end;

function TBlock.GetDataHashCode : PtrInt;
var
  buf : array[0..1] of QWord;
begin
  buf[0] := GetID;
  buf[1] := GetSubID;
  Result := ModuloBuf(@buf[0], SizeOf(buf), 1);
end;

{ TEntity }

procedure TEntity.SwitchChunk(NewChunk: TOurChunk);
begin                 
  LockForWriting;
  try
    if (FChunk <> nil) and Assigned(FChunk) then
    begin
      FChunk.Entities.BeginWrite;
      try
        FChunk.Entities.RemoveFirstKey(Self);
      finally
        FChunk.Entities.EndWrite;
      end;
      OnLeaveChunk(FChunk);
    end;
    FChunk := NewChunk;
    if (FChunk <> nil) and Assigned(FChunk) then
    begin
      if FChunk.Loaded then
      begin
        FChunk.Entities.BeginWrite;
        try
          FChunk.Entities.UpdateFirst(Self);
        finally
          FChunk.Entities.EndWrite;
        end;
        OnEnterChunk(FChunk);
      end
      else
         FChunk := nil;
    end;
  finally
    UnlockFromWriting;
  end;
end;

procedure TEntity.SetVelocity(AValue: TVector3);
begin
  if StateBox.Velocity=AValue then Exit;
  LockForWriting;
  StateBox.Velocity:=AValue;
  UnlockFromWriting;
end;

function TEntity.GetVelocity: TVector3;
begin
  Exit(StateBox.Velocity);
end;

procedure TEntity.SetWorld(AValue: TOurWorld);
var
  p : TVector3;
begin
  if fWorld=AValue then Exit;
  fWorld:=AValue;
  World.Entities.BeginWrite;
  try
     World.Entities.Add(Self); //TODO: nie dać tego w World?
  finally
     World.Entities.EndWrite;
  end;
  p := Position;
  SwitchChunk(World.GetChunkFromBlockCoors(floor(p[axisX]), floor(p[axisY]), floor(p[axisZ])));
end;

function TEntity.GetRotation: TRotationVector;
begin
  Exit(FRotation);
end;

procedure TEntity.SetPosition(const AValue: TVector3);
begin
  StateBox.CollisionBox.Position := AValue;
end;

procedure TEntity.SetRotation(const AValue: TRotationVector);
begin
  FRotation:=AValue;
  StateBox.CollisionBox.RotationMatrix := CreateRotateMatrixZXY(FRotation);
end;

procedure TEntity.SetRotation(const AValue: TRotationVector; const UpdateBoxes: Boolean);
begin
  if GetRotation <> AValue then
  begin
    SetRotation(AValue);
    if UpdateBoxes then
       AfterMovement;
  end;
end;

function TEntity.GetPosition: TVector3;
begin
  Exit(StateBox.CollisionBox.Position);
end;

procedure TEntity.SetPosition(const AValue: TVector3; const UpdateBoxes: Boolean);
begin
  if AValue <> GetPosition then
  begin
    SetPosition(AValue);
    if UpdateBoxes then
       AfterMovement;
  end;
end;

procedure TEntity.AfterMovement;
begin
  StateBox.CollisionBox.RotationMatrix := CreateRotateMatrixZXY(GetRotation);
  AutoSwitchChunk;
end;

function TEntity.GetLightLevel(const Point: TVector3): TRealLight;
begin
  if (Chunk <> nil) and Assigned(Chunk) then
    Exit(Chunk.GetSmoothLightLevel(Point - ChunkCoordToBlockCoord(Chunk.Position)));

  Result[lcRed] := 0;
  Result[lcGreen] := 0;
  Result[lcBlue] := 0;
end;

function TEntity.Unregistered: Boolean;
begin
  Exit(FUnregistered);
end;

procedure TEntity.Unregister;
begin
  FUnregistered := True;
  World.Queues.DequeueObject(Self);
  World.Entities.BeginWrite;
  try
    World.Entities.RemoveFirstKey(Self);
  finally
    World.Entities.EndWrite;
  end;
  SwitchChunk(nil);
end;

procedure TEntity.AutoSwitchChunk;
var
  NewChunk : TOurChunk;
begin
  NewChunk := World.GetChunkFromBlockCoors(floor(StateBox.CollisionBox.Position[axisX]), floor(StateBox.CollisionBox.Position[axisY]), floor(StateBox.CollisionBox.Position[axisZ]));
  if NewChunk <> FChunk then
    SwitchChunk(NewChunk);
end;

procedure TEntity.LockForReading;
begin
  FLock.Beginread;
end;

procedure TEntity.UnlockFromReading;
begin
  FLock.Endread;
end;

procedure TEntity.LockForWriting;
begin
  FLock.Beginwrite;
end;

procedure TEntity.UnlockFromWriting;
begin
  FLock.Endwrite;
end;

procedure TEntity.OnLeaveChunk(AChunk: TOurChunk);
begin
  //
end;

procedure TEntity.OnEnterChunk(AChunk: TOurChunk);
begin
  //
end;

procedure TEntity.StopMovement;
begin
    StateBox.Velocity := Vector3(0, 0, 0);
    StateBox.AngularVelocityMatrix := IdentityMatrix;
end;

procedure TEntity.Tick(const DeltaTime: QWord);
var
  PositionB : TIntVector3;
  c : TOurChunk;
begin
  OnTick(DeltaTime);
  PositionB := floor(GetPosition);
  c := World.GetChunkFromBlockCoors(PositionB[axisX], PositionB[axisY], PositionB[axisZ]);

  OnTick(DeltaTime);
  if c <> Chunk then
    SwitchChunk(c);
end;

function TEntity.CheckCollisionsWithBlock: Boolean;
begin
  Exit(True);
end;

function TEntity.CheckCollisionsWithEntity: Boolean;
begin
  Exit(True);
end;

function TEntity.AutoComputeCollisions: Boolean;
begin
  Exit(Chunk <> nil);
end;

procedure TEntity.OnCollisionWithBlock(Blocks: TCollisionBoxSet);
var
  b : TBlock;
  e : Double;  
  Which : TCollisionBoxSet.TKeyValuePair;
  Where, Resilence : TVector3;
  side : TAxis;
begin
  e := 1;
  Resilence := Vector3(1, 1, 1);
  for Which in Blocks.GetKeyValueEnumerator do
  begin
    b := Chunk.GetBlock(Which.Key[AxisX], Which.Key[AxisY], Which.Key[AxisZ]);
    if b = nil then
       Continue;
    if CheckCollision(Which.Value, StateBox.CollisionBox, Where{%H-}) then
    begin
      side := TextureModeSidesAxis[StateBox.CollisionBox.GetHittedSide(Where)].Axis;
      e := min(e, exp(-abs(min(1, (b.Friction * StateBox.Velocity[side])))));
      if (b.Resilence > 0) and (abs(StateBox.Velocity[side]) > b.ResilenceMinVelocity) then
         Resilence[side] := min(Resilence[side],  -b.Resilence)
         else
         StateBox.Velocity[side] := 0;
    end;
  end;
  for side := Low(TAxis) to High(TAxis) do
    if Resilence[side] < MinDouble then
    StateBox.Velocity[side] *= Resilence[side]
    else
    StateBox.Velocity[side] *= e;
  StateBox.AngularVelocityMatrix := StateBox.AngularVelocityMatrix*e + (1-e)*IdentityMatrix;
end;

procedure TEntity.OnCollisionWithEntity;
begin
  //Do nothing
end;

procedure TEntity.OnBeInBlock(Blocks: TCollisionBoxSet; const DeltaTime: Double);
var
  r, e : Double;
  Where : TVector3;
  b : TBlock;
  Which : TCollisionBoxSet.TKeyValuePair;
  TestBox : TCollisionBox;
begin
  e := 1;
  TestBox :=StateBox.CollisionBox;
  TestBox.Size /= 3;
  for Which in Blocks.GetKeyValueEnumerator do
  begin
    if not CheckCollision(TestBox, Which.Value, Where{%H-}) then
       Continue;

    b := Chunk.GetBlock(Which.Key[AxisX], Which.Key[AxisY], Which.Key[AxisZ]);
    if b = nil then
       Continue;
    if b.IsFluid then
      e := min(e, exp(-DeltaTime))
    else
      e := 0;
  end;

  r := Hypot3(StateBox.Velocity);
  if r <> 0 then
  begin
    StateBox.Velocity *= e;
    StateBox.AngularVelocityMatrix := (1-e) * StateBox.AngularVelocityMatrix + e*IdentityMatrix;
  end;
end;

function TEntity.MovementResistance: Double;
begin
  Exit(0.01);
end;

procedure TEntity.UpdateModelLight;
begin
  UpdateModel;
end;

function TEntity.GetBlockCollisionBox(const LocalBlockPosition: TIntVector3; out CollisionBox : TCollisionBox): Boolean;
var
  c : TOurChunk;
  Block : TBlock;   
  MaskedPosition : TIntVector3;
begin           
  c := Chunk.GetNeightborFromBlockCoord(LocalBlockPosition.X, LocalBlockPosition.Y, LocalBlockPosition.Z);
  if c = nil then
     Exit(False);
  MaskedPosition := LocalBlockPosition.Mask(ChunkSizeMask);
  Block := c.GetBlockDirect(MaskedPosition.X, MaskedPosition.Y, MaskedPosition.Z);
  if Block = nil then
    Exit(False);
  if Block.HasCollisionBox then
    CollisionBox := Block.GetCollisionBox(c, BlockCoord(MaskedPosition.X, MaskedPosition.Y, MaskedPosition.Z))
  else
    Exit(False);
  Exit(True);
end;

function TEntity.FindCollision(const FromTime, ToTime: Double;
  Blocks: TCollisionBoxSet; var Where: TVector3; const MaxDepth: Integer): Double;
var
  HalfTime : Double;      
  TestState : TVelocityBox;

  CollidedBlocks : TCollisionBoxSet;
  BlockEnumerator : TCollisionBoxSet.TKeyValuePair;
begin                              
  if MaxDepth <= 0 then
     Exit(ToTime);
  HalfTime := (FromTime + ToTime) / 2;
  TestState := StateBox.TimeCalculate(HalfTime);

  CollidedBlocks := TCollisionBoxSet.Create();

  for BlockEnumerator in Blocks.GetKeyValueEnumerator do
    if CheckCollision(TestState.CollisionBox, BlockEnumerator.Value, Where{%H-}) then
       CollidedBlocks.Add(BlockEnumerator.Key, BlockEnumerator.Value);

  if CollidedBlocks.Count > 0 then
  begin
     Result := FindCollision(FromTime, HalfTime, CollidedBlocks, Where, MaxDepth-1);
     Blocks.Clear;
     CollidedBlocks.ForEach(@Blocks.Add);
     CollidedBlocks.Free;
  end
  else
  begin                                                                                
     CollidedBlocks.Free;
     Result := FindCollision(HalfTime, ToTime, Blocks, Where, MaxDepth-1);
  end;
end;

procedure TEntity.ComputeMovement(const DeltaTime: Double);
var
  i, Count : Integer;
  x, y, z : Integer;
  NewState : TVelocityBox;
  a, b : TIntVector3;
  ChunkCoord : TIntVector3;
  BlockCollisionBox : TCollisionBox;
  Where : TVector3;
  t, AllTime, CalculatedTime, e : Double;
  BlockCollisionBoxSet : TCollisionBoxSet;
begin
  LockForWriting;
  try
    if Chunk = nil then
       Exit;

    e := exp(-sqr(DeltaTime * MovementResistance));
    StateBox.Velocity *= e;
    StateBox.AngularVelocityMatrix *= e;
    
    BlockCollisionBoxSet := TCollisionBoxSet.Create();
    try
      count := max(1, round(DeltaTime/StateBox.SuggestedDelay));

      AllTime := DeltaTime;
      CalculatedTime := 0;
      if (StateBox.Velocity <> Vector3(0, 0, 0)) or (not CompareMem(@StateBox.AngularVelocityMatrix, @IdentityMatrix, SizeOf(TMatrix3x3))) then
      begin
        for i := 0 to Count do
        begin
          BlockCollisionBoxSet.Clear;
          NewState := StateBox.TimeCalculate(i*DeltaTime/Count-CalculatedTime);

          a := floor(NewState.CollisionBox.Position-1);
          b := ceil(NewState.CollisionBox.Position+1);

          NewState.CollisionBox.GetIntegerBorders(a, b);

          ChunkCoord := ChunkCoordToBlockCoord(Chunk.Position);
          a := a - ChunkCoord;
          b := b - ChunkCoord;

          for x := a[AxisX] to b[AxisX] do
            for y := a[AxisY] to b[AxisY] do
                for z := a[AxisZ] to b[AxisZ] do
                   if GetBlockCollisionBox(IntVector3(x, y, z), BlockCollisionBox) and CheckCollision(NewState.CollisionBox, BlockCollisionBox, Where{%H-}) then
                     BlockCollisionBoxSet.Add(IntVector3(x, y, z), BlockCollisionBox);

          if BlockCollisionBoxSet.Count > 0 then
            if i = 0 then
            begin
               OnCollisionWithBlock(BlockCollisionBoxSet);
               //OnBeInBlock(BlockCollisionBoxSet, DeltaTime);
            end
            else
            begin
               t := FindCollision((i-1)*DeltaTime/Count, i*DeltaTime/Count, BlockCollisionBoxSet, Where);
               StateBox := StateBox.TimeCalculate(t);
               AllTime-=t;
               CalculatedTime += t;
               OnCollisionWithBlock(BlockCollisionBoxSet);
            end;

          if (StateBox.Velocity = Vector3(0, 0, 0)) or (AllTime <= 0) then
            Break;
        end;

        if AllTime > 0 then
          StateBox := StateBox.TimeCalculate(AllTime);
      end;
    finally       
      BlockCollisionBoxSet.Free;
    end;
  finally
    UnlockFromWriting;
  end;      
  AutoSwitchChunk;
end;

function TEntity.GetID: integer;
begin
  Exit(Creator.GetID);
end;

procedure TEntity.OnTick(const DeltaTime: QWord);
begin
  //do nothing: this is only to provide abstract error
end;

procedure TEntity.OnHit(Entity: TEntity);
begin
  //do nothing: this is only to provide abstract error
end;

procedure TEntity.OnOptions(Entity: TEntity);
begin
  //do nothing: this is only to provide abstract error
end;

constructor TEntity.Create(TheWorld: TOurWorld; MyCreator: TElementCreator; const APosition: TVector3);
begin
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;
  SetPosition(APosition);           
  StateBox.AngularVelocityMatrix := IdentityMatrix;
  StateBox.CollisionBox.RotationMatrix := IdentityMatrix;
  FUnregistered := False;
  inherited Create(MyCreator);
  fWorld := nil;
  SetWorld(TheWorld);
end;

constructor TEntity.Create(MyCreator: TElementCreator; const APosition: TVector3);
begin
  Create(nil, MyCreator, APosition);
end;

destructor TEntity.Destroy;
begin
  if not FUnregistered then
    Unregister;
  FLock.Free;
  inherited Destroy;
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

  for side := Low(TTextureMode) to High(TTextureMode) do
    vm.AddWall(fShape.Position, c.Corners[side], c.Textures[side],
      fShape.Texture, AsLight(LightLevel));
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
  fShapes := [];
end;

destructor TModel.Destroy;
var
  i : integer;
begin
  for i := 0 to fShapeCount - 1 do
    fShapes[i].Free;
  fShapeCount := 0;
  SetLength(fShapes, 0);
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
