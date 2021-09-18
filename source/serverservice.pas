unit ServerService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock,
  OurGame, OurUtils,
  FileSaver, SaverPaths,
  Queues, TinyHashData, Locker, AsyncMilliTimer;

type
  TServerService = class;

  { TServerSideRenderArea }

  TServerSideRenderArea = class(TRenderArea)
  private
    ConnectionSocket : TTCPBlockSocket;
    FServer : TServerService;
  public
    property Server : TServerService read FServer;
    constructor Create(ASocket : LongInt; AServer : TServerService; OurWorld: TOurWorld);
    destructor Destroy; override;
  end;

  TSetOfServerSideRenderArea = specialize TTinyHashSet<TServerSideRenderArea>;

  { TServerService }

  TServerService = class
  const
    ListeningTime = 10;
  private
    FWorld : TOurWorld;
    FQueue : TQueueManager;
    FMilliTimer : TMilliTimer;
    FPort : Word;
    ListenerSocket : TTCPBlockSocket;

    FAreaSet : TSetOfServerSideRenderArea;   
    FLock : TLocker;

    procedure InitListening;
    procedure FinitListening;
    procedure Listen;
    procedure DisconnectAll;
  public
    property World : TOurWorld read FWorld;
    property Queue : TQueueManager read FQueue;
    property MilliTimer : TMilliTimer read FMilliTimer;
    property Port : Word read FPort;
    property AreaSet : TSetOfServerSideRenderArea read FAreaSet;
    property Lock : TLocker read FLock;

    constructor Create(AWorld : TOurWorld; const APort : Word; const ExpectedClientsCount : Integer = 8);
    destructor Destroy; override;
  end;



implementation

{ TServerSideRenderArea }

constructor TServerSideRenderArea.Create(ASocket: LongInt;
  AServer: TServerService; OurWorld: TOurWorld);
begin
  inherited Create(OurWorld);
  FServer:=AServer;
  ConnectionSocket := TTCPBlockSocket.Create;
  ConnectionSocket.Socket:=ASocket;
  try
    Server.Lock.Lock;
    Server.AreaSet.Add(Self);
  finally
    Server.Lock.Unlock;
  end;

  //todo: working
end;

destructor TServerSideRenderArea.Destroy;
begin
  ConnectionSocket.CloseSocket;
  ConnectionSocket.Free;      
  FServer.Queue.DequeueObject(Self);
  try
    FServer.Lock.Lock;
    FServer.AreaSet.RemoveFirstKey(Self);
  finally        
    FServer.Lock.Unlock;
  end;
  inherited Destroy;
end;

{ TServerService }

procedure TServerService.InitListening;
begin
  ListenerSocket := TTCPBlockSocket.Create;
  ListenerSocket.CreateSocket;
  ListenerSocket.SetLinger(true,10);
  ListenerSocket.Bind('0.0.0.0',IntToStr(Port));
  ListenerSocket.Listen;
  Listen;
end;

procedure TServerService.FinitListening;
begin
  Queue.DequeueObject(Self);
  sleep((17*ListeningTime) shr 4);
  ListenerSocket.Free;
end;

procedure TServerService.Listen;
begin
  if ListenerSocket.canread(ListeningTime) then
    World.AddRenderArea(TServerSideRenderArea.Create(ListenerSocket.Accept, Self, World));
  Queue.AddMethod(@Listen);
end;

procedure TServerService.DisconnectAll;
begin
  AreaSet.ForEach(AreaSet.TForEachKeyValueStaticProcedure(@FreeObject));
  AreaSet.Clear;
end;

constructor TServerService.Create(AWorld: TOurWorld; const APort: Word;
  const ExpectedClientsCount: Integer);
begin
  FWorld := AWorld;
  FPort:=APort;
  FLock := TLocker.Create;
  FAreaSet := TSetOfServerSideRenderArea.Create;
  FQueue := TQueueManager.Create(1, ExpectedClientsCount+1);
  FMilliTimer := TMilliTimer.Create(FQueue);
  ListenerSocket := nil;
  InitListening;
end;

destructor TServerService.Destroy;
begin
  FinitListening;
  DisconnectAll;
  FAreaSet.Free;
  FMilliTimer.Free;
  FQueue.Free;
  FLock.Free;
  inherited Destroy;
end;

end.

