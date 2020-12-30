unit ServerService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock,
  OurGame, OurUtils,
  FileSaver, SaverPaths,
  Queues, Collections;

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

  TSetOfServerSideRenderArea = specialize Collections.TCustomSet<TServerSideRenderArea>;

  { TServerService }

  TServerService = class
  const
    ListeningTime = 10;
  private
    FWorld : TOurWorld;
    FQueue : TQueueManager2;
    FPort : Word;
    ListenerSocket : TTCPBlockSocket;

    FAreaSet : TSetOfServerSideRenderArea;

    procedure InitListening;
    procedure FinitListening;
    procedure Listen;
    procedure DisconnectAll;
  public
    property World : TOurWorld read FWorld;
    property Queue : TQueueManager2 read FQueue;
    property Port : Word read FPort;
    property AreaSet : TSetOfServerSideRenderArea read FAreaSet;

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
  //todo: working
end;

destructor TServerSideRenderArea.Destroy;
begin
  ConnectionSocket.CloseSocket;
  ConnectionSocket.Free;      
  FServer.Queue.DequeueObject(Self);
  FServer.AreaSet.RemoveItem(Self);
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
var
  r : TServerSideRenderArea;
begin
  if ListenerSocket.canread(ListeningTime) then
  begin
    r := TServerSideRenderArea.Create(ListenerSocket.Accept, Self, World);
    AreaSet.Add(r);
    World.AddRenderArea(r);
  end;
  Queue.AddMethod(@Listen);
end;

procedure TServerService.DisconnectAll;
begin
  while AreaSet.GetCount>0 do
    AreaSet.Get(0).Free;
end;

constructor TServerService.Create(AWorld: TOurWorld; const APort: Word;
  const ExpectedClientsCount: Integer);
begin
  FWorld := AWorld;
  FPort:=APort;
  FAreaSet := TSetOfServerSideRenderArea.Create;
  FQueue := TQueueManager2.Create(1, ExpectedClientsCount+1);
  ListenerSocket := nil;
  InitListening;
end;

destructor TServerService.Destroy;
begin
  FinitListening;
  DisconnectAll;
  FAreaSet.Free;
  FQueue.Free;
  inherited Destroy;
end;

end.

