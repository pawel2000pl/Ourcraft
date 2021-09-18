unit ClientService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock,
  OurGame, OurUtils, SocketCommands,
  CustomSaver, Locker;

type
  TClientService = class;

  TClientServiceRenderArea = class(TRenderArea)
    //todo
  end;

  { TClientServiceSaver }

  TClientServiceSaver = class(TCustomSaver)
  private
    FService : TClientService;                
  protected
    procedure SendPath(const Path : array of AnsiString);
  public
    property Service : TClientService read FService;

    function Exists(const Path: array of AnsiString): Boolean; override;
    procedure Save(const {%H-}Path: array of AnsiString; {%H-}Stream: TStream); override; overload;
    procedure Load(const Path : array of AnsiString; Stream : TStream); override; overload;

    constructor Create(AService : TClientService);
  end;

  { TClientService }

  TClientService = class
  const
    ListeningTime = 1;
    TimeOut = 1500;
  private
    FConnectionSocket : TTCPBlockSocket;
    FWorld : TOurWorld;
    FRenderArea : TClientService;
    FSaver : TClientServiceSaver;
    FPort : Word;
    FIP: AnsiString;
    FLock : TLocker;

    procedure Load;
    procedure ReadConnection;
    procedure Listen;
    procedure Connect;
    procedure Disconnect;
  public
    procedure Lock;
    procedure UnLock;
    property World : TOurWorld read FWorld;
    property ConnectionSocket : TTCPBlockSocket read FConnectionSocket;
    property IP : AnsiString read FIP;
    property Port : Word read FPort;

    constructor Create(ServerIP : AnsiString; ServerPort : Word; defaultBlock : TBlockCreator; Game : TOurGame; WorldGenerator : TAbstractGenerator);
    destructor Destroy; override;
  end;

implementation

{ TClientServiceSaver }

procedure TClientServiceSaver.SendPath(const Path: array of AnsiString);
var
  s : AnsiString;
begin
  Service.ConnectionSocket.SendInteger(Length(Path));
  for s in Path do
    Service.ConnectionSocket.SendString(s);
end;

function TClientServiceSaver.Exists(const Path: array of AnsiString): Boolean;
begin
  try
    Service.Lock;
    Service.ConnectionSocket.SendString(scExists);
    SendPath(Path);
    Result := scTrue = Service.ConnectionSocket.RecvString(Service.TimeOut);
  finally
    Service.UnLock;
  end;
end;

procedure TClientServiceSaver.Save(const Path: array of AnsiString; Stream: TStream);
begin
  //Do nothing: client cannot write anything in that way.
end;

procedure TClientServiceSaver.Load(const Path: array of AnsiString; Stream: TStream);
begin
  try
    Service.Lock;
    Service.ConnectionSocket.SendString(scAsk);
    SendPath(Path);
    Service.ConnectionSocket.RecvStream(Stream, Service.TimeOut);
  finally
    Service.UnLock;
  end;
end;

constructor TClientServiceSaver.Create(AService: TClientService);
begin
  FService := AService;
  inherited Create;
end;

{ TClientService }

procedure TClientService.Load;
var
  Path : array of AnsiString;
  MS : TMemoryStream;
  i, c : Integer;
begin
  c := ConnectionSocket.RecvInteger(TimeOut);
  SetLength(Path, c);
  for i := 0 to c-1 do
    Path[i] := ConnectionSocket.RecvString(Timeout);
  MS := TMemoryStream.Create;
  ConnectionSocket.RecvStream(MS, TimeOut);
  MS.Position:=0;
  FSaver.DoLoadEvent(Path, MS);
  MS.Free;
  SetLength(Path, 0);
end;

procedure TClientService.ReadConnection;
var
  Command : AnsiString;
begin
  Command := ConnectionSocket.RecvString(TimeOut);
  if Command = scLoad then
    Load;
end;

procedure TClientService.Listen;
begin
  try
  Lock;
    if ConnectionSocket.CanRead(ListeningTime) then
      ReadConnection;
  finally
    UnLock;
  end;
  World.MilliTimer.AddMethod(@Listen, ListeningTime);
end;

procedure TClientService.Connect;
begin
  ConnectionSocket.Connect(IP, IntToStr(Port));
end;

procedure TClientService.Disconnect;
begin
  ConnectionSocket.CloseSocket;
end;

procedure TClientService.Lock;
begin
  FLock.Lock;
end;

procedure TClientService.UnLock;
begin
  FLock.Unlock;
end;

constructor TClientService.Create(ServerIP: AnsiString; ServerPort: Word;
  defaultBlock: TBlockCreator; Game: TOurGame;
  WorldGenerator: TAbstractGenerator);
begin
  FIP:=ServerIP;
  FPort:=ServerPort;
  FLock := TLocker.Create;
  Connect;
  FSaver := TClientServiceSaver.Create(Self); //todo
  Game.Environment.Remote:=True;
  FWorld := TOurWorld.Create(defaultBlock, Game, WorldGenerator, FSaver);
end;

destructor TClientService.Destroy;
begin
  Disconnect;
  FWorld.Free;
  FSaver.Free;
  FConnectionSocket.Free;
  FLock.Free;
  inherited Destroy;
end;

end.

