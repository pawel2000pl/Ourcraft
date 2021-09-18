program SingleMode;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  {$IFDEF UseCThreads}
  cmem {fastcmem}, {$ifdef DEBUGBUILD} {heaptrc,} MemGuard, {$endif}
  cthreads,
  {$ENDIF}
  {$ENDIF}
  MainOurcraftDirectory,
  Interfaces, // this includes the LCL widgetset
  Forms, lazopenglcontext, SingleModeUnit, OurData, OurUtils, ProcessUtils,
  Sorts, GlCamera, Models, Freerer, Queues, Locker, Chain, WorldGenerator,
  DeterminedRandomGenerator, SimpleCache, CustomSaver, FileSaver,
  SaverPaths, LightTypes, ServerService, ClientService, SocketCommands,
  NearestVectors, CollisionBoxes, TextureMode, PhisicalBox, CalcUtils, OurGame,
  SimpleTypes, ThreeDimensionalArrayOfBoolean, UniversalImage, PrefixSI,
  AsyncMicroTimer, AsyncMilliTimer, ArrayOfNumber, MovingBlock;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
