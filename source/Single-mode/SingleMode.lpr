program SingleMode;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  {$IFDEF UseCThreads}
  cmem {fastcmem}, {$ifdef DEBUGBUILD} MemGuard, {$endif}
  cthreads,
  {$ENDIF}
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazopenglcontext, SingleModeUnit, OurData, OurUtils, ProcessUtils,
  Sorts, GlCamera, Models, Freerer, Queues, Locker, Chain, WorldGenerator,
  DeterminedRandomGenerator, SimpleCache, ArrayOfNumber, Collections,
  CustomSaver, FileSaver, SaverPaths, LightTypes, ServerService, ClientService,
  SocketCommands;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
