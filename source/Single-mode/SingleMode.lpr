program SingleMode;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  {$IFDEF UseCThreads}
  cmem, {$ifdef DEBUGBUILD} MemGuard, {$endif}
  cthreads,
  {$ENDIF}
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazopenglcontext, SingleModeUnit, OurData, OurUtils, ProcessUtils,
  Sorts, GlCamera, Models, Freerer, Queues, Locker, Chain, WorldGenerator,
  DeterminedRandomGenerator, SimpleCache, ArrayOfNumber, Collections, CustomSaver;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
