program SingleMode;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  {$IFDEF UseCThreads}
  cmem {fastcmem}, {$ifdef DEBUGBUILD} heaptrc, MemGuard, {$endif}
  cthreads,
  {$ENDIF}
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazopenglcontext, SingleModeUnit, GlCamera, OurData, air, glowstone,
  MovingBlock, stone, PlayerEntity, ChunkLight;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
