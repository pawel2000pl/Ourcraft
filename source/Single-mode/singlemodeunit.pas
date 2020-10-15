unit SingleModeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  OpenGLContext, OurGame, OurUtils, Sorts, GLext, gl, glu, Glut,
  CalcUtils, ProcessUtils, Math, Models, GlCamera, WorldGenerator;

type

  { TMainForm }

  TMainForm = class(TForm)
    Timer1 : TTimer;
    Timer2 : TTimer;
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure FormKeyPress(Sender : TObject; var Key : char);
    procedure OnAppIdle(Sender : TObject; var Done : boolean);
    procedure OpenGLControl1Paint(Sender : TObject);
    procedure Timer1Timer(Sender : TObject);
    procedure Timer2Timer(Sender : TObject);
  private
    GLBox : TOpenGLControl;
    angleX : single;
    angleY : single;
    angleZ : single;

    Game : TOurGame;
    World : TOurWorld;
    RenderArea : TRenderArea;
    Camera : TGlCamera;


  public

  end;

var
  MainForm : TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.OnAppIdle(Sender : TObject; var Done : boolean);
begin
  Done := False;
  GLBox.Invalidate;
end;

procedure TMainForm.OpenGLControl1Paint(Sender : TObject);
var
  t, dt : Qword;
begin            
  Application.ProcessMessages;
  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_FOG);

  if Game = nil then
  begin
    Game := TOurGame.Create;
    World := TOurWorld.Create(Game.Environment.GetCreator(0) as TBlockCreator, Game, TWorldGenerator.Create(50000));
    writeln('Generating world');
    RenderArea := World.AddRenderArea(0, 0, 0, 10);
    Camera := TGlCamera.Create;
    Camera.Position := Vector3(10, ChunkSize div 2 + 4, 10);
    writeln('Modeling world');
    RenderArea.IsVisibledPointFunction := @Camera.IsVisibled;
    World.Queues.AddMethod(@RenderArea.RepaintBlocks);
  end;

  Camera.Width := Width;
  Camera.Height := Height;

  t := GetMicroseconds;

  Camera.SetMatrix;
  Game.Textures.SelectTextures;
  RenderArea.DrawBlocks;

  dt := GetMicroseconds - t;
  writeln('T=', dt, #9, 1000000 / dt: 3: 2);

  GLBox.SwapBuffers;
end;

procedure TMainForm.Timer1Timer(Sender : TObject);
begin
  GLBox.Repaint;
end;

procedure TMainForm.Timer2Timer(Sender : TObject);
var
  c : TOurChunk;
  s, g : TElementCreator;
  t, dt : QWord;
  i, j, k : integer;
  side : TTextureMode;
begin
  if Game = nil then
    exit;
  s := Game.Environment.GetCreator(Game.Environment.GetID('stone'));
  g := Game.Environment.GetCreator(Game.Environment.GetID('glowstone'));
  for i := -1 to 0 do
  begin
    c := World.GetChunk(0, 0, i);
    if c = nil then
      continue;
    if Timer2.tag = 0 then
    begin
      c.Blocks[8, ChunkSize div 2 + 2, 0] := nil;
      c.Blocks[8, ChunkSize div 2 + 2, 8] := nil;
      c.Blocks[0, ChunkSize div 2 + 2, 8] := nil;
      c.Blocks[0, ChunkSize div 2 + 2, 0] := nil;

      writeln('Removed');
    end
    else
    begin
      c.Blocks[8, ChunkSize div 2 + 2, 8] := g.CreateElement(0) as TBlock;
      c.Blocks[8, ChunkSize div 2 + 2, 0] := s.CreateElement(0) as TBlock;
      c.Blocks[0, ChunkSize div 2 + 2, 8] := g.CreateElement(0) as TBlock;
      c.Blocks[0, ChunkSize div 2 + 2, 0] := s.CreateElement(0) as TBlock;
      writeln('Added');
    end;
  end;

  if Timer2.tag = 0 then
    Timer2.tag := 1
  else
    Timer2.tag := 0;


  t := GetMicroseconds;
  dt := GetMicroseconds - t;
  writeln('Updating: ', dt / 1000: 2: 3, 'ms');

end;

procedure TMainForm.FormCreate(Sender : TObject);
begin
  writeln(Load_GL_VERSION_2_0);
  writeln(Load_GL_ARB_vertex_array_object(True));
  writeln(Load_GL_ARB_framebuffer_object(True));
  GLBox := TOpenGLControl.Create(Self);
  with GLBox do
  begin
    Name := 'GLBox';
    Align := alClient;
    Parent := Self;
    OnPaint := @OpenGLControl1Paint;
    OnKeyPress := @FormKeyPress;
    AutoResizeViewport := True;
  end;

  Writeln('Available memory: ', GetMemInfo.MemAvailable);

  angleX := 0;
  angleY := 0;
  angleZ := 0;

  WriteLn(GetCurrentDir);

  Game := nil;

end;

procedure TMainForm.FormDestroy(Sender : TObject);
begin
  RenderArea.Free;
  Camera.Free;
  World.Free;
  Game.Free;
  GlBox.Free;
end;

procedure TMainForm.FormKeyPress(Sender : TObject; var Key : char);
var
  v : TVector3;
  c : TOurChunk;
begin
  if key = 'j' then
    angleY := angleY + pi / 150;
  if key = 'l' then
    angleY := angleY - pi / 150;
  if key = 'i' then
    angleX := angleX + pi / 150;
  if key = 'k' then
    angleX := angleX - pi / 150;
  if key = 'u' then
    angleZ := angleZ + pi / 150;
  if key = 'o' then
    angleZ := angleZ - pi / 150;

  if key = 'w' then
    Camera.Position := Camera.Position + FlatVector(Camera.ForwardVector,
      [axisX, axisZ]) * 0.4;
  if key = 's' then
    Camera.Position := Camera.Position + FlatVector(Camera.BackVector,
      [axisX, axisZ]) * 0.4;
  if key = ' ' then
    Camera.Position := Camera.Position + FlatVector(Camera.UpVector, [axisY]) * 0.4;
  if key = 'x' then
    Camera.Position := Camera.Position + FlatVector(Camera.DownVector, [axisY]) * 0.4;
  if key = 'a' then
    Camera.Position := Camera.Position + FlatVector(Camera.LeftVector,
      [axisX, axisZ]) * 0.4;
  if key = 'd' then
    Camera.Position := Camera.Position + FlatVector(Camera.RightVector,
      [axisX, axisZ]) * 0.4;

  if key = 'c' then
  begin
    c := World.GetChunkFromBlockCoors(floor(Camera.Position[axisX]), floor(Camera.Position[axisY]), floor(Camera.Position[axisZ]));
    if c <> nil then
      c.ForceUpdateModelLight;
  end;

  if key = 'v' then
  begin
    c := World.GetChunkFromBlockCoors(floor(Camera.Position[axisX]), floor(Camera.Position[axisY]), floor(Camera.Position[axisZ]));
    if c <> nil then
    begin
      c.RelightArea(0, 0, 0, ChunkSize-1, ChunkSize-1, ChunkSize-1);
    end;
  end;

  if key = 'm' then
  begin
    v := Camera.Position + Camera.ForwardVector*4;
    World.SetBlock(floor(v[axisX]), floor(v[axisY]), floor(v[axisZ]), Game.Environment.GetCreator(Game.Environment.GetID('stone')).CreateElement(0) as TBlock);
  end;
  if key = 'n' then
  begin
    v := Camera.Position + Camera.ForwardVector*4;
    World.SetBlock(floor(v[axisX]), floor(v[axisY]), floor(v[axisZ]), Game.Environment.GetCreator(0).CreateElement(0) as TBlock);
  end;

  RenderArea.SetPosition(IntVector3(floor(Camera.Position[axisX] / ChunkSize),
    floor(Camera.Position[axisY] / ChunkSize), floor(Camera.Position[axisZ] / ChunkSize)));

  if RenderArea = nil then
    exit;
  Camera.SetRotation(angleX, angleY, angleZ);
end;


end.
