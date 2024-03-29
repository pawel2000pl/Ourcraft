unit Models;

{$mode objfpc}{$H+}{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, math, CalcUtils, UniversalImage, Sorts,
  GLext, gl, strutils, Locker, LightTypes, TextureMode;

type

  TLightedSide = array[0..3] of TColor3f; //saved with conversion

  TTexturedCuboid = record
     Corners : TCubeCorners;
     Textures : TCubeTextureCorners;
  end;

  TTextureRect = record //lokalizacja tekstury w zlepku tekstur
    Name : ansistring;
    Left, Right, Top, Bottom : Single;
  end;
  PTextureRect = ^TTextureRect;

  { TDarkModel }

  TDarkModel = record
    AddCount : PtrUInt;
    WallCorners : array of TRectangleCorners;
    TextureCorners : array of TTextureCorners;
    TextureRects : array of PTextureRect;

    class function Empty : TDarkModel; static;
    procedure AddWall(const _WallCorners : TRectangleCorners; const _TextureCorners : TTextureCorners; const _TextureRect : PTextureRect);
  end;

  { TTextureRectSorter }

  TTextureRectSorter = class(specialize TStaticSort<TTextureRect>)
  {$if (FPC_VERSION >= 3) and (FPC_RELEASE >= 2)}
  type
      TValue = TTextureRect;
  {$ENDIF}
  public
    class function Compare(const a, b : TValue) : integer; override;
  end;

  { TTextureRectSearcher }

  TTextureRectSearcher = class(specialize TStaticBSearch<TTextureRect, ansistring>)
  {$if (FPC_VERSION >= 3) and (FPC_RELEASE >= 2)}
  type
      TValue = TTextureRect;
      TKey = AnsiString;
  {$ENDIF}
  public
    class function Compare(const a : TValue; const b : TKey) : integer; override;
  end;

  { TTextureManager }

  TTextureManager = class
  private
    fTextureID : GLuint;
    fTextures : array of TTextureRect;
  public
    property TextureID : GLuint read fTextureID;
    procedure SelectTextures;
    function GetTexture(const Name : ansistring) : PTextureRect;
    constructor Create(const List : ansistring =
      'compiled-resources/RenderedTextures.txt';
      const RenderedTextures : ansistring = 'compiled-resources/RenderedTextures.png';
      const UseMipmaps : boolean = true);
    destructor Destroy; override;
  end;

  { TVertexModel }

  TVertexModel = class
  const
    BufferedCount = 32;
    cfNone = GL_NONE;
    cfFrontAndBack = GL_FRONT_AND_BACK;
    cfBack = GL_BACK;
    cfFront = GL_FRONT;
  private
    fCullFace: GLenum;
    fVertex : array of TVector3;
    fColor : array of TColor3b;
    fTexture : array of TTexture2d;
    fCount : integer;
    fRealCount : integer;
    Editing : TLocker;
    class var PrevCullFace : GLenum;
    function GetColor(const index : integer) : TColor3b; inline;
    procedure SetColor(const index : integer; const AValue: TColor3b); inline;
    function GetColorPtr: Pointer; inline;
    function GetTexture(const index : integer) : TTexture2d; inline;
    function GetTexturePtr: Pointer; inline;
    function GetVertex(const index : integer) : TVector3; inline;
    function GetVertexPtr: Pointer; inline;
    procedure SetCullFace(AValue: GLenum);
    procedure SetTexture(const index : integer; const AValue: TTexture2d); inline;
    procedure SetVertex(const index : integer; const AValue: TVector3); inline;
    procedure UpdateLength(const OverSize : boolean = True);
  public
    procedure Lock;
    procedure Unlock;
    property CullFace : GLenum read fCullFace write SetCullFace;

    property Count : integer read fCount;

    //functions do not check range
    property Color[const index : integer] : TColor3b read GetColor write SetColor;
    property Vertex[const index : integer] : TVector3 read GetVertex write SetVertex;
    property Texture[const index : integer] : TTexture2d read GetTexture write SetTexture;

    property ColorPtr : Pointer read GetColorPtr;
    property VertexPtr : Pointer read GetVertexPtr;
    property TexturePtr : Pointer read GetTexturePtr;

    class procedure BeginDraw; static; inline;
    class procedure EndDraw; static; inline;
    procedure JustDraw; inline;
    procedure Draw; inline;

    function Allocated : Integer;

    procedure Rotate(const RotationMatrix : TMatrix3x3);
    procedure Move(const Vector : TVector3);

    procedure AddVertex(const AVertex : TVector3; const ALight : TRealLight; const ATexture : TTexture2d; const Tex: PTextureRect);

    procedure AddWall(const Position : TVector3;
      const WallCornersCoords : TRectangleCorners; TextureCorners : TTextureCorners;
      const Tex : PTextureRect; const LightLevel : TLight); overload;

    procedure AddWall(const Position : TVector3;
      const WallCornersCoords : TRectangleCorners; TextureCorners : TTextureCorners;
      const Tex : PTextureRect; const LightLevel : TRealLight); overload;

    procedure AddWall(const Position : TVector3;
      const WallCornersCoords : TRectangleCorners; TextureCorners : TTextureCorners;
      const Tex : PTextureRect; const LightLevel : TLightedSide); overload;

    procedure AddCuboid(const Position : TVector3; const Cuboid : TTexturedCuboid;
      const Tex : PTextureRect; const LightLevel : TLight);

    procedure AddVertexModel(Model : TVertexModel);
    procedure AddVertexModelAndFree(Model : TVertexModel); //warning: this will not set to nil!

    procedure Clear;

    constructor Create;
    class constructor Create;
    destructor Destroy; override;
  end;
                              
procedure MakeFog(const fog_Start, fog_end : Integer; const Color : TColor3f);  
function LightLevelToColor3f(const Level : integer) : TColor3f;
operator := (const a : TColor3f) : TColor3b; inline;
operator := (const a : TColor3b) : TColor3f; inline;
function SingleToByte(const s : Single) : byte; inline;

implementation

procedure MakeFog(const fog_Start, fog_end : Integer; const Color : TColor3f);
begin
  glFogfv( GL_FOG_COLOR, @Color);
  glFogf(GL_FOG_DENSITY, 1);
  glFogf( GL_FOG_MODE, GL_LINEAR);

  glFogf( GL_FOG_START, fog_Start);
  glFogf( GL_FOG_END, fog_end);
end;

function SingleToByte(const s: Single): byte;
begin
  if s >= 1 then
    Result := 255
    else if s <= 0 then
    Result := 0
    else
    Result := trunc(255*s);
end;

operator:=(const a: TColor3f): TColor3b;
begin
  Result.r:=SingleToByte(a[lcRed]);
  Result.g:=SingleToByte(a[lcGreen]);
  Result.b:=SingleToByte(a[lcBlue]);
end;

operator:=(const a: TColor3b): TColor3f;
begin
  Result[lcRed] := a.r/255;
  Result[lcGreen] := a.g/255;
  Result[lcBlue] := a.b/255;
end;

function LightLevelToColor3f(const Level : integer) : TColor3f; inline;
var
  s : single;
begin
  s := LightLevelToFloat(Level);
  Result[lcRed] := s;
  Result[lcGreen] := s;
  Result[lcBlue] := s;
end;

{ TDarkModel }

class function TDarkModel.Empty: TDarkModel;
begin
  Result.AddCount:=0;
  Result.WallCorners := [];
  Result.TextureCorners := [];
  Result.TextureRects := [];
end;

procedure TDarkModel.AddWall(const _WallCorners: TRectangleCorners;
  const _TextureCorners: TTextureCorners; const _TextureRect: PTextureRect);
begin
  Insert(_WallCorners, WallCorners, High(WallCorners));
  Insert(_TextureCorners, TextureCorners, High(TextureCorners));
  Insert(_TextureRect, TextureRects, High(TextureRects));
  Inc(AddCount);
end;

{ TVertexModel }

function TVertexModel.GetColor(const index: integer): TColor3b;
begin
  Result := fColor[index];
end;

procedure TVertexModel.SetColor(const index: integer; const AValue: TColor3b);
begin
  fColor[Index] := AValue;
end;

function TVertexModel.GetColorPtr: Pointer;
begin
  Result := @fColor[0];
end;

function TVertexModel.GetTexture(const index: integer): TTexture2d;
begin
  Result := fTexture[index];
end;

function TVertexModel.GetTexturePtr: Pointer;
begin
  Result := @fTexture[0];
end;

function TVertexModel.GetVertex(const index: integer): TVector3;
begin
  Result := fVertex[index];
end;

function TVertexModel.GetVertexPtr: Pointer;
begin
  Result := @fVertex[0];
end;

procedure TVertexModel.SetCullFace(AValue: GLenum);
begin
  if fCullFace=AValue then Exit;
  Assert((fCullFace = GL_BACK) or (fCullFace = GL_FRONT) or (fCullFace = GL_FRONT_AND_BACK));
  fCullFace:=AValue;
end;

procedure TVertexModel.SetTexture(const index: integer; const AValue: TTexture2d
  );
begin
  fTexture[index] := AValue;
end;

procedure TVertexModel.SetVertex(const index: integer; const AValue: TVector3);
begin
  fVertex[Index] := AValue;
end;

procedure TVertexModel.UpdateLength(const OverSize : boolean);
begin
  if (fCount < fRealCount) and OverSize then
    exit;

  if not OverSize then
    fRealCount := fCount
  else
    fRealCount += fCount + BufferedCount - (fCount mod BufferedCount);

  setlength(fVertex, fRealCount);
  setlength(fColor, fRealCount);
  setlength(fTexture, fRealCount);
end;

procedure TVertexModel.Lock;
begin
  Editing.Lock;
end;

procedure TVertexModel.Unlock;
begin
  Editing.Unlock;
end;

class procedure TVertexModel.BeginDraw;
begin
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_COLOR_ARRAY);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
end;

class procedure TVertexModel.EndDraw;
begin
  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_COLOR_ARRAY);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
end;

procedure TVertexModel.JustDraw;
begin
  Editing.Lock;
  try   
    if fCount = 0 then
      exit;
    glVertexPointer(3, GL_DOUBLE, 0, @fVertex[0]);
    glColorPointer(3, GL_UNSIGNED_BYTE, 0, @fColor[0]);
    glTexCoordPointer(2, GL_FLOAT, 0, @fTexture[0]);

    if PrevCullFace <> fCullFace then
    begin                       
        PrevCullFace:=fCullFace;
        glCullFace(fCullFace);
    end;
    glDrawArrays(GL_TRIANGLES, 0, fCount);
  finally
    Editing.Unlock;
  end;
end;

procedure TVertexModel.Draw;
begin
  BeginDraw;
  JustDraw;
  EndDraw;
end;

function TVertexModel.Allocated: Integer;
begin
  if fCount <= 0 then
    Result := 0
    else
    Result := fCount * (sizeof(fColor[0]) + sizeof(fVertex[0]) + sizeof(fTexture[0]));
end;

procedure TVertexModel.Rotate(const RotationMatrix: TMatrix3x3);
var
  i : Integer;
begin
  Lock;
  try
    for i := 0 to Count-1 do
       Vertex[i] := RotationMatrix * Vertex[i];
  finally
    Unlock;
  end;
end;

procedure TVertexModel.Move(const Vector: TVector3);
var
  i : Integer;
begin
  Lock;
  try
    for i := 0 to Count-1 do
       Vertex[i] := Vertex[i] + Vector;
  finally
    Unlock;
  end;
end;

procedure TVertexModel.AddVertex(const AVertex: TVector3;
  const ALight: TRealLight; const ATexture: TTexture2d; const Tex: PTextureRect);
begin
    Inc(fCount, 1);
    UpdateLength; 
    fTexture[fCount-1, axisX] := ATexture[axisX] * (Tex^.Right - Tex^.Left) + Tex^.Left;
    fTexture[fCount-1, axisY] := ATexture[axisY] * (Tex^.Bottom - Tex^.Top) + Tex^.Top;
    fVertex[fCount-1] :=AVertex;
    fColor[fCount-1].r := SingleToByte(ALight[lcRed]);
    fColor[fCount-1].g := SingleToByte(ALight[lcGreen]);
    fColor[fCount-1].b := SingleToByte(ALight[lcBlue]);
end;

procedure TVertexModel.AddWall(const Position: TVector3; const WallCornersCoords: TRectangleCorners; TextureCorners: TTextureCorners; const Tex: PTextureRect; const LightLevel: TLight);
begin
  AddWall(Position, WallCornersCoords, TextureCorners, Tex, LightLevelToFloat(LightLevel));
end;

procedure TVertexModel.AddWall(const Position: TVector3;
  const WallCornersCoords: TRectangleCorners; TextureCorners: TTextureCorners;
  const Tex: PTextureRect; const LightLevel: TRealLight);
var
  side : TLightedSide;
begin
  side[0][lcRed] := LightLevel[lcRed];
  side[0][lcGreen] := LightLevel[lcGreen];
  side[0][lcBlue]:= LightLevel[lcBlue];

  side[1] := side[0];
  side[2] := side[0];
  side[3] := side[0];

  AddWall(Position, WallCornersCoords, TextureCorners, Tex, side);
end;

procedure TVertexModel.AddWall(const Position: TVector3;
  const WallCornersCoords: TRectangleCorners; TextureCorners: TTextureCorners;
  const Tex: PTextureRect; const LightLevel: TLightedSide);
const
  sides : array[0..5] of Integer = (0, 1, 2, 2, 3, 0);
var
  i, j : integer;
begin
    j := fCount;
    Inc(fCount, 6);
    UpdateLength;

    for i := Low(sides) to High(sides) do
    begin
      fTexture[j, axisX] := TextureCorners[sides[i], axisX] * (Tex^.Right - Tex^.Left) + Tex^.Left;
      fTexture[j, axisY] := TextureCorners[sides[i], axisY] * (Tex^.Bottom - Tex^.Top) + Tex^.Top;
      fVertex[j] := WallCornersCoords[sides[i]] + Position;
      fColor[j].r := SingleToByte(LightLevel[sides[i]][lcRed]);
      fColor[j].g := SingleToByte(LightLevel[sides[i]][lcGreen]);
      fColor[j].b := SingleToByte(LightLevel[sides[i]][lcBlue]);
      Inc(j);
    end;
end;

procedure TVertexModel.AddCuboid(const Position: TVector3;
  const Cuboid: TTexturedCuboid; const Tex: PTextureRect;
  const LightLevel: TLight);
var
  side : TTextureMode;
begin
    for side := low(TTextureMode) to High(TTextureMode) do
      AddWall(Position, Cuboid.Corners[side], Cuboid.Textures[side], tex, LightLevel);
end;

procedure TVertexModel.AddVertexModel(Model : TVertexModel);
var
  i, j : integer;
begin
    j := fCount;
    Inc(fCount, Model.Count);
    UpdateLength;
    for i := 0 to Model.Count - 1 do
    begin
      fColor[j] := Model.Color[i];
      fVertex[j] := Model.Vertex[i];
      fTexture[j] := Model.Texture[i];
      Inc(j);
    end;
end;

procedure TVertexModel.AddVertexModelAndFree(Model: TVertexModel);
begin
  AddVertexModel(Model);
  Model.Free;
end;

procedure TVertexModel.Clear;
begin
    fCount := 0;
    fRealCount := 0;
    setlength(fVertex, fRealCount);
    setlength(fColor, fRealCount);
    setlength(fTexture, fRealCount);
end;

constructor TVertexModel.Create;
begin
  Editing := TLocker.Create;
  fCullFace := cfNone;
  Clear;
end;

class constructor TVertexModel.Create;
begin
  PrevCullFace := 1;
end;

destructor TVertexModel.Destroy;
begin
  Clear;
  Editing.Free;
  inherited Destroy;
end;

{ TTextureRectSearcher }

class function TTextureRectSearcher.Compare(const a : TValue; const b : TKey) : integer;
begin
  Result := CompareStr(a.Name, b);
end;

{ TTextureRectSorter }

class function TTextureRectSorter.Compare(const a, b : TValue) : integer;
begin
  Result := CompareStr(a.Name, b.Name);
end;

{ TTextureManager }

procedure TTextureManager.SelectTextures;
begin
  glBindTexture(GL_TEXTURE_2D, fTextureID);
end;

function TTextureManager.GetTexture(const Name : ansistring) : PTextureRect;
var
  i : integer;
begin
  i := TTextureRectSearcher.BSearch(fTextures, UpperCase(Name));
  if i < 0 then
    Result := nil
  else
    Result := @fTextures[i];
end;

constructor TTextureManager.Create(const List : ansistring;
  const RenderedTextures : ansistring; const UseMipmaps : boolean);
var
  Image : TUniversalImage;
  Data : Pointer;
  i, c : integer;
  SL : TStringList;
  s, tmp, ext : ansistring;
begin
  inherited Create;
  i := glGetError();
  if i <> 0 then
    writeln('There were some errors (OpenGL) before: ', i);
  glEnable(GL_TEXTURE_2D);
  glGenTextures(1, @fTextureID);
  glBindTexture(GL_TEXTURE_2D, fTextureID);

  Image := TUniversalImage.CreateEmpty;
  Image.LoadFromFile(RenderedTextures);

  Data := Image.GetGLBuffer16;

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA16, Image.Width, Image.Height,
    0, GL_RGBA, GL_UNSIGNED_SHORT, Data);
  FreeMem(Data);

  if UseMipmaps then
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  end
  else
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST_MIPMAP_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_BASE_LEVEL, 0);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 5);
    glGenerateMipmap(GL_TEXTURE_2D);
  end;

  SL := TStringList.Create;
  SL.LoadFromFile(List);

  c := SL.Count;
  setlength(fTextures, c);

  for i := 0 to c - 1 do
  begin
    s := SL[i];
    tmp := Copy2SymbDel(s, #9);
    fTextures[i].Left := (StrToIntDef(tmp, 0)) / Image.Width;
    tmp := Copy2SymbDel(s, #9);
    fTextures[i].Top := (StrToIntDef(tmp, 0)) / Image.Height;
    tmp := Copy2SymbDel(s, #9);
    fTextures[i].Right := (StrToIntDef(tmp, Image.Width)) / Image.Width;
    tmp := Copy2SymbDel(s, #9);
    fTextures[i].Bottom := (StrToIntDef(tmp, Image.Height)) / Image.Height;
    Delete(s, 1, length('Textures/'));

    ext := ExtractFileExt(s);
    Delete(s, length(s) - length(ext) + 1, length(ext));
    fTextures[i].Name := UpperCase(s);
  end;

  writeln('Loading textures: ErrorCode: ', glGetError);

  TTextureRectSorter.InsertCombF(fTextures, 0, c - 1);

  Image.Free;
  SL.Free;
end;

destructor TTextureManager.Destroy;
var
  i : integer;
begin
  glDeleteTextures(1, @fTextureID);
  for i := 0 to Length(fTextures) - 1 do
    fTextures[i].Name := '';
  SetLength(fTextures, 0);
  inherited Destroy;
end;

end.
