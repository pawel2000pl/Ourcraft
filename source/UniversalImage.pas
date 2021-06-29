{
  Copyright (C) 2020 Paweł Bielecki pawelek24@op.pl / pbielecki2000@gmail.com

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}

unit UniversalImage;
              
{$mode objfpc}

interface

uses
  SysUtils, Classes, Math, FpImage, fpreadbmp, fpwritebmp,
  fpreadjpeg, fpwritejpeg, fpreadpng, fpwritepng, fpreadpnm, fpwritepnm,
  fpreadtga, fpwritetga, fpreadtiff, fpwritetiff, fpreadxpm, fpwritexpm,
  fpreadpcx, fpwritepcx, FPImgCanv, matrix;

type
  TPoint = record
    X, Y : integer;
  end;

  TDrawFunction = function(const a, b : TFPColor) : TFPColor;
    
  { TUniversalImage }

  TUniversalImage = class(TFPCustomImage)
  protected
    FData : array of array of TFPColor;
    FCanvas : TFPImageCanvas;

    function GetReader(const Ext : ansistring) : TFPCustomImageReader;
    function GetWriter(const Ext : ansistring) : TFPCustomImageWriter;
    function GetInternalColor(x, y : integer) : TFPColor; override;
    procedure SetInternalColor(x, y : integer; const Value : TFPColor); override;
    function GetInternalPixel(x, y : integer) : integer; override; //Ignores Palette
    procedure SetInternalPixel(x, y : integer; Value : integer); override;
  public
    property Canvas : TFPImageCanvas read FCanvas;
  
    //need FreeMem()
    function GetGLBuffer : PLongWord; //RGBA
    function GetGLBuffer16 : PQWord;  //RGBA16
    function GetGLBuffer(const RGBA16 : boolean) : Pointer; overload;

    procedure SaveToPixelBuffer(const buf : Pointer);  //RGBA
    procedure SaveToPixelBuffer16(const buf : Pointer); //RGBA16
    procedure LoadFromPixelBuffer(const buf : Pointer);  //RGBA
    procedure LoadFromPixelBuffer16(const buf : Pointer); //RGBA16

    function CreateMipmap(const Level : integer) : TUniversalImage;

    property DirectColor[x, y : integer] : TFPColor
      read GetInternalColor write SetInternalColor; default;
    procedure SaveToFile(const FileName : ansistring); overload;
    procedure SaveToFile(const FileName : ansistring; const UseAlpha : Boolean); overload; //only PNG 
    procedure SaveToFile(const FileName : ansistring; const Quality : Integer); overload; //only JPG

    procedure LoadFromFile(const FileName : ansistring); overload;
    constructor CreateEmpty; virtual;
    constructor Create(AWidth, AHeight : integer); override;
    constructor CreateSubImage(Image : TUniversalImage; const Left, Top, Right, Bottom : Integer); virtual;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight : integer); override;
    procedure Draw(const PositionX, PositionY : integer; Img : TUniversalImage; const Transparency : double = 0; DrawFunction : TDrawFunction = nil);
  end;

  { TUniversalTransformationImage }

  //warning: write only without matrix (UseMatrix := False)
  TUniversalTransformationImage = class(TUniversalImage)
  type
    TTransformationMatrix = Tmatrix3_double;
  private
    FMatrix : TTransformationMatrix;
    FDefaultColor : TFPColor;
    FUseMatrix: Boolean;
    procedure Init;
  protected
    function GetPointerForFPColor(const x, y : Integer; const def : PFPColor = nil) : PFPColor; inline;
    function GetInternalColor(x, y : integer) : TFPColor; override;
    procedure SetInternalColor(x, y : integer; const Value : TFPColor); override;
    function GetInternalPixel(x, y : integer) : integer; override; //Ignores Palette
    procedure SetInternalPixel(x, y : integer; Value : integer); override;
  public
    property Matrix : TTransformationMatrix read FMatrix write FMatrix;
    property DefaultColor : TFPColor read FDefaultColor write FDefaultColor;
    property UseMatrix : Boolean read FUseMatrix write FUseMatrix;

    procedure MultipleMatrixLeft(const LeftMatrix : TTransformationMatrix); //Matrix := LeftMatrix * Matrix
    procedure MultipleMatrixRight(const RightMatrix : TTransformationMatrix); //Matrix := Matrix * RightMatrix

    class function RotationMatrix(const angle : Double) : TTransformationMatrix; static; inline;
    class function TranslationMatrix(const x, y : Double) : TTransformationMatrix; static; inline;
    class function ScaleMatrix(const kx, ky : Double) : TTransformationMatrix; static; inline;
    class function MirrorXMatrix : TTransformationMatrix; static; inline;
    class function MirrorYMatrix : TTransformationMatrix; static; inline;
    class function InclinationXMatrix(const a : Double) : TTransformationMatrix; static; inline;
    class function InclinationYMatrix(const a : Double) : TTransformationMatrix; static; inline;

    constructor CreateEmpty; override;
    constructor Create(AWidth, AHeight : integer); override;
    constructor CreateSubImage(Image : TUniversalImage; const Left, Top, Right, Bottom : Integer); override;
    destructor Destroy; override;
  end;

function MixColors(const CanvColor, DrawColor : TFPColor; const Transparency : double = 0) : TFPColor; inline;
function Point(const X, Y : integer) : TPoint; inline;
function FpColor(red, green, blue, alpha : word) : TFPColor; inline;

operator = (const a, b : TFPColor) : boolean; inline;

implementation

function PostInc(var i : Integer) : Integer;
begin
  Result := i;
  inc(i);
end;

operator = (const a, b : TFPColor) : boolean; inline;
begin
  Result := (a.alpha = b.alpha) and (a.red = b.red) and (a.green = b.green) and
    (a.blue = b.blue);
end;

function NormalDraw(const {%H-}a, b : TFPColor) : TFPColor;
begin
    Result := b;
end;

function MixColors(const CanvColor, DrawColor : TFPColor;
  const Transparency : double = 0) : TFPColor; inline;
var
  v : double;
begin
  v := (1 - Transparency) * DrawColor.alpha / 65535;
  Result.alpha := floor((1 - (1 - CanvColor.alpha / 65535) * (1 - v)) * 65535);
  Result.red := floor(DrawColor.red * v + CanvColor.red * (1 - v));
  Result.green := floor(DrawColor.green * v + CanvColor.green * (1 - v));
  Result.blue := floor(DrawColor.blue * v + CanvColor.blue * (1 - v));
end;

function Point(const X, Y : integer) : TPoint; inline;
begin
  Result.X := X;
  Result.Y := Y;
end;

function FpColor(red, green, blue, alpha : word) : TFPColor; inline;
begin
  Result.red := red;
  Result.green := green;
  Result.blue := blue;
  Result.alpha := alpha;
end;

{ TUniversalTransformationImage }

procedure TUniversalTransformationImage.Init;
begin
  FMatrix.init_identity;
  FUseMatrix := True;
  FDefaultColor := FpColor(0, 0, 0, 0);
end;

function TUniversalTransformationImage.GetPointerForFPColor(const x, y: Integer; const def: PFPColor): PFPColor;
var
  v : Tvector3_double;
  newX, newY : Integer;
begin
  if not FUseMatrix then
     Exit(@FData[x, y]);
  v.init(x, y, 1);
  v := FMatrix * v;
  newX := round(v.data[0]);
  newY := round(v.data[1]);
  if (0 <= newX) and (Width < newX) and (0 <= newY) and (Height < newY) then
     Exit(@FData[newX, newY]);
  Exit(@def);
end;

function TUniversalTransformationImage.GetInternalColor(x, y: integer): TFPColor;
begin
  Exit(GetPointerForFPColor(x, y, @FDefaultColor)^);
end;

procedure TUniversalTransformationImage.SetInternalColor(x, y: integer; const Value: TFPColor);
var
  c : PFPColor;
begin
  c := GetPointerForFPColor(x, y, nil);
  if c = nil then
     Exit;
  c^ := Value;
end;

function TUniversalTransformationImage.GetInternalPixel(x, y: integer): integer;
var
  c : PFPColor;
begin
  c := GetPointerForFPColor(x, y, @FDefaultColor);
  Result := c^.red shr 8 + c^.blue shr 8 shl 8 + c^.green shr 8 shl 16 + c^.alpha shr 8 shl 24;
end;

procedure TUniversalTransformationImage.SetInternalPixel(x, y: integer;Value: integer);
var
  c : PFPColor;
begin
  c := GetPointerForFPColor(x, y, nil);
  if c = nil then
     Exit;
  c^.red := (Value and $FF) shl 8;
  c^.blue := (Value and $FF00) shl 8;
  c^.green := (Value and $FF0000) shl 8;
  c^.alpha := (Value and $FF000000) shl 8;
end;

procedure TUniversalTransformationImage.MultipleMatrixLeft(const LeftMatrix: TTransformationMatrix);
begin
  Matrix := LeftMatrix * Matrix;
end;

procedure TUniversalTransformationImage.MultipleMatrixRight(const RightMatrix: TTransformationMatrix);
begin
  Matrix := Matrix * RightMatrix;
end;

class function TUniversalTransformationImage.RotationMatrix(const angle: Double): TTransformationMatrix;
var
  c, s : Double;
begin
  c := cos(angle);
  s := sin(angle);
  Result.init_identity;
  Result.data[0, 0] := c;
  Result.data[0, 1] := -s;
  Result.data[1, 0] := s;
  Result.data[1, 1] := c;
end;

class function TUniversalTransformationImage.TranslationMatrix(const x, y: Double): TTransformationMatrix;
begin
  Result.init_identity;
  Result.data[0, 2] := x;
  Result.data[1, 2] := y;
end;

class function TUniversalTransformationImage.ScaleMatrix(const kx, ky: Double): TTransformationMatrix;
begin
  Result.init_identity;
  Result.data[0, 0] := kx;
  Result.data[1, 1] := ky;
end;

class function TUniversalTransformationImage.MirrorXMatrix: TTransformationMatrix;
begin
  Result.init_identity;
  Result.data[0, 0] := -1;
end;

class function TUniversalTransformationImage.MirrorYMatrix: TTransformationMatrix;
begin
  Result.init_identity;
  Result.data[1, 1] := -1;
end;

class function TUniversalTransformationImage.InclinationXMatrix(const a: Double): TTransformationMatrix;
begin
  Result.init_identity;
  Result.data[0, 1] := a;
end;

class function TUniversalTransformationImage.InclinationYMatrix(const a: Double): TTransformationMatrix;
begin
  Result.init_identity;
  Result.data[1, 0] := a;
end;

constructor TUniversalTransformationImage.CreateEmpty;
begin
   Init;
   Inherited CreateEmpty;
end;

constructor TUniversalTransformationImage.Create(AWidth, AHeight: integer);
begin                 
  Init;
  inherited Create(AWidth, AHeight);
end;

constructor TUniversalTransformationImage.CreateSubImage(Image: TUniversalImage; const Left, Top, Right, Bottom: Integer);
begin
  Init;
  inherited CreateSubImage(Image, Left, Top, Right, Bottom);
end;

destructor TUniversalTransformationImage.Destroy;
begin
  inherited Destroy;
end;

function TUniversalImage.GetInternalColor(x, y : integer) : TFPColor;
begin
  Result := FData[x, y];
end;

procedure TUniversalImage.SetInternalColor(x, y : integer; const Value : TFPColor);
begin
  FData[x, y] := Value;
end;

function TUniversalImage.GetInternalPixel(x, y : integer) : integer;
begin
  Result :=
    FData[x, y].red shr 8 + FData[x, y].blue shr 8 shl 8 + FData[x, y].green shr
    8 shl 16 + FData[x, y].alpha shr 8 shl 24;
end;

procedure TUniversalImage.SetInternalPixel(x, y : integer; Value : integer);
begin
  FData[x, y].red := (Value and $FF) shl 8;
  FData[x, y].blue := (Value and $FF00) shl 8;
  FData[x, y].green := (Value and $FF0000) shl 8;
  FData[x, y].alpha := (Value and $FF000000) shl 8;
end;

function TUniversalImage.GetGLBuffer : PLongWord;
begin
  Result := AllocMem(Width * Height * sizeof(longword));
  SaveToPixelBuffer(Result);
end;

function TUniversalImage.GetGLBuffer16 : PQWord;
begin
  Result := AllocMem(Width * Height * sizeof(QWord));
  SaveToPixelBuffer16(Result);
end;

function TUniversalImage.GetGLBuffer(const RGBA16 : boolean) : Pointer;
begin
  if RGBA16 then
    Result := GetGLBuffer16
  else
    Result := GetGLBuffer;
end;

procedure TUniversalImage.SaveToPixelBuffer(const buf: Pointer);
var
  x, y, i : integer;
begin
  i := 0;
  for y := 0 to Height - 1 do
    for x := 0 to Width - 1 do
      PLongWord(buf)[PostInc(i)] :=
        (FData[x, y].red shr 8) or (FData[x, y].green shr 8 shl 8) or
        (FData[x, y].blue shr 8 shl 16) or (FData[x, y].alpha shr 8 shl 24);
end;

procedure TUniversalImage.SaveToPixelBuffer16(const buf: Pointer);
var
  x, y, i : integer;
begin
  i := 0;
  for y := 0 to Height - 1 do
    for x := 0 to Width - 1 do
      PQWord(buf)[PostInc(i)] := QWord(FData[x, y]);
end;

procedure TUniversalImage.LoadFromPixelBuffer(const buf: Pointer);
var
  x, y, i : integer;
begin
  i := 0;
  for y := 0 to Height - 1 do
    for x := 0 to Width - 1 do
    begin
      FData[x, y].red := (PLongWord(buf)[i] and $FF) shl 8;
      FData[x, y].green := (PLongWord(buf)[i] and $FF00);
      FData[x, y].Blue := (PLongWord(buf)[i] and $FF0000) shr 8;
      FData[x, y].Alpha := (PLongWord(buf)[i] and $FF000000) shr 16;
      Inc(i);
    end;
end;

procedure TUniversalImage.LoadFromPixelBuffer16(const buf: Pointer);
var
  x, y, i : integer;
begin
  i := 0;
  for y := 0 to Height - 1 do
    for x := 0 to Width - 1 do
      QWord(FData[x, y]) := PQWord(buf)[PostInc(i)];
end;

function TUniversalImage.CreateMipmap(const Level : integer) : TUniversalImage;
var
  w, h, x, y : integer;
begin
  w := Width shr Level;
  h := Height shr Level;

  if w * h = 0 then
  begin
    Result := nil;
    exit;
  end;

  Result := TUniversalImage.Create(w, h);
  for x := 0 to w - 1 do
    for y := 0 to h - 1 do
      Result.DirectColor[x, y] := FPColor(0, 0, 0, 0);
  for x := 0 to w - 1 do
    for y := 0 to h - 1 do
    begin
      Inc(Result.FData[x shr Level, y shr Level].alpha,
        DirectColor[x, y].alpha shr Level);
      Inc(Result.FData[x shr Level, y shr Level].red, DirectColor[x, y].red shr Level);
      Inc(Result.FData[x shr Level, y shr Level].green,
        DirectColor[x, y].green shr Level);
      Inc(Result.FData[x shr Level, y shr Level].blue,
        DirectColor[x, y].blue shr Level);
    end;
end;

function TUniversalImage.GetReader(const Ext : ansistring) : TFPCustomImageReader;
begin
  if SameText(Ext, '.bmp') then
    Result := TFPReaderBMP.Create
  else if SameText(Ext, '.jpeg') or SameText(Ext, '.jpg') then
    Result := TFPReaderJPEG.Create
  else if SameText(Ext, '.png') then
    Result := TFPReaderPNG.Create
  else if SameText(Ext, '.pnm') then
    Result := TFPReaderPNM.Create
  else if SameText(Ext, '.tga') then
    Result := TFPReaderTarga.Create
  else if SameText(Ext, '.tiff') then
    Result := TFPReaderTIFF.Create
  else if SameText(Ext, '.xpm') then
    Result := TFPReaderXPM.Create
  else if SameText(Ext, '.pcx') then
    Result := TFPReaderPCX.Create
  else
    Result := TFPReaderPNG.Create;
end;

function TUniversalImage.GetWriter(const Ext : ansistring) : TFPCustomImageWriter;
begin
  if SameText(Ext, '.bmp') then
    Result := TFPWriterBMP.Create
  else if SameText(Ext, '.jpeg') or SameText(Ext, '.jpg') then
    Result := TFPWriterJPEG.Create
  else if SameText(Ext, '.png') then
    Result := TFPWriterPNG.Create
  else if SameText(Ext, '.pnm') then
    Result := TFPWriterPNM.Create
  else if SameText(Ext, '.tga') then
    Result := TFPWriterTarga.Create
  else if SameText(Ext, '.tiff') then
    Result := TFPWriterTIFF.Create
  else if SameText(Ext, '.xpm') then
    Result := TFPWriterXPM.Create
  else if SameText(Ext, '.pcx') then
    Result := TFPWriterPCX.Create
  else
    Result := TFPWriterPNG.Create;
end;

procedure TUniversalImage.SaveToFile(const FileName : ansistring);
var
  Writer : TFPCustomImageWriter;
begin
  Writer := GetWriter(ExtractFileExt(FileName));
  if (Writer is TFPWriterPNG) then
    (Writer as TFPWriterPNG).UseAlpha := True;
  SaveToFile(FileName, Writer);
  Writer.Free;
end;

procedure TUniversalImage.SaveToFile(const FileName : ansistring; const UseAlpha : Boolean); overload; //only PNG
var
  Writer : TFPCustomImageWriter;
begin
  Writer := GetWriter(ExtractFileExt(FileName));
  if (Writer is TFPWriterPNG) then
    (Writer as TFPWriterPNG).UseAlpha := UseAlpha;
  SaveToFile(FileName, Writer);
  Writer.Free;
end;

procedure TUniversalImage.SaveToFile(const FileName : ansistring; const Quality : Integer); overload; //only JPG
var
  Writer : TFPCustomImageWriter;
begin
  Writer := GetWriter(ExtractFileExt(FileName));
  if (Writer is TFPWriterJPEG) then
    (Writer as TFPWriterJPEG).CompressionQuality := Quality;
  SaveToFile(FileName, Writer);
  Writer.Free;
end;

procedure TUniversalImage.LoadFromFile(const FileName : ansistring);
var
  Reader : TFPCustomImageReader;
begin
  Reader := GetReader(ExtractFileExt(FileName));
  LoadFromFile(FileName, Reader);
  Reader.Free;
end;

constructor TUniversalImage.CreateEmpty;
begin
  inherited Create(0, 0);
end;

constructor TUniversalImage.Create(AWidth, AHeight : integer);
begin
  setlength(FData, AWidth, AHeight);
  inherited Create(AWidth, AHeight);
  SetUsePalette(False);
  FCanvas := TFPImageCanvas.Create(Self);
end;

constructor TUniversalImage.CreateSubImage(Image : TUniversalImage; const Left, Top, Right, Bottom : Integer); 
var
    w, h : Integer;
begin
    w := Right-Left+1;
    h := Bottom-Top+1;
    SetLength(FData, w, h);
    inherited Create(w, h);
    SetUsePalette(False);
    FCanvas := TFPImageCanvas.Create(Self);
    Draw(-Left, -Top, Image);
end;

destructor TUniversalImage.Destroy;
begin
    FCanvas.Free;
    SetLength(FData, 0, 0);
    inherited Destroy;
end;

procedure TUniversalImage.SetSize(AWidth, AHeight : integer);
begin
  if (AWidth <> Width) or (AHeight <> AHeight) then
    setLength(FData, AWidth, AHeight);
  inherited;
end;

procedure TUniversalImage.Draw(const PositionX, PositionY : integer; Img : TUniversalImage; const Transparency : double = 0; DrawFunction : TDrawFunction = nil);
var
  x, y : integer;
begin
  if Img = nil then
      exit;
  if DrawFunction = nil then
      DrawFunction := @NormalDraw;
  for x := 0 to Img.Width - 1 do
    if (x + PositionX >= 0) and (x + PositionX < Width) then
      for y := 0 to Img.Height - 1 do
        if (y + PositionY >= 0) and (y + PositionY < Height) then
          FData[x + PositionX, y + PositionY] := MixColors(FData[x + PositionX, y + PositionY], DrawFunction(FData[x + PositionX, y + PositionY], Img.GetInternalColor(x, y)), Transparency);
end;

end.
