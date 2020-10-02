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
  SysUtils, Classes, Math, FPImage, DrawModes, fpreadbmp, fpwritebmp,
  fpreadjpeg, fpwritejpeg, fpreadpng, fpwritepng, fpreadpnm, fpwritepnm,
  fpreadtga, fpwritetga, fpreadtiff, fpwritetiff, fpreadxpm, fpwritexpm,
  fpreadpcx, fpwritepcx;

type
  TPoint = record
    X, Y : integer;
  end;

  { TUniversalImage }

  TUniversalImage = class(TFPCustomImage)
  protected
    FData : array of array of TFPColor;

    function GetReader(const Ext : ansistring) : TFPCustomImageReader;
    function GetWriter(const Ext : ansistring) : TFPCustomImageWriter;
    function GetInternalColor(x, y : integer) : TFPColor; override;
    procedure SetInternalColor(x, y : integer; const Value : TFPColor); override;
    function GetInternalPixel(x, y : integer) : integer; override; //Ignore Palette
    procedure SetInternalPixel(x, y : integer; Value : integer); override;
  public
    //need FreeMem()
    function GetGLBuffer : PLongWord; //RGBA
    function GetGLBuffer16 : PQWord;  //RGBA16
    function GetGLBuffer(const RGBA16 : boolean) : Pointer; overload;

    function CreateMipmap(const Level : integer) : TUniversalImage;

    property DirectColor[x, y : integer] : TFPColor
      read GetInternalColor write SetInternalColor; default;
    procedure SaveToFile(const FileName : ansistring); overload;
    procedure SaveToFile(const FileName : ansistring; const UseAlpha : Boolean); overload; //only PNG 
    procedure SaveToFile(const FileName : ansistring; const Quality : Integer); overload; //only JPG

    procedure LoadFromFile(const FileName : ansistring); overload;
    constructor CreateEmpty;
    constructor Create(AWidth, AHeight : integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight : integer); override;
    procedure Draw(const PositionX, PositionY : integer; Img : TUniversalImage;
      const Transparency : double = 0; const DrawMode : TDrawMode = dmNormal);
  end;

function MixColors(const CanvColor, DrawColor : TFPColor;
  const Transparency : double = 0) : TFPColor; inline;
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
var
  x, y, i : integer;
begin
  Result := AllocMem(Width * Height * sizeof(longword));
  i := 0;
  for y := 0 to Height - 1 do
    for x := 0 to Width - 1 do
      Result[PostInc(i)] :=
        (FData[x, y].red shr 8) or (FData[x, y].green shr 8 shl 8) or
        (FData[x, y].blue shr 8 shl 16) or (FData[x, y].alpha shr 8 shl 24);
end;

function TUniversalImage.GetGLBuffer16 : PQWord;
var
  x, y, i : integer;
begin
  Result := AllocMem(Width * Height * sizeof(QWord));
  i := 0;
  for y := 0 to Height - 1 do
    for x := 0 to Width - 1 do
      Result[PostInc(i)] := QWord(FData[x, y]);
end;

function TUniversalImage.GetGLBuffer(const RGBA16 : boolean) : Pointer;
begin
  if RGBA16 then
    Result := GetGLBuffer16
  else
    Result := GetGLBuffer;
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
var
  x, y : integer;
begin
  setlength(FData, AWidth);
  for x := 0 to AWidth - 1 do
  begin
    setlength(FData[x], AHeight);
    for y := 0 to AHeight - 1 do
      FData[x, y] := FPColor(0, 0, 0, 0);
  end;
  inherited Create(AWidth, AHeight);
  SetUsePalette(False);
end;

destructor TUniversalImage.Destroy;
begin
  SetLength(FData, 0, 0);
  inherited destroy;
end;

procedure TUniversalImage.SetSize(AWidth, AHeight : integer);
var
  x, y : integer;
begin
  if Width > AWidth then
  begin
    for x := AWidth to Width - 1 do
      setlength(FData[x], 0);
    setlength(FData, AWidth);
  end
  else if Width < AWidth then
  begin
    setlength(FData, AWidth);
    for x := Width to AWidth - 1 do
    begin
      setlength(FData[x], AHeight);
      for y := 0 to AHeight - 1 do
        FData[x, y] := FPColor(0, 0, 0, 0);
    end;
  end;
  if Height <> AHeight then
    for x := 0 to Width - 1 do
    begin
      setlength(FData[x], AHeight);
      if AHeight > Height then
        for y := Height to AHeight - 1 do
          FData[x, y] := fpColor(0, 0, 0, 0);
    end;
  inherited;
end;

procedure TUniversalImage.Draw(const PositionX, PositionY : integer;
  Img : TUniversalImage; const Transparency : double = 0;
  const DrawMode : TDrawMode = dmNormal);
var
  x, y : integer;
  foo : TDrawFunction;
begin
  if Img = nil then
    exit;
  foo := GetDrawFunction(DrawMode);
  for x := 0 to Img.Width - 1 do
    if (x + PositionX >= 0) and (x + PositionX < Width) then
      for y := 0 to Img.Height - 1 do
        if (y + PositionY >= 0) and (y + PositionY < Height) then
          FData[x + PositionX, y + PositionY] :=
            MixColors(FData[x + PositionX, y + PositionY],
            foo(FData[x + PositionX, y + PositionY], Img.FData[x, y]), Transparency);

end;

end.
