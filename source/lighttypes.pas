unit LightTypes;

{$mode objfpc}{$H+}
{$ModeSwitch typehelpers}

interface

const
  MAX_LIGHT_LEVEL = 255;//15;

type
  TLightColor = (lcRed, lcGreen, lcBlue);
  TLight = packed array[TLightColor] of Byte;
  TLongLight = array[TLightColor] of Integer;

  TColor3f = array[TLightColor] of Single;   
  TRealLight = TColor3f;

  TColor3b = packed record
    r, g, b : byte;
  end;

  { TLightHelper }

  TLightHelper = type helper for TLight
  private
    function GetBlue : byte;
    function GetGreen : byte;
    function GetRed : byte;

    procedure SetBlue(AValue : byte);
    procedure SetGreen(AValue : byte);
    procedure SetRed(AValue : byte);
  public
    property Red : byte read GetRed write SetRed;
    property Green : byte read GetGreen write SetGreen;
    property Blue : byte read GetBlue write SetBlue;
    function Value : integer;
    function MinValue : integer;
    function White : TLight;

    procedure RangeCheck;
    function RangeChecked : TLight;

    procedure Init(AValue : integer); overload;
    procedure Init(const RedValue, GreenValue, BlueValue : integer); overload;
  end;

  { TLongLightHelper }

  TLongLightHelper = type helper for TLongLight
  private
    function GetBlue : Integer;
    function GetGreen : Integer;
    function GetRed : Integer;

    procedure SetBlue(const AValue : Integer);
    procedure SetGreen(const AValue : Integer);
    procedure SetRed(const AValue : Integer);
  public
    property Red : Integer read GetRed write SetRed;
    property Green : Integer read GetGreen write SetGreen;
    property Blue : Integer read GetBlue write SetBlue;
    function Value : integer;
    function MinValue : integer;
    function White : TLongLight;

    procedure Init(const AValue : integer); overload;
    procedure Init(const RedValue, GreenValue, BlueValue : integer); overload;
  end;

operator +(const a, b : TLongLight) : TLongLight; inline;
operator -(const a, b : TLongLight) : TLongLight; inline;
operator +(const a : TLongLight; const b : integer) : TLongLight; inline;
operator +(const a : integer; const b : TLongLight) : TLongLight; inline;
operator -(const a : TLongLight; const b : integer) : TLongLight; inline;
operator > (const a, b : TLongLight) : boolean; inline;
operator < (const a, b : TLongLight) : boolean; inline;
operator = (const a, b : TLongLight) : boolean; inline;
operator <> (const a, b : TLongLight) : boolean; inline;
operator >= (const a, b : TLongLight) : boolean; inline;
operator <= (const a, b : TLongLight) : boolean; inline;

operator := (const a : TLongLight) : TLight;
operator := (const a : TLight) : TLongLight;

operator * (const a : TRealLight; const b : Double) : TRealLight;

procedure UpdateIfGreater(var a : TLongLight; const b : TLongLight); overload;
procedure UpdateIfLesser(var a : TLongLight; const b : TLongLight); overload;
procedure UpdateIfGreater(var a : TRealLight; const b : TRealLight); overload;
procedure UpdateIfLesser(var a : TRealLight; const b : TRealLight); overload;

function AsLight(const AValue : integer) : TLight; inline; overload;
function AsLight(const RedValue, GreenValue, BlueValue : integer) : TLight; inline; overload;

function Max(const a, b : TLongLight) : TLongLight; overload;
function Min(const a, b : TLongLight) : TLongLight; overload;

function Max(const a, b : TRealLight) : TRealLight; overload;
function Min(const a, b : TRealLight) : TRealLight; overload;

function LightMultiple(const Light : TLight; const k : Double) : TLight;
function ScaleLightChannels(const Light : TLight; const k : TRealLight) : TLight; overload;
function ScaleLightChannels(const Light : TRealLight; const k : TRealLight) : TRealLight; overload;

function LightLevelToFloat(const AverageLevel : Single) : Single; inline; overload;
function LightLevelToFloat(const Value : TLight) : TRealLight; overload;

const
  AsLightZero : TLight = (0, 0, 0);
  AsLightMax : TLight = (MAX_LIGHT_LEVEL, MAX_LIGHT_LEVEL, MAX_LIGHT_LEVEL);

  WarmSunLight : TRealLight = (0.98, 0.97, 0.95);

implementation

uses
  SysUtils, Math;

function LightLevelToFloat(const AverageLevel: Single): Single; inline;
begin
  Result := (AverageLevel/MAX_LIGHT_LEVEL + sqrt(MAX_LIGHT_LEVEL/8/((MAX_LIGHT_LEVEL+1)-AverageLevel) - 1/(MAX_LIGHT_LEVEL+1)) * (AverageLevel + 1) / (MAX_LIGHT_LEVEL+1) + 1/(MAX_LIGHT_LEVEL*1.4))/2;
end;

operator+(const a, b: TLongLight): TLongLight;
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
    Result[c] := integer(a[c]) + integer(b[c]);
end;

operator-(const a, b: TLongLight): TLongLight;
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
    Result[c] := max(integer(a[c]) - integer(b[c]), 0);
end;

operator+(const a: TLongLight; const b: integer): TLongLight;
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
    Result[c] := integer(a[c]) + integer(b);
end;

operator+(const a: integer; const b: TLongLight): TLongLight;
begin
  Result := b + a;
end;

operator-(const a: TLongLight; const b: integer): TLongLight;
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
    Result[c] := max(integer(a[c]) - integer(b), 0);
end;

operator>(const a, b: TLongLight): boolean;
begin
  Result := (a>=b) and ((a[lcRed] > b[lcRed]) or (a[lcGreen] > b[lcGreen]) or (a[lcBlue] > b[lcBlue]));
end;

operator<(const a, b: TLongLight): boolean;
begin
  Result := b>a;
end;

operator=(const a, b: TLongLight): boolean;
begin
  Result := (a[lcRed] = b[lcRed]) and (a[lcGreen] = b[lcGreen]) and (a[lcBlue] = b[lcBlue]);
end;

operator<>(const a, b: TLongLight): boolean;
begin
  Result := not (a=b);
end;

operator>=(const a, b: TLongLight): boolean;
begin
    Result := (a[lcRed] >= b[lcRed]) and (a[lcGreen] >= b[lcGreen]) and (a[lcBlue] >= b[lcBlue]);
end;

operator<=(const a, b: TLongLight): boolean;
begin
    Result := b>=a;
end;

function LightLevelToFloat(const Value: TLight): TRealLight;
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
      Result[c] := LightLevelToFloat(Value[c]);
end;

operator:=(const a: TLongLight): TLight;
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
      Result[c] := EnsureRange(a[c], 0, MAX_LIGHT_LEVEL);
end;

operator:=(const a: TLight): TLongLight;
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
      Result[c] := a[c];
end;

operator*(const a: TRealLight; const b: Double): TRealLight;
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
    Result[c] := a[c] * b;
end;

procedure UpdateIfGreater(var a: TLongLight; const b: TLongLight);
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
      if b[c] > a[c] then
         a[c] := b[c];
end;

procedure UpdateIfLesser(var a: TLongLight; const b: TLongLight);
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
      if b[c] < a[c] then
         a[c] := b[c];
end;

procedure UpdateIfGreater(var a: TRealLight; const b: TRealLight);
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
      if b[c] > a[c] then
         a[c] := b[c];
end;

procedure UpdateIfLesser(var a: TRealLight; const b: TRealLight);
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
      if b[c] < a[c] then
         a[c] := b[c];
end;

function AsLight(const AValue : integer) : TLight;
begin
  Result{%H-}.Init(AValue);
end;

function AsLight(const RedValue, GreenValue, BlueValue : integer) : TLight;
begin
  Result{%H-}.Init(RedValue, GreenValue, BlueValue);
end;

function Max(const a, b: TLongLight): TLongLight;
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
    Result[c] := max(a[c], b[c]);
end;

function Min(const a, b: TLongLight): TLongLight;
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
    Result[c] := min(a[c], b[c]);
end;

function Max(const a, b: TRealLight): TRealLight;
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
    Result[c] := max(a[c], b[c]);
end;

function Min(const a, b: TRealLight): TRealLight;
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
    Result[c] := min(a[c], b[c]);
end;

function LightMultiple(const Light: TLight; const k: Double): TLight;
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
    Result[c] := max(round(Light[c]*k), 0);
end;

function ScaleLightChannels(const Light: TLight; const k: TRealLight): TLight;
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
    Result[c] := EnsureRange(round(Light[c]*k[c]), 0, 255);
end;

function ScaleLightChannels(const Light: TRealLight; const k: TRealLight): TRealLight;
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
    Result[c] := EnsureRange(Light[c]*k[c], 0, 1);
end;

{ TLongLightHelper }

function TLongLightHelper.GetBlue: Integer;
begin
  Result := Self[lcBlue];
end;

function TLongLightHelper.GetGreen: Integer;
begin
  Result := Self[lcGreen];
end;

function TLongLightHelper.GetRed: Integer;
begin
  Result := Self[lcRed];
end;

procedure TLongLightHelper.SetBlue(const AValue: Integer);
begin
  Self[lcBlue] := AValue;
end;

procedure TLongLightHelper.SetGreen(const AValue: Integer);
begin
  Self[lcGreen] := AValue;
end;

procedure TLongLightHelper.SetRed(const AValue: Integer);
begin
  Self[lcRed] := AValue;
end;

function TLongLightHelper.Value: integer;
begin
  Result := max(Self[lcRed], max(Self[lcBlue], Self[lcGreen]));
end;

function TLongLightHelper.MinValue: integer;
begin
  Result := min(Self[lcRed], min(Self[lcBlue], Self[lcGreen]));
end;

function TLongLightHelper.White: TLongLight;
begin
  Result[lcRed] := Value;
  Result[lcGreen] := Result[lcRed];
  Result[lcBlue] := Result[lcRed];
end;

procedure TLongLightHelper.Init(const AValue: integer);
begin
  Self[lcRed] := AValue;
  Self[lcGreen] := AValue;
  Self[lcBlue] := AValue;
end;

procedure TLongLightHelper.Init(const RedValue, GreenValue, BlueValue: integer);
begin
  Self[lcRed] := RedValue;
  Self[lcGreen] := GreenValue;
  Self[lcBlue] := BlueValue;
end;

{ TLightHelper }

function TLightHelper.GetRed : byte;
begin
  Result := Self[lcRed];
end;

function TLightHelper.GetGreen : byte;
begin
  Result := Self[lcGreen];
end;

function TLightHelper.GetBlue : byte;
begin
  Result := Self[lcBlue];
end;

procedure TLightHelper.SetBlue(AValue : byte);
begin
  if Self[lcBlue] = AValue then
    Exit;
  Self[lcBlue] := AValue;
end;

procedure TLightHelper.SetGreen(AValue : byte);
begin
  if Self[lcGreen] = AValue then
    Exit;
  Self[lcGreen] := AValue;
end;

procedure TLightHelper.SetRed(AValue : byte);
begin
  if Self[lcRed] = AValue then
    Exit;
  Self[lcRed] := AValue;
end;

function TLightHelper.Value : integer;
begin
  Result := max(Self[lcRed], max(Self[lcGreen], Self[lcBlue]));
end;

function TLightHelper.MinValue: integer;
begin
  Result := min(Self[lcRed], min(Self[lcGreen], Self[lcBlue]));
end;

function TLightHelper.White: TLight;
begin
  Result{%H-}.Init(Value);
end;

procedure TLightHelper.RangeCheck;
begin
  Self := RangeChecked;
end;

function TLightHelper.RangeChecked: TLight;
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
    Result[c] := EnsureRange(Self[c], 0, MAX_LIGHT_LEVEL);
end;

procedure TLightHelper.Init(AValue : integer);
var
  c : TLightColor;
begin
  for c := Low(TLightColor) to High(TLightColor) do
    Self[c] := AValue;
end;

procedure TLightHelper.Init(const RedValue, GreenValue, BlueValue : integer);
begin
  Self[lcRed] := RedValue;
  Self[lcGreen] := GreenValue;
  Self[lcBlue] := BlueValue;
end;

end.
