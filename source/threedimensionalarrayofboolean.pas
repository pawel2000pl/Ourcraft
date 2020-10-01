unit ThreeDimensionalArrayOfBoolean;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, CalcUtils;

type

  { TThreeDimensionalArrayOfBoolean }

  TThreeDimensionalArrayOfBoolean = class
  private
    fData : array of array of array of boolean;
    fWidth, fHeight, fDepth : integer;
    function GetData(const x, y, z : integer) : boolean;
    procedure SetData(const x, y, z : integer; const AValue : boolean);
  public
    property Data[const x, y, z : integer] : boolean read GetData write SetData; default;
    constructor Create;
    destructor Destroy; override;
  end;

  { TThreeDimensionalSignedArrayOfBoolean }

  TThreeDimensionalSignedArrayOfBoolean = class
    private
      fArrays : array[0..1, 0..1, 0..1] of TThreeDimensionalArrayOfBoolean;
      fOffsetX, fOffsetY, fOffsetZ : Integer;
      function GetData(x, y, z : integer): boolean;
      function GetDataByVector(const Vector : TIntVector3): Boolean; inline;
      procedure SetData(x, y, z : integer; const AValue: boolean);
      procedure SetDataByVector(const Vector : TIntVector3; const AValue: Boolean); inline;
    protected
      function GetArray(var x, y, z : Integer) : TThreeDimensionalArrayOfBoolean;
    public
      property DataByVector[const Vector : TIntVector3] : Boolean read GetDataByVector write SetDataByVector;
      property Data[x, y, z : integer] : Boolean read GetData write SetData; default;
      constructor Create(const OffsetX : Integer = 0; const OffsetY : Integer = 0; const OffsetZ : Integer = 0);
      constructor Create(const OffsetVector : TIntVector3);
      destructor Destroy; override;
  end;

implementation

{ TThreeDimensionalSignedArrayOfBoolean }

function TThreeDimensionalSignedArrayOfBoolean.GetArray(var x, y, z: Integer): TThreeDimensionalArrayOfBoolean;
var
  ax, ay, az : Integer;
begin
   if x >= 0 then
    ax := 1
    else
    begin
      ax := 0;
      x := -1-x;
    end;

  if y >= 0 then
    ay := 1
    else
    begin
      ay := 0;
      y := -1-y;
    end;

  if z >= 0 then
    az := 1
    else
    begin
      az := 0;
      z := -1-z;
    end;

  Result := fArrays[ax, ay, az];
end;

function TThreeDimensionalSignedArrayOfBoolean.GetData(x, y, z : integer): boolean;
begin          
  Dec(x, fOffsetX);
  Dec(y, fOffsetY);
  Dec(z, fOffsetZ);

  Result := GetArray(x, y, z).Data[x, y, z];
end;

procedure TThreeDimensionalSignedArrayOfBoolean.SetData(x, y, z : integer; const AValue: boolean);
begin
  Dec(x, fOffsetX);
  Dec(y, fOffsetY);
  Dec(z, fOffsetZ);

  GetArray(x, y, z).Data[x, y, z] := AValue;
end;

function TThreeDimensionalSignedArrayOfBoolean.GetDataByVector(
  const Vector: TIntVector3): Boolean;
begin
  Result := GetData(Vector[axisX], Vector[axisY], Vector[axisZ]);
end;

procedure TThreeDimensionalSignedArrayOfBoolean.SetDataByVector(
  const Vector: TIntVector3; const AValue: Boolean);
begin
  SetData(Vector[axisX], Vector[axisY], Vector[axisZ], AValue);
end;

constructor TThreeDimensionalSignedArrayOfBoolean.Create(
  const OffsetX: Integer; const OffsetY: Integer; const OffsetZ: Integer);
begin
  fOffsetX:=OffsetX;
  fOffsetY:=OffsetY;
  fOffsetZ:=OffsetZ;
  fArrays[0, 0, 0] := TThreeDimensionalArrayOfBoolean.Create;
  fArrays[0, 0, 1] := TThreeDimensionalArrayOfBoolean.Create;
  fArrays[0, 1, 0] := TThreeDimensionalArrayOfBoolean.Create;
  fArrays[0, 1, 1] := TThreeDimensionalArrayOfBoolean.Create;
  fArrays[1, 0, 0] := TThreeDimensionalArrayOfBoolean.Create;
  fArrays[1, 0, 1] := TThreeDimensionalArrayOfBoolean.Create;
  fArrays[1, 1, 0] := TThreeDimensionalArrayOfBoolean.Create;
  fArrays[1, 1, 1] := TThreeDimensionalArrayOfBoolean.Create;
end;

constructor TThreeDimensionalSignedArrayOfBoolean.Create(
  const OffsetVector: TIntVector3);
begin
  Create(OffsetVector[axisX], OffsetVector[axisY], OffsetVector[axisZ]);
end;

destructor TThreeDimensionalSignedArrayOfBoolean.Destroy;
begin               
  fArrays[0, 0, 0].Free;
  fArrays[0, 0, 1].Free;
  fArrays[0, 1, 0].Free;
  fArrays[0, 1, 1].Free;
  fArrays[1, 0, 0].Free;
  fArrays[1, 0, 1].Free;
  fArrays[1, 1, 0].Free;
  fArrays[1, 1, 1].Free;
  inherited Destroy;
end;

{ TThreeDimensionalArrayOfBoolean }

function TThreeDimensionalArrayOfBoolean.GetData(const x, y, z : integer) : boolean;
begin
  Result := (x < fWidth) and (y < fHeight) and (z < fDepth) and fData[x, y, z];
end;

procedure TThreeDimensionalArrayOfBoolean.SetData(const x, y, z : integer;
  const AValue : boolean);
begin
  if not ((x < fWidth) and (y < fHeight) and (z < fDepth)) then
  begin
    UpdateIfGreater(fWidth, x + 1);
    UpdateIfGreater(fHeight, y + 1);
    UpdateIfGreater(fDepth, z + 1);
    //it fill array with 0 (false)
    setlength(fData, fWidth, fHeight, fDepth);
  end;
  fData[x, y, z] := AValue;
end;

constructor TThreeDimensionalArrayOfBoolean.Create;
begin
  setlength(fData, 0, 0, 0);
  fWidth := 0;
  fHeight := 0;
  fDepth := 0;
end;

destructor TThreeDimensionalArrayOfBoolean.Destroy;
begin
  setlength(fData, 0, 0, 0);
  inherited Destroy;
end;

end.
