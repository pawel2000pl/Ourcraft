unit ThreeDimensionalArrayOfAnything;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, CalcUtils, PostPreOperations, math;

type

  { TThreeDimensionalArrayOfAnything }

  generic TThreeDimensionalArrayOfAnything<TAnything> = class
  private
    fData : array of array of array of TAnything;
    fWidth, fHeight, fDepth : integer;
    function GetData(const x, y, z : integer) : TAnything;
    procedure SetData(const x, y, z : integer; const AValue : TAnything);
  public
    function GetCube : TIntVector3;
    property Data[const x, y, z : integer] : TAnything read GetData write SetData; default;
    constructor Create;
    destructor Destroy; override;
  end;

  { TThreeDimensionalSignedArrayOfAnything }

  generic TThreeDimensionalSignedArrayOfAnything<TAnything> = class
    type
      TThreeDimensionalArrayOfSomething = specialize TThreeDimensionalArrayOfAnything<TAnything>;

      { TMyEnumerator }

      TMyEnumerator = class
      private
        cubeMax, cubeMin : TIntVector3;
        instance : TThreeDimensionalSignedArrayOfAnything;
        x, y, z : Integer;
      public
        function MoveNext: Boolean;
        function GetCurrent: TIntVector3; inline;
        property Current : TIntVector3 read GetCurrent;
        constructor Create(const S : TThreeDimensionalSignedArrayOfAnything);
      end;

    private
      fArrays : array[0..1, 0..1, 0..1] of TThreeDimensionalArrayOfSomething;
      fOffsetX, fOffsetY, fOffsetZ : Integer;
      function GetData(x, y, z : integer): TAnything; inline;
      function GetDataByVector(const Vector : TIntVector3): TAnything; inline;
      procedure SetData(x, y, z : integer; const AValue: TAnything); inline;
      procedure SetDataByVector(const Vector : TIntVector3; const AValue: TAnything); inline;
      class function SetValueOfSignedArray(var v : Integer) : Integer; static; inline;
    protected
      function GetArray(var x, y, z : Integer) : TThreeDimensionalArrayOfSomething; inline;
    public
      function GetEnumerator : TMyEnumerator;
      function GetCubeMin : TIntVector3;
      function GetCubeMax : TIntVector3;
      property DataByVector[const Vector : TIntVector3] : TAnything read GetDataByVector write SetDataByVector;
      property Data[x, y, z : integer] : TAnything read GetData write SetData; default;
      constructor Create(const OffsetX : Integer = 0; const OffsetY : Integer = 0; const OffsetZ : Integer = 0);
      constructor Create(const OffsetVector : TIntVector3);
      destructor Destroy; override;
  end;

implementation

{ TThreeDimensionalSignedArrayOfAnything.TMyEnumerator }

function TThreeDimensionalSignedArrayOfAnything.TMyEnumerator.MoveNext: Boolean;
begin
  Inc(x);
  if x > cubeMax.X then
  begin
    x := cubeMin.X;
    Inc(y);
    if y > cubeMax.Y then
    begin
      y := cubeMin.Y;
      Inc(z);
      if z > cubeMax.z then
         Exit(False);
    end;
  end;
  Exit(True);
end;

function TThreeDimensionalSignedArrayOfAnything.TMyEnumerator.GetCurrent: TIntVector3;
begin
  Exit(IntVector3(x, y, z));
end;

constructor TThreeDimensionalSignedArrayOfAnything.TMyEnumerator.Create(
  const S: TThreeDimensionalSignedArrayOfAnything);
begin
  instance := S;
  cubeMin:=S.GetCubeMin;
  cubeMax:=S.GetCubeMax;
  x := cubeMin.X-1;
  y := cubeMin.Y;
  z := cubeMin.Z;
end;

{ TThreeDimensionalSignedArrayOfAnything }

class function TThreeDimensionalSignedArrayOfAnything.SetValueOfSignedArray(var v : Integer) : Integer; inline;
begin
  if v >= 0 then
     Exit(1);
    v := not v;
    Exit(0);
end;

function TThreeDimensionalSignedArrayOfAnything.GetArray(var x, y, z: Integer): TThreeDimensionalArrayOfSomething;
begin
  Exit(fArrays[SetValueOfSignedArray(x), SetValueOfSignedArray(y), SetValueOfSignedArray(z)]);
end;

function TThreeDimensionalSignedArrayOfAnything.GetEnumerator: TMyEnumerator;
begin
  Exit(TMyEnumerator.Create(Self));
end;

function TThreeDimensionalSignedArrayOfAnything.GetCubeMin: TIntVector3;
begin
  Result := IntVector3(
         -max(max(fArrays[0, 0, 0].fWidth, fArrays[0, 1, 0].fWidth), max(fArrays[0, 1, 1].fWidth, fArrays[0, 1, 1].fWidth))+fOffsetX,
         -max(max(fArrays[0, 0, 0].fHeight, fArrays[1, 0, 0].fHeight), max(fArrays[1, 0, 1].fHeight, fArrays[1, 0, 1].fHeight))+fOffsetY,
         -max(max(fArrays[0, 0, 0].fDepth, fArrays[0, 1, 0].fDepth), max(fArrays[1, 1, 0].fDepth, fArrays[1, 1, 0].fDepth))+fOffsetZ);
end;

function TThreeDimensionalSignedArrayOfAnything.GetCubeMax: TIntVector3;
begin
  Result := IntVector3(
         max(max(fArrays[1, 0, 0].fWidth, fArrays[1, 1, 0].fWidth), max(fArrays[1, 1, 1].fWidth, fArrays[1, 1, 1].fWidth))-1+fOffsetX,
         max(max(fArrays[0, 1, 0].fHeight, fArrays[1, 1, 0].fHeight), max(fArrays[1, 1, 1].fHeight, fArrays[1, 1, 1].fHeight))-1+fOffsetY,
         max(max(fArrays[0, 0, 1].fDepth, fArrays[0, 1, 1].fDepth), max(fArrays[1, 1, 1].fDepth, fArrays[1, 1, 1].fDepth))-1+fOffsetZ);
end;

function TThreeDimensionalSignedArrayOfAnything.GetData(x, y, z : integer): TAnything;
begin
  Dec(x, fOffsetX);
  Dec(y, fOffsetY);
  Dec(z, fOffsetZ);

  Result := GetArray(x, y, z).Data[x, y, z];
end;

procedure TThreeDimensionalSignedArrayOfAnything.SetData(x, y, z : integer; const AValue: TAnything);
begin
  Dec(x, fOffsetX);
  Dec(y, fOffsetY);
  Dec(z, fOffsetZ);
  GetArray(x, y, z).Data[x, y, z] := AValue;
end;

function TThreeDimensionalSignedArrayOfAnything.GetDataByVector(
  const Vector: TIntVector3): TAnything;
begin
  Result := GetData(Vector[axisX], Vector[axisY], Vector[axisZ]);
end;

procedure TThreeDimensionalSignedArrayOfAnything.SetDataByVector(
  const Vector: TIntVector3; const AValue: TAnything);
begin
  SetData(Vector[axisX], Vector[axisY], Vector[axisZ], AValue);
end;

constructor TThreeDimensionalSignedArrayOfAnything.Create(
  const OffsetX: Integer; const OffsetY: Integer; const OffsetZ: Integer);
begin
  fOffsetX:=OffsetX;
  fOffsetY:=OffsetY;
  fOffsetZ:=OffsetZ;
  fArrays[0, 0, 0] := TThreeDimensionalArrayOfSomething.Create;
  fArrays[0, 0, 1] := TThreeDimensionalArrayOfSomething.Create;
  fArrays[0, 1, 0] := TThreeDimensionalArrayOfSomething.Create;
  fArrays[0, 1, 1] := TThreeDimensionalArrayOfSomething.Create;
  fArrays[1, 0, 0] := TThreeDimensionalArrayOfSomething.Create;
  fArrays[1, 0, 1] := TThreeDimensionalArrayOfSomething.Create;
  fArrays[1, 1, 0] := TThreeDimensionalArrayOfSomething.Create;
  fArrays[1, 1, 1] := TThreeDimensionalArrayOfSomething.Create;
end;

constructor TThreeDimensionalSignedArrayOfAnything.Create(
  const OffsetVector: TIntVector3);
begin
  Create(OffsetVector[axisX], OffsetVector[axisY], OffsetVector[axisZ]);
end;

destructor TThreeDimensionalSignedArrayOfAnything.Destroy;
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

{ TThreeDimensionalArrayOfAnything }

function TThreeDimensionalArrayOfAnything.GetData(const x, y, z: integer): TAnything;
begin
  if (x < fWidth) and (y < fHeight) and (z < fDepth) then
    Exit(fData[x, y, z]);
  FillByte(Result, SizeOf(TAnything), 0);
end;

procedure TThreeDimensionalArrayOfAnything.SetData(const x, y, z : integer;
  const AValue : TAnything);
begin
  if not ((x < fWidth) and (y < fHeight) and (z < fDepth)) then
  begin
    UpdateIfGreater(fWidth, x + 1);
    UpdateIfGreater(fHeight, y + 1);
    UpdateIfGreater(fDepth, z + 1);
    //it fill array with 0
    setlength(fData, fWidth, fHeight, fDepth);
  end;
  fData[x, y, z] := AValue;
end;

function TThreeDimensionalArrayOfAnything.GetCube: TIntVector3;
begin
  Exit(IntVector3(fWidth, fHeight, fDepth));
end;

constructor TThreeDimensionalArrayOfAnything.Create;
begin
  setlength(fData, 0, 0, 0);
  fWidth := 0;
  fHeight := 0;
  fDepth := 0;
end;

destructor TThreeDimensionalArrayOfAnything.Destroy;
begin
  setlength(fData, 0, 0, 0);
  inherited Destroy;
end;

end.
