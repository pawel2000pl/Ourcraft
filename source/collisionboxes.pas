unit CollisionBoxes;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, CalcUtils, TextureMode;

type

  { TCollisionBox }

  TCollisionBox = record
    Position : TVector3; ///center
    RotationMatrix : TMatrix3x3;
    Size : TSizeVector;
    procedure GetRegularCube(out LeftBottomFront, RightTopBack : TVector3);
    procedure Cut(const Axis : TAxis; out A, B : TCollisionBox);
    function GetSide(const Side : TTextureMode; const Thickness : Double = 0) : TCollisionBox;
    function CheckCollision(const B : TCollisionBox; var Where : TVector3; const MaxDepth : Integer = 8) : Boolean;
    function GetHittedSide(const Where : TVector3) : TTextureMode;
    function GetNormalVectorForSide(const Side : TTextureMode) : TVector3;
    function HittedPointToSide(const Where : TVector3) : TVector3;
  end;

function CreateCollosionBox(const Position : TVector3; const Rotation : TRotationVector; const Size : TSizeVector) : TCollisionBox; inline;
function CheckCollision(const A, B : TCollisionBox; var Where : TVector3; const MaxDepth : Integer = 8) : Boolean;

implementation

uses
  Math;

function BiggestDimension(const v : TVector3) : TAxis; inline;
begin
   if abs(v[AxisX]) > abs(v[AxisY]) then
  begin
    if abs(v[AxisX]) > abs(v[axisZ]) then
       Exit(AxisX)
    else
       Exit(AxisZ);
  end
  else
  begin
    if abs(v[AxisY]) > abs(v[axisZ]) then
       Exit(AxisY)
    else
       Exit(AxisZ);
  end;
end;

function CreateCollosionBox(const Position: TVector3; const Rotation: TRotationVector; const Size: TSizeVector): TCollisionBox;
begin
  Result.Position := Position;
  Result.RotationMatrix := CreateRotateMatrixZXY(Rotation);
  Result.Size := Size;
end;

function MinIndex(const Values : array of Double) : Integer;
var
  i : Integer;
begin
  Result := Low(Values);
  for i := Result+1 to High(Values) do
     if Values[i] < Values[Result] then
        Result := i;
end;

function CheckSubCollision(const A, B : TCollisionBox; var Where: TVector3; const MaxDepth : Integer) : Boolean;
var
  SubA1, SubA2, SubB1, SubB2 : TCollisionBox;
begin
  A.Cut(BiggestDimension(A.Size), SubA1, SubA2);
  B.Cut(BiggestDimension(B.Size), SubB1, SubB2);
  case MinIndex([Hypot3(SubA1.Position-SubB1.Position), Hypot3(SubA1.Position-SubB2.Position), Hypot3(SubA2.Position - SubB1.Position), Hypot3(SubA2.Position - SubB2.Position)]) of
    0: Result := CheckCollision(SubA1, SubB1, Where, MaxDepth);
    1: Result := CheckCollision(SubA1, SubB2, Where, MaxDepth);
    2: Result := CheckCollision(SubA2, SubB1, Where, MaxDepth);
    3: Result := CheckCollision(SubA2, SubB2, Where, MaxDepth);
  end;
end;
                    {
function CheckSubCollision(const A, B : TCollisionBox; var Where: TVector3; const MaxDepth : Integer) : Boolean;
var
  axis, axis2, minAxis, minAxis2 : TAxis;
  ia, ib, minIA, minIB : Integer;
  tmp, MinLength : Double;
  SubsA, SubsB : array[TAxis] of array[0..1] of TCollisionBox;

  Wage : Double;
  TempWhere : TVector3;
begin
  for axis := Low(TAxis) to High(TAxis) do
  begin
    A.Cut(axis, SubsA[axis, 0], SubsA[axis, 1]);
    B.Cut(axis, SubsB[axis, 0], SubsB[axis, 1]);
  end;
                            {
  MinLength := MaxDouble;
  minAxis:=AxisX;
  minAxis2:=AxisX;
  minIA := 0;
  minIB := 0;

  for axis := Low(TAxis) to High(TAxis) do
    for axis2 := axis to High(TAxis) do
      for ia := 0 to 1 do
        for ib := 0 to 1 do
        begin
           tmp := Hypot3(SubsA[axis, ia].Position - SubsB[axis2, ib].Position);
           if tmp < MinLength then
           begin
             MinLength:=tmp;
             minAxis:=axis;
             minAxis2:=axis2;
             minIA := ia;
             minIB := ib;
           end;
        end;

  Exit(CheckCollision(SubsA[minAxis, minIA], SubsB[minAxis2, minIB], Where, MaxDepth));   }
  Wage := 0;
  Where := Vector3(0, 0, 0);
  for axis := Low(TAxis) to High(TAxis) do
    for axis2 := axis to High(TAxis) do
      for ia := 0 to 1 do
        for ib := 0 to 1 do
           if CheckCollision(SubsA[axis, ia], SubsB[axis2, ib], TempWhere{%H-}, MaxDepth) then
           begin
              Wage += Hypot3(SubsA[axis, ia].Size)+Hypot3(SubsB[axis2, ib].Size);
              Where := Where + TempWhere;
           end;

  if Wage <> 0 then
     Where := Where / Wage;
  Exit(Wage<>0);
end;            }

function AverageCollisionPlace(const A, B : TCollisionBox) : TVector3; inline;
var
  ha, hb : Double;
begin
  ha := Hypot3(A.Size);
  hb := Hypot3(B.Size);
  Exit((A.Position*hb+B.Position*ha)/(ha+hb));
end;

function CheckCollision(const A, B: TCollisionBox; var Where: TVector3; const MaxDepth: Integer): Boolean;
const
  Epsilon = 2*MinDouble;
var
  Distance, MinDistance, MaxDistance : Double;
begin
  Distance := Hypot3(A.Position - B.Position);
  MinDistance := (A.Size.Min + B.Size.Min)/2;
  if Distance - Epsilon <= MinDistance + Epsilon then
  begin
     Where := AverageCollisionPlace(A, B);
     //Where := (A.Position + B.Position)/2;
     Exit(True);
  end;
  MaxDistance := (Hypot3(A.Size)+Hypot3(B.Size))/2;
  if (Distance > MaxDistance) or (MaxDepth<=0) then
     Exit(False);
  Exit(CheckSubCollision(A, B, Where, MaxDepth-1));
end;

{ TCollisionBox }

procedure TCollisionBox.GetRegularCube(out LeftBottomFront, RightTopBack: TVector3);
var
  s : TVector3;
  a : TAxis;
begin
  s := RotationMatrix*(Size/2);
  LeftBottomFront := Position - s;
  RightTopBack := Position + s;
  for a := Low(TAxis) to High(TAxis) do
     specialize MinToLeft<Double>(LeftBottomFront[a], RightTopBack[a]);
end;

procedure TCollisionBox.Cut(const Axis: TAxis; out A, B: TCollisionBox);
var
  PositionOffset : TVector3;
begin
  A.Size := Size;
  A.Size[Axis] /= 2;
  A.RotationMatrix := RotationMatrix;
  B := A;

  PositionOffset := Vector3(0, 0, 0);
  PositionOffset[Axis] := Size[Axis]/4;
  PositionOffset := RotationMatrix*PositionOffset;

  A.Position := Position + PositionOffset;
  B.Position := Position - PositionOffset;
end;

function TCollisionBox.GetSide(const Side: TTextureMode; const Thickness: Double): TCollisionBox;
var
  PositionOffset : TVector3;
begin
  Result.Size := Size;    
  Result.Size[TextureModeSidesAxis[Side].Axis] := Thickness;
  Result.RotationMatrix := RotationMatrix;
                                    
  PositionOffset := Vector3(0, 0, 0);
  PositionOffset[TextureModeSidesAxis[Side].Axis] := Size[TextureModeSidesAxis[Side].Axis]/2;

  Result.Position := Position + (RotationMatrix*PositionOffset)*TextureModeSidesAxis[Side].Offset;
end;

function TCollisionBox.CheckCollision(const B: TCollisionBox; var Where: TVector3; const MaxDepth: Integer): Boolean;
begin
  Result := CollisionBoxes.CheckCollision(Self, B, Where, MaxDepth);
end;

function AxisAndSignToTextureMode(const a : TAxis; const s : Double) : TTextureMode; inline;
const
  UpArr : array[TAxis] of TTextureMode = (tmNorth, tmUp, tmEast);
  DownArr : array[TAxis] of TTextureMode = (tmSouth, tmDown, tmWest);
begin
  if s > 0 then
     Exit(UpArr[a]);
  Exit(DownArr[a]);
end;

function TCollisionBox.GetHittedSide(const Where: TVector3): TTextureMode;
var
  v : TVector3;
  a : TAxis;
begin
  v := Transposing(RotationMatrix)*(Where-Position);
  for a := low(TAxis) to High(TAxis) do
     if Size[a] = 0 then
        Exit(AxisAndSignToTextureMode(a, 1))
        else
        v[a] /= Size[a];

  if abs(v[AxisX]) > abs(v[AxisY]) then
  begin
    if abs(v[AxisX]) > abs(v[axisZ]) then
       Exit(AxisAndSignToTextureMode(AxisX, v[AxisX]))
    else
       Exit(AxisAndSignToTextureMode(AxisZ, v[AxisZ]));
  end
  else
  begin
    if abs(v[AxisY]) > abs(v[axisZ]) then
       Exit(AxisAndSignToTextureMode(AxisY, v[AxisY]))
    else
       Exit(AxisAndSignToTextureMode(AxisZ, v[AxisZ]));
  end;
end;

function TCollisionBox.GetNormalVectorForSide(const Side: TTextureMode): TVector3;
begin
  Exit(RotationMatrix * Vector3(Size[AxisX]*TextureModeSidesD[Side][AxisX]/2, Size[AxisY]*TextureModeSidesD[Side][AxisY]/2, Size[AxisZ]*TextureModeSidesD[Side][AxisZ]/2));
end;

function TCollisionBox.HittedPointToSide(const Where: TVector3): TVector3;
var
  a : TAxis;
  v : TVector3;
begin
  a := TextureModeSidesAxis[GetHittedSide(Where)].Axis;
  if Size[a] = 0 then
     Exit(Where);
  v := Transposing(RotationMatrix)*(Where - Position);
  if v[a] = 0 then
     Exit(Vector3(0, 0, 0));
  Exit(RotationMatrix * v*abs(Size[a]/v[a]) + Position);
end;

end.

