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
    procedure GetIntegerBorders(var a, b : TIntVector3);
  end;

  { TVelocityBox }

  TVelocityBox = record
    CollisionBox : TCollisionBox;
    Velocity : TVector3;
    AngularVelocityMatrix : TMatrix3x3;
    function TimeCalculate(const TimeInSeconds : Double) : TVelocityBox;
    function SuggestedDelay : Double;
  end;

function CreateCollosionBox(const Position : TVector3; const Rotation : TRotationVector; const Size : TSizeVector) : TCollisionBox; inline;
function CheckCollision(const A, B : TCollisionBox; var Where : TVector3; const MaxDepth : Integer = 8) : Boolean;

implementation

uses
  Math;

const
  Corners : array[0..7] of TIntVector3 = ((-1, -1, -1),(-1, -1, 1), (-1, 1, -1),(-1, 1, 1), (1, -1, -1),(1, -1, 1), (1, 1, -1),(1, 1, 1));

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

function MinIndexArray(const Values : array of Double) : specialize TArray<Integer>;
var
  i, m : Integer;
begin
  m := MinIndex(Values);
  Result := [];
  for i := Low(Values) to High(Values) do
     if Values[i] = m then
        Insert(i, Result, Length(Result));
end;

function CheckSubCollision(const A, B : TCollisionBox; var Where: TVector3; const MaxDepth : Integer) : Boolean;
var
  SubA1, SubA2, SubB1, SubB2 : TCollisionBox;
begin
  A.Cut(BiggestDimension(A.Size), SubA1, SubA2);
  B.Cut(BiggestDimension(B.Size), SubB1, SubB2);
  case MinIndex([SquaredHypot3(SubA1.Position-SubB1.Position), SquaredHypot3(SubA1.Position-SubB2.Position), SquaredHypot3(SubA2.Position - SubB1.Position), SquaredHypot3(SubA2.Position - SubB2.Position)]) of
    0: Result := CheckCollision(SubA1, SubB1, Where, MaxDepth);
    1: Result := CheckCollision(SubA1, SubB2, Where, MaxDepth);
    2: Result := CheckCollision(SubA2, SubB1, Where, MaxDepth);
    3: Result := CheckCollision(SubA2, SubB2, Where, MaxDepth);
  end;
end;

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
     Exit(True);
  end;
  MaxDistance := (Hypot3(A.Size)+Hypot3(B.Size))/2;
  if (Distance > MaxDistance) or (MaxDepth<=0) then
     Exit(False);
  Exit(CheckSubCollision(A, B, Where, MaxDepth-1));
end;

{ TVelocityBox }

function TVelocityBox.TimeCalculate(const TimeInSeconds: Double): TVelocityBox;
begin
  Result.CollisionBox.RotationMatrix := AngularVelocityMatrix * CollisionBox.RotationMatrix;
  Result.Velocity := Result.CollisionBox.RotationMatrix * Transposing(CollisionBox.RotationMatrix) * Velocity;
  Result.CollisionBox.Position := CollisionBox.Position + (TimeInSeconds/2) * (Result.CollisionBox.RotationMatrix * Result.Velocity + CollisionBox.RotationMatrix * Velocity);
  Result.AngularVelocityMatrix := AngularVelocityMatrix;
  Result.CollisionBox.Size := CollisionBox.Size;
end;

function TVelocityBox.SuggestedDelay: Double;
begin
  Exit(0.125/(abs(CollisionBox.Size[AxisX]) + abs(CollisionBox.Size[AxisY]) + abs(CollisionBox.Size[AxisZ]) + abs(Velocity[AxisX]) + abs(Velocity[AxisY]) + abs(Velocity[AxisZ]) + 1 ));
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

procedure TCollisionBox.GetIntegerBorders(var a, b: TIntVector3);
var
  v : TVector3;
  Corner : TIntVector3;
  x : TAxis;
begin
  for Corner in Corners do
  begin
      v := RotationMatrix*Vector3(Size[axisX]*Corner[axisX]/2, Size[axisY]*Corner[axisY]/2, Size[axisZ]*Corner[axisZ]/2) + Position;
      for x := low(TAxis) to High(TAxis) do
      begin
          if v[x] < a[x] then
             a[x] := floor(v[x])-1;
          if v[x] > b[x] then
             b[x] := ceil(v[x])+1;
      end;
  end;
end;

end.

