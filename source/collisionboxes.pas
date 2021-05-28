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
    function CheckCollision(const B : TCollisionBox; var Where : TVector3; const MaxDepth : Integer = 256; const CutAxis : TAxis = AxisX) : Boolean;
  end;

function CreateCollosionBox(const Position : TVector3; const Rotation : TRotationVector; const Size : TSizeVector) : TCollisionBox; inline;
function CheckCollision(const A, B : TCollisionBox; var Where : TVector3; const MaxDepth : Integer = 256; const CutAxis : TAxis = AxisX) : Boolean;

implementation

uses
  Math;

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

function CheckSubCollision(const A, B : TCollisionBox; var Where: TVector3; const MaxDepth : Integer; const CutAxis : TAxis) : Boolean;
var
  SubA1, SubA2, SubB1, SubB2 : TCollisionBox;
begin
  A.Cut(CutAxis, SubA1, SubA2);
  B.Cut(CutAxis, SubB1, SubB2);
  case MinIndex([Hypot3(SubA1.Position-SubB1.Position), Hypot3(SubA1.Position-SubB2.Position), Hypot3(SubA2.Position - SubB1.Position), Hypot3(SubA2.Position - SubB2.Position)]) of
    0: Result := CheckCollision(SubA1, SubB1, Where, MaxDepth, CutAxis);
    1: Result := CheckCollision(SubA1, SubB2, Where, MaxDepth, CutAxis);
    2: Result := CheckCollision(SubA2, SubB1, Where, MaxDepth, CutAxis);
    3: Result := CheckCollision(SubA2, SubB2, Where, MaxDepth, CutAxis);
  end;
end;

function CheckCollision(const A, B: TCollisionBox; var Where: TVector3; const MaxDepth: Integer; const CutAxis : TAxis): Boolean;
const
  Epsilon = 2*MinDouble;
var
  Distance, MinDistance, MaxDistance : Double;
begin
  Distance := Hypot3(A.Position - B.Position);
  MinDistance := (A.Size.Min + B.Size.Min)/2;
  if Distance - Epsilon <= MinDistance + Epsilon then
  begin
     Where := (A.Position + B.Position)/2;
     Exit(True);
  end;
  MaxDistance := (Hypot3(A.Size)+Hypot3(B.Size))/2;
  if (Distance > MaxDistance) or (MaxDepth<=0) then
     Exit(False);
  Exit(CheckSubCollision(A, B, Where, MaxDepth-1, NextAxis[CutAxis]));
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

  Result.Position := Position + (RotationMatrix*PositionOffset)*TextureModeSidesAxis[Side].Offset ;
end;

function TCollisionBox.CheckCollision(const B: TCollisionBox; var Where: TVector3; const MaxDepth: Integer; const CutAxis: TAxis): Boolean;
begin
  Result := CollisionBoxes.CheckCollision(Self, B, Where, MaxDepth, CutAxis);
end;

end.

