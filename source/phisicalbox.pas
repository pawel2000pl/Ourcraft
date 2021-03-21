unit PhisicalBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CalcUtils, CollisionBoxes, math;

type
  TPhisicalShape = record
    Position : TVector3;
    Size : TSizeVector;
    Rotate : TRotationVector;
  end;

  { TPhisicalBox }

  TPhisicalBox = class
  private
    fCollisionBox : TCollisionBox;
    RotateVector : TRotationVector;

    fVelocity : TVector3;
    fAngularVelocity : TRotationVector;
                                      
    Force : TVector3;
    ForceMoment : TRotationVector;

    function GetPosition: TVector3; inline;
    function GetSize: TSizeVector; inline;
    procedure SetPosition(AValue: TVector3); inline;
    procedure SetRotate(AValue: TRotationVector); inline; 
    procedure SetSize(const AValue : TSizeVector); inline;
  public
    property Size : TSizeVector read GetSize write SetSize;
    property Rotate : TRotationVector read RotateVector write SetRotate;
    property Position : TVector3 read GetPosition write SetPosition;

    property Velocity : TVector3 read fVelocity write fVelocity;
    property AngularVelocity : TVector3 read fAngularVelocity write fAngularVelocity;

    property CollisionBox : TCollisionBox read fCollisionBox;

    procedure GetIntegerBorders(var a, b : TIntVector3);

    function GetMass : Double; virtual;
    function GetInertaMoment(const Axis : TAxis) : Double; overload;
    function GetInertaMoment : TRotationVector; overload;
    procedure Tick(const dt : Double);
    procedure AddResistance(const Dentisy : Double);
    procedure AddLocalForce(const Where : TVector3; const Value : TVector3);
    procedure AddGlobalForce(const Where : TVector3; const Value : TVector3);
    procedure AddGlobalAcceleration(const Value : TVector3);
  end;

implementation

const
  Corners : array[0..7] of TIntVector3 = ((-1, -1, -1),(-1, -1, 1), (-1, 1, -1),(-1, 1, 1), (1, -1, -1),(1, -1, 1), (1, 1, -1),(1, 1, 1));

{ TPhisicalBox }

function TPhisicalBox.GetSize: TSizeVector;
begin
  Result := CollisionBox.Size;
end;

function TPhisicalBox.GetPosition: TVector3;
begin
  Result := CollisionBox.Position;
end;

procedure TPhisicalBox.SetPosition(AValue: TVector3);
begin
  fCollisionBox.Position := AValue;
end;

procedure TPhisicalBox.SetRotate(AValue: TRotationVector);
begin
  if RotateVector = AValue then
     Exit;
  RotateVector := AValue;
  fCollisionBox.RotationMatrix := CreateRotateMatrixZXY(RotateVector);
end;

procedure TPhisicalBox.SetSize(const AValue: TSizeVector);
begin
  fCollisionBox.Size := AValue;
end;

procedure TPhisicalBox.GetIntegerBorders(var a, b: TIntVector3);
var
  v : TVector3;
  Corner : TIntVector3;
  x : TAxis;
begin
  for Corner in Corners do
  begin
      v := CollisionBox.RotationMatrix*Vector3(Size[axisX]*Corner[axisX]/2, Size[axisY]*Corner[axisY]/2, Size[axisZ]*Corner[axisZ]/2)+Position;
      for x := low(TAxis) to High(TAxis) do
      begin
          if v[x] < a[x] then
             a[x] := floor(v[x]);
          if v[x] > b[x] then
             a[x] := floor(v[x]);
      end;
  end;
end;

function TPhisicalBox.GetMass: Double;
begin
  Result := 1;
end;

function TPhisicalBox.GetInertaMoment(const Axis: TAxis): Double;
begin
  Result := GetMass * (sqr(CollisionBox.Size[NextAxis[Axis]]) + sqr(CollisionBox.Size[NextAxis[NextAxis[Axis]]])) / 12;
end;

function TPhisicalBox.GetInertaMoment: TRotationVector;
var
  a : TAxis;
begin
  for a := low(TAxis) to High(TAxis) do
      Result[a] := GetInertaMoment(a);
end;

procedure TPhisicalBox.Tick(const dt: Double);
var
  NewVelocity : TVector3;
  NewAngularVelocity : TRotationVector;
  NewShapeRotate : TRotationVector;
  a : TAxis;
  I : Double;
begin
  NewVelocity := Force/GetMass*dt + fVelocity;
  for a := low(TAxis) to High(TAxis) do
  begin
      I := GetInertaMoment(a);
      if I > 0 then
         NewAngularVelocity[a] := ForceMoment[a]/I * dt + fAngularVelocity[a];
  end;
  Position := CollisionBox.Position + (fVelocity + NewVelocity/2) * dt;
  NewShapeRotate := RotateVector + (fAngularVelocity + NewAngularVelocity/2) * dt;
  fVelocity := CreateRotateMatrixZXY(RotateVector - NewShapeRotate)*NewVelocity;
  Rotate := NewShapeRotate;
  fAngularVelocity := NewAngularVelocity;
end;

procedure TPhisicalBox.AddResistance(const Dentisy: Double);
var
  Corner : TIntVector3;
  F, S, v, CornerPlace : TVector3;
  a : TAxis;
begin
  S := Vector3(CollisionBox.Size[SizeDepth]*CollisionBox.Size[SizeHeight]/4, CollisionBox.Size[SizeWidth]*CollisionBox.Size[SizeDepth]/4, CollisionBox.Size[SizeWidth]*CollisionBox.Size[SizeHeight]/4);
  for Corner in Corners do
  begin
       CornerPlace := Vector3(CollisionBox.Size[SizeWidth]/4*Corner[AxisX], CollisionBox.Size[SizeHeight]/4*Corner[AxisY], CollisionBox.Size[SizeDepth]/4*Corner[AxisZ]);
       v := fVelocity + VectorProduct(fAngularVelocity,CornerPlace);
       for a := low(TAxis) to High(TAxis) do
            F[a] := v[a] * abs(v[a]) * S[a] * Dentisy / (-2);
       AddLocalForce(CornerPlace, F);
  end;
end;

procedure TPhisicalBox.AddLocalForce(const Where: TVector3; const Value: TVector3);
begin
  Force := Force + Value;
  ForceMoment := ForceMoment + VectorProduct(Value, Where - CollisionBox.Size/2);
end;

procedure TPhisicalBox.AddGlobalForce(const Where: TVector3; const Value: TVector3);
var
  m : TMatrix3x3;
begin
  m := Transposing(CollisionBox.RotationMatrix);
  AddLocalForce(m*(Where - CollisionBox.Position), m*Value);
end;

procedure TPhisicalBox.AddGlobalAcceleration(const Value: TVector3);
begin
    AddGlobalForce(Vector3(0, 0, 0), Value*GetMass());
end;

end.

