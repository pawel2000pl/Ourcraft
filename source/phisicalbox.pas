unit PhisicalBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CalcUtils;

type
  TPhisicalShape = record
    Position : TVector3;
    Size : TSizeVector;
    Rotate : TRotationVector;
  end;

  { TPhisicalBox }

  TPhisicalBox = class
  private
    FPosition: TVector3;
    FRotate: TRotationVector;
    Shape : TPhisicalShape;

    Velocity : TVector3;
    AngularVelocity : TRotationVector;
                                      
    Force : TVector3;
    ForceMoment : TRotationVector;

    function GetSize: TSizeVector;
    procedure SetPosition(AValue: TVector3);
    procedure SetRotate(AValue: TRotationVector);
  protected
    procedure SetSize(const AValue : TSizeVector);
  public
    property Size : TSizeVector read GetSize;
    property Rotate : TRotationVector read FRotate write SetRotate;
    property Position : TVector3 read FPosition write SetPosition;

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

{ TPhisicalBox }

function TPhisicalBox.GetSize: TSizeVector;
begin
  Result := Shape.Size;
end;

procedure TPhisicalBox.SetPosition(AValue: TVector3);
begin
  Shape.Position := AValue;
end;

procedure TPhisicalBox.SetRotate(AValue: TRotationVector);
begin
  Shape.Rotate := AValue;
end;

procedure TPhisicalBox.SetSize(const AValue: TSizeVector);
begin
  Shape.Size := AValue;
end;

function TPhisicalBox.GetMass: Double;
begin
  Result := 1;
end;

function TPhisicalBox.GetInertaMoment(const Axis: TAxis): Double;
begin
  Result := GetMass * (sqr(Shape.Size[NextAxis[Axis]]) + sqr(Shape.Size[NextAxis[NextAxis[Axis]]])) / 12;
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
  NewVelocity := Force/GetMass*dt + Velocity;
  for a := low(TAxis) to High(TAxis) do
  begin
      I := GetInertaMoment(a);
      if I > 0 then
         NewAngularVelocity[a] := ForceMoment[a]/I * dt + AngularVelocity[a];
  end;
  Shape.Position := Shape.Position + (Velocity + NewVelocity/2) * dt;
  NewShapeRotate := Shape.Rotate + (AngularVelocity + NewAngularVelocity/2) * dt;
  Velocity := CreateRotateMatrixZXY(Shape.Rotate - NewShapeRotate)*NewVelocity;
  Shape.Rotate := NewShapeRotate;
  AngularVelocity := NewAngularVelocity;
end;

procedure TPhisicalBox.AddResistance(const Dentisy: Double);
const
  Corners : array[0..7] of TIntVector3 = ((-1, -1, -1),(-1, -1, 1), (-1, 1, -1),(-1, 1, 1), (1, -1, -1),(1, -1, 1), (1, 1, -1),(1, 1, 1));
var
  Corner : TIntVector3;
  F, S, v, CornerPlace : TVector3;
  a : TAxis;
begin
  S := Vector3(Shape.Size[SizeDepth]*Shape.Size[SizeHeight]/4, Shape.Size[SizeWidth]*Shape.Size[SizeDepth]/4, Shape.Size[SizeWidth]*Shape.Size[SizeHeight]/4);
  for Corner in Corners do
  begin
       CornerPlace := Vector3(Shape.Size[SizeWidth]/4*Corner[AxisX], Shape.Size[SizeHeight]/4*Corner[AxisY], Shape.Size[SizeDepth]/4*Corner[AxisZ]);
       v := Velocity + VectorProduct(AngularVelocity,CornerPlace);
       for a := low(TAxis) to High(TAxis) do
            F[a] := v[a] * abs(v[a]) * S[a] * Dentisy / (-2);
       AddLocalForce(CornerPlace, F);
  end;
end;

procedure TPhisicalBox.AddLocalForce(const Where: TVector3; const Value: TVector3);
begin
  Force := Force + Value;
  ForceMoment := ForceMoment + VectorProduct(Value, Where - Shape.Size/2);
end;

procedure TPhisicalBox.AddGlobalForce(const Where: TVector3; const Value: TVector3);
var
  m : TMatrix3x3;
begin
  m := Transposing(CreateRotateMatrixZXY(Shape.Rotate));
  AddLocalForce(m*(Where - Shape.Position), m*Value);
end;

procedure TPhisicalBox.AddGlobalAcceleration(const Value: TVector3);
begin
    AddGlobalForce(Vector3(0, 0, 0), Value*GetMass());
end;

end.

