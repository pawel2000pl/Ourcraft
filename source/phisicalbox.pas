unit PhisicalBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CalcUtils, CollisionBoxes, math, Locker, ProcessUtils, PrefixSI;

type
  TPhisicalBox = class;

  TCollisionChecker = function(Box : TPhisicalBox) : Boolean;

  { TPhisicalBox }

  TPhisicalBox = class(TLocker)
  private
    fMicroseconds : QWord;
    fAccurancy : Double;

    fCollisionBox : TCollisionBox;
    RotateVector : TRotationVector;

    fVelocity : TVector3;
    fAngularVelocity : TRotationVector;

    fAdditionalVelocity : TVector3;
    fAdditionalAngularVelocity : TVector3;
                                      
    fForce : TVector3;
    fForceMoment : TRotationVector;

    function GetPosition: TVector3; inline; //center
    function GetSize: TSizeVector; inline;
    procedure SetPosition(AValue: TVector3); inline;
    procedure SetRotate(AValue: TRotationVector); inline; 
    procedure SetSize(const AValue : TSizeVector); inline;

  protected
    function ResetTime : Double;

  public
    procedure ZeroForce;
    property Size : TSizeVector read GetSize write SetSize;
    property Rotate : TRotationVector read RotateVector write SetRotate;
    property Position : TVector3 read GetPosition write SetPosition;

    property Velocity : TVector3 read fVelocity write fVelocity;
    property AngularVelocity : TVector3 read fAngularVelocity write fAngularVelocity;

    property Force : TVector3 read fForce write fForce;
    property ForceMoment : TRotationVector read fForceMoment write fForceMoment;

    property CollisionBox : TCollisionBox read fCollisionBox;

    procedure GetIntegerBorders(var a, b : TIntVector3);

    function GetMass : Double; virtual;
    function GetInertiaMoment(const Axis : TAxis) : Double; overload;
    function GetInertiaMoment : TRotationVector; overload;
    procedure Tick(dt : Double);
    procedure AddResistance(const Dentisy : Double);
    procedure AddLocalForce(const Where : TVector3; const Value : TVector3);
    procedure AddGlobalForce(const Where : TVector3; const Value : TVector3);
    procedure AddGlobalAcceleration(const Value : TVector3);

    procedure AddLocalVelocity(const Where : TVector3; const Value : TVector3);
    procedure AddGlobalVelocity(const Where : TVector3; const Value : TVector3);

    procedure MoveLocal(const Where : TVector3; const Value : TVector3);
    procedure MoveGlobal(const Where : TVector3; const Value : TVector3);

    function VelocityAtGlobalPoint(const Where : TVector3) : TVector3;
    function SuggestedDelay : Double;

    constructor Create;
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
  fCollisionBox.RotationMatrix := CreateRotateMatrixFromVector(RotateVector);
end;

procedure TPhisicalBox.SetSize(const AValue: TSizeVector);
begin
  fCollisionBox.Size := AValue;
end;

function TPhisicalBox.ResetTime: Double;
begin
  Result := fMicroseconds;
  fMicroseconds := GetMicroseconds;
  Result := fMicroseconds - Result;
  Result *= PrefixSI.Micro;
end;

procedure TPhisicalBox.ZeroForce;
begin
  Force := Vector3(0, 0, 0);
  ForceMoment := Vector3(0, 0, 0);  
  fAdditionalVelocity := Vector3(0, 0, 0);
  fAdditionalAngularVelocity := Vector3(0, 0, 0);
end;

procedure TPhisicalBox.GetIntegerBorders(var a, b: TIntVector3);
var
  v : TVector3;
  Corner : TIntVector3;
  x : TAxis;
begin
  for Corner in Corners do
  begin
      v := CollisionBox.RotationMatrix*Vector3(Size[axisX]*Corner[axisX]/2, Size[axisY]*Corner[axisY]/2, Size[axisZ]*Corner[axisZ]/2);
      for x := low(TAxis) to High(TAxis) do
      begin
          if v[x] < a[x] then
             a[x] := floor(v[x])-1;
          if v[x] > b[x] then
             b[x] := ceil(v[x])+1;
      end;
  end;
end;

function TPhisicalBox.GetMass: Double;
begin
  Result := 1000;
end;

function TPhisicalBox.GetInertiaMoment(const Axis: TAxis): Double;
begin
  Result := GetMass * (sqr(CollisionBox.Size[NextAxis[Axis]]) + sqr(CollisionBox.Size[NextAxis[NextAxis[Axis]]])) / 12;
end;

function TPhisicalBox.GetInertiaMoment: TRotationVector;
var
  a : TAxis;
begin
  for a := low(TAxis) to High(TAxis) do
      Result[a] := GetInertiaMoment(a);
end;

procedure TPhisicalBox.Tick(dt: Double);  ////remove this parametr
var
  NewVelocity : TVector3;
  NewAngularVelocity : TRotationVector;
  NewShapeRotate : TRotationVector;
  a : TAxis;
  I : Double;
  oldRotateMatrix : TMatrix3x3;
begin
  Lock;
  dt := SuggestedDelay;//0.002;//ResetTime;

  NewVelocity := Force/GetMass*dt + fVelocity + fAdditionalVelocity;
  for a := low(TAxis) to High(TAxis) do
  begin
      I := GetInertiaMoment(a);
      if I > 0 then
         NewAngularVelocity[a] := ForceMoment[a]/I * dt;
  end;
  NewAngularVelocity := CollisionBox.RotationMatrix*NewAngularVelocity + fAngularVelocity + fAdditionalAngularVelocity;
  Position := Position + CollisionBox.RotationMatrix*((fVelocity + NewVelocity)*(dt/2));
  NewShapeRotate := RotateVector + ((fAngularVelocity + NewAngularVelocity)*(dt/2));

  //FromZeroTo2PiVector(NewShapeRotate);

  oldRotateMatrix := CollisionBox.RotationMatrix;
  Rotate := NewShapeRotate;
  fVelocity := Transposing(CollisionBox.RotationMatrix) * (oldRotateMatrix * NewVelocity);
  fAngularVelocity := NewAngularVelocity;

  ZeroForce;

  Unlock;

  ////test
  I := Hypot3(fVelocity);
  if I > dt then
    fVelocity := fVelocity*(1-dt)
    else
    fVelocity := Vector3(0, 0, 0);

  I := Hypot3(fAngularVelocity);
  if I > dt then
    fAngularVelocity := fAngularVelocity*(1-dt)
    else
    fAngularVelocity := Vector3(0, 0, 0);
end;

procedure TPhisicalBox.AddResistance(const Dentisy: Double);
var
  Corner : TIntVector3;
  F, S, v, CornerPlace : TVector3;
  a : TAxis;
begin
  exit;  //////////////////////////////////////
  S := Vector3(CollisionBox.Size[SizeDepth]*CollisionBox.Size[SizeHeight]/4, CollisionBox.Size[SizeWidth]*CollisionBox.Size[SizeDepth]/4, CollisionBox.Size[SizeWidth]*CollisionBox.Size[SizeHeight]/4);
  for Corner in Corners do
  begin
       CornerPlace := Vector3(CollisionBox.Size[SizeWidth]/4*Corner[AxisX], CollisionBox.Size[SizeHeight]/4*Corner[AxisY], CollisionBox.Size[SizeDepth]/4*Corner[AxisZ]);
       v := fVelocity + VectorProduct(fAngularVelocity,CornerPlace);
       for a := low(TAxis) to High(TAxis) do
            F[a] := v[a] * abs(v[a]) * S[a] * Dentisy / (-2);
       AddLocalForce(CornerPlace, F/1000);
  end;
end;

procedure TPhisicalBox.AddLocalForce(const Where: TVector3; const Value: TVector3);
begin
  Force := Force + Value;
  ForceMoment := ForceMoment + VectorProduct(Where, Value);
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
  Force := Force + Transposing(CollisionBox.RotationMatrix)*Value*GetMass;
end;

procedure TPhisicalBox.AddLocalVelocity(const Where: TVector3; const Value: TVector3);
var
  v : TVector3;
begin                                    
  v[AxisX] := Value[AxisX] * Where[AxisX] / GetInertiaMoment(AxisX);
  v[AxisY] := Value[AxisY] * Where[AxisY] / GetInertiaMoment(AxisY);
  v[AxisZ] := Value[AxisZ] * Where[AxisZ] / GetInertiaMoment(AxisZ);
  v := (-GetMass) * VectorProduct(Where, v);
  fAdditionalAngularVelocity += v;
  fAdditionalVelocity += (Value-VectorProduct(v, Where));
end;

procedure TPhisicalBox.AddGlobalVelocity(const Where: TVector3; const Value: TVector3);
var
  m : TMatrix3x3;
begin
  m := Transposing(CollisionBox.RotationMatrix);
  AddLocalVelocity(m*(Where - CollisionBox.Position), m*Value);
end;

procedure TPhisicalBox.MoveLocal(const Where: TVector3; const Value: TVector3);
var
  v : TVector3;
begin
  v[AxisX] := Value[AxisX] * Where[AxisX] / GetInertiaMoment(AxisX);
  v[AxisY] := Value[AxisY] * Where[AxisY] / GetInertiaMoment(AxisY);
  v[AxisZ] := Value[AxisZ] * Where[AxisZ] / GetInertiaMoment(AxisZ);
  v := (-GetMass) * VectorProduct(Where, v/SquaredHypot3(Where));
  Rotate := Rotate + CollisionBox.RotationMatrix * v;
  Position := Position + CollisionBox.RotationMatrix * (Value-VectorProduct(v, Where));
end;

procedure TPhisicalBox.MoveGlobal(const Where: TVector3; const Value: TVector3);
var
  m : TMatrix3x3;
begin
  m := Transposing(CollisionBox.RotationMatrix);
  MoveLocal(m*(Where - CollisionBox.Position), m*Value);
end;

function TPhisicalBox.VelocityAtGlobalPoint(const Where: TVector3): TVector3;
begin
  Exit(CollisionBox.RotationMatrix * Velocity + VectorProduct(AngularVelocity, Where-Position));
end;

function TPhisicalBox.SuggestedDelay: Double;
begin
  Result := fAccurancy / (1+Hypot3(Velocity) + sqrt(SquaredHypot3(Rotate)*SquaredHypot3(Size/2)));
end;

constructor TPhisicalBox.Create;
begin
  inherited Create;
  ZeroForce;
  Position := Vector3(0, 0, 0);
  Rotate := Vector3(0, 0, 0);
  fCollisionBox.RotationMatrix := IdentityMatrix;
  fMicroseconds := GetMicroseconds;
  fAccurancy:=1/256;
end;

end.

