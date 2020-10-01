unit GlCamera;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, CalcUtils, GLext, gl, glu, Glut;

type

  TRenderMethod = procedure of object;

  { TGlCamera }

  TGlCamera = class
  private
    fPosition : TVector3;
    fRotate : TRotationVector;
    fRotateNormalVector : TVector3;
    fRotateDownVector : TVector3;
    fRotateLeftVector : TVector3;
    fPerspective : double;
    PerspectiveWidth : double;
    fWidth, fHeight : longword;
    RotateVectorNeedUpdate : boolean;
    function GetBackVector: TVector3;
    function GetRightVector: TVector3;
    function GetUpVector: TVector3;
    procedure SetHeight(const AValue : longword);
    procedure SetPerspective(const AValue : double);
    procedure SetWidth(const AValue : longword);
    procedure UpdatePerspectiveWidth;
    procedure SetPosition(const AValue : TVector3);
    procedure SetRotate(const AValue : TRotationVector);
  public
    procedure SetRotation(const Up, Direction, Rot : double);
    property Width : longword read fWidth write SetWidth;
    property Height : longword read fHeight write SetHeight;
    property Rotate : TRotationVector read FRotate write SetRotate;
    property Perspective : double read fPerspective write SetPerspective;
    property Position : TVector3 read fPosition write SetPosition;
    property ForwardVector : TVector3 read fRotateNormalVector;
    property BackVector : TVector3 read GetBackVector;
    property UpVector : TVector3 read GetUpVector;
    property RightVector : TVector3 read GetRightVector;
    property DownVector : TVector3 read fRotateDownVector;
    property LeftVector : TVector3 read fRotateLeftVector;
    function IsVisibled(const Point : TVector3; const Size : double) : boolean;
    procedure SetMatrix;
    constructor Create;
  end;

implementation

{ TGlCamera }

procedure TGlCamera.SetRotate(const AValue: TRotationVector);
begin
  FRotate := AValue;
  RotateVectorNeedUpdate := True;
  UpdatePerspectiveWidth;
end;

procedure TGlCamera.SetRotation(const Up, Direction, Rot : double);
begin
  SetRotate(Vector3(Up, Direction, Rot));
end;

procedure TGlCamera.UpdatePerspectiveWidth;
begin
  PerspectiveWidth := hypot(fWidth / fHeight, 1) * fPerspective / 2;
end;

procedure TGlCamera.SetHeight(const AValue: longword);
begin
  if fHeight = AValue then
    Exit;
  fHeight := AValue;
  UpdatePerspectiveWidth;
end;

function TGlCamera.GetRightVector: TVector3;
begin
  Result := fRotateLeftVector * (-1);
end;

function TGlCamera.GetBackVector: TVector3;
begin
  Result := fRotateNormalVector * (-1);
end;

function TGlCamera.GetUpVector: TVector3;
begin
  Result := fRotateDownVector * (-1);
end;

procedure TGlCamera.SetPerspective(const AValue: double);
begin
  if fPerspective = AValue then
    Exit;
  fPerspective := AValue;
  UpdatePerspectiveWidth;
end;

procedure TGlCamera.SetWidth(const AValue: longword);
begin
  if fWidth = AValue then
    Exit;
  fWidth := AValue;
  UpdatePerspectiveWidth;
end;

procedure TGlCamera.SetPosition(const AValue: TVector3);
begin
  fPosition := AValue;
  UpdatePerspectiveWidth;
end;

function TGlCamera.IsVisibled(const Point : TVector3; const Size : double) : boolean;
var
  len : double;
begin
  try
    len := Hypot3(Point - fPosition);
    Result := (Len <= Size) or (ScalarProduct(Point - fPosition,
      fRotateNormalVector) >= len * cos(PerspectiveWidth + Size / len));
  except                            //cos(min(pi, …))  ?
    Result := True;
  end;
end;

procedure TGlCamera.SetMatrix;
var
  m : TMatrix3x3;
  RotationGLMatrix : array[0..15] of double;
  x, y : integer;
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(RadToDeg(fPerspective), fWidth / fHeight, 0.1, 16 * 16);
  glMatrixMode(GL_MODELVIEW);

  glLoadIdentity;
  glRotated(RadToDeg(fRotate[axisZ]), 0, 0, -1);
  glRotated(RadToDeg(fRotate[axisX]), -1, 0, 0);
  glRotated(RadToDeg(fRotate[axisY]), 0, -1, 0);
  glTranslated(-fPosition[axisX], -fPosition[axisY], -fPosition[axisZ]);

  if RotateVectorNeedUpdate then
  begin
    glGetDoublev(GL_MODELVIEW_MATRIX, @RotationGLMatrix);
    for x := 0 to 2 do
      for y := 0 to 2 do
        m[TAxis(y), TAxis(x)] := RotationGLMatrix[4 * x + y];
    fRotateNormalVector := m * Vector3(0, 0, -1);
    fRotateLeftVector := m * Vector3(-1, 0, 0);
    fRotateDownVector := m * Vector3(0, -1, 0);

    RotateVectorNeedUpdate := False;
  end;
end;

constructor TGlCamera.Create;
begin
  inherited;
  Rotate := Vector3(0, 0, 0);
  Position := Vector3(0, 0, 0);
  Perspective := DegToRad(60);
  Width := 640;
  Height := 480;
end;

end.
