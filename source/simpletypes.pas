unit SimpleTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;    {TODO: more functions? or delete this module?}

type

  { TGenericNumber }

  generic TGenericNumber<TNumber> = class
  private
    FValue : TNumber;
  public
    procedure Increase(const AValue : TNumber); inline; overload;
    procedure Decrease(const AValue : TNumber); inline; overload;
    function PostInc(const AValue : TNumber) : TNumber; inline; overload;
    function PostDec(const AValue : TNumber) : TNumber; inline; overload;
    function PreInc(const AValue : TNumber) : TNumber; inline; overload;
    function PreDec(const AValue : TNumber) : TNumber; inline; overload;

    property Value : TNumber read FValue write FValue;
    function Sign : integer; inline; overload;
    constructor Create(const InitValue : TNumber = 0);
  end;

  { TGenericOrdinal }

  generic TGenericOrdinal<TOrdinal> = class(specialize TGenericNumber<TOrdinal>)
  public
    procedure Increase; inline; overload;
    procedure Decrease; inline; overload;
    function PostInc : TOrdinal; inline; overload;
    function PostDec : TOrdinal; inline; overload;
    function PreInc : TOrdinal; inline; overload;
    function PreDec : TOrdinal; inline; overload;
  end;

  TInteger = specialize TGenericOrdinal<integer>;
  TQWord = specialize TGenericOrdinal<QWord>;

  TDouble = specialize TGenericNumber<Double>;
  TExtended = specialize TGenericNumber<Extended>;

implementation

{ TGenericOrdinal }

procedure TGenericOrdinal.Increase;
begin
  Inc(FValue);
end;

procedure TGenericOrdinal.Decrease;
begin
  Dec(FValue);
end;

function TGenericOrdinal.PostInc : TOrdinal;
begin
  Result := FValue;
  Increase;
end;

function TGenericOrdinal.PostDec : TOrdinal;
begin
  Result := FValue;
  Decrease;
end;

function TGenericOrdinal.PreInc : TOrdinal;
begin
  Increase;
  Result := FValue;
end;

function TGenericOrdinal.PreDec : TOrdinal;
begin
  Decrease;
  Result := FValue;
end;

{ TGenericNumber }

procedure TGenericNumber.Increase(const AValue : TNumber);
begin
  FValue += AValue;
end;

procedure TGenericNumber.Decrease(const AValue : TNumber);
begin
  FValue -= AValue;
end;

function TGenericNumber.PostInc(const AValue : TNumber) : TNumber;
begin
  Result := FValue;
  Increase(AValue);
end;

function TGenericNumber.PostDec(const AValue : TNumber) : TNumber;
begin
  Result := FValue;
  Decrease(AValue);
end;

function TGenericNumber.PreInc(const AValue : TNumber) : TNumber;
begin
  Increase(AValue);
  Result := FValue;
end;

function TGenericNumber.PreDec(const AValue : TNumber) : TNumber;
begin
  Decrease(AValue);
  Result := FValue;
end;

function TGenericNumber.Sign : Integer;
begin
  if FValue < 0 then
    Result := -1
  else if FValue > 0 then
    Result := 1
  else
    Result := 0;
end;

constructor TGenericNumber.Create(const InitValue : TNumber);
begin
  FValue := InitValue;
end;

end.

