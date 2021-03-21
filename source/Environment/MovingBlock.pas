unit MovingBlock;

{$mode objfpc}
interface

{DO NOT USE YET}

uses
  OurUtils,
  OurGame,
  CalcUtils, LightTypes;

type

  { TMovingBlock }

  TMovingBlock = class(TEntity)
  public

  end;

  { TMovingBlockCreator }

  TMovingBlockCreator = class(TElementCreator)
  public
    function GetType: TElementType; override;
    function CreateElement(const Coords: TVector3; const SubID: integer=0): TEnvironmentElement; override;
    function getTextID: ansistring; override;
  end;

procedure RegisterElementCreator(Environment : TEnvironment; Register : TRegisterCreatorMethod);

implementation

procedure RegisterElementCreator(Environment: TEnvironment;
  Register: TRegisterCreatorMethod);
begin
  //Register(TMovingBlockCreator.Create(Environment));
end;

{ TMovingBlockCreator }

function TMovingBlockCreator.GetType: TElementType;
begin
  Result := etEntity;
end;

function TMovingBlockCreator.CreateElement(const Coords: TVector3; const SubID: integer): TEnvironmentElement;
begin
  Result := nil;//TMovingBlock.Create(self);
end;

function TMovingBlockCreator.getTextID: ansistring;
begin
  Result:='MovingBlock';
end;

end.
