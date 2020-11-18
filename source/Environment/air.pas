unit air;

{$mode objfpc}
interface

uses
  OurUtils,
  OurGame,
  CalcUtils;

type

  { TAir }

  TAir = class(TBlock)
  public
    function Transparency : Integer; override;
    function NeedDraw: boolean; override;
  end;

  { TAirCreator }

  TAirCreator = class(TBlockCreator)
  public
    function CreateElement(const Coords: TVector3; const SubID: integer=0): TEnvironmentElement; override;
    function getTextID: ansistring; override;
  end;

procedure RegisterElementCreator(Environment : TEnvironment; Register : TRegisterCreatorMethod);

implementation

procedure RegisterElementCreator(Environment: TEnvironment;
  Register: TRegisterCreatorMethod);
begin
  Register(TAirCreator.Create(Environment));
end;

{ TAirCreator }

function TAirCreator.CreateElement(const Coords: TVector3; const SubID: integer
  ): TEnvironmentElement;
begin
  Result := TAir.Create(self);
end;

function TAirCreator.getTextID: ansistring;
begin
  Result:='Air';
end;

{ TAir }

function TAir.Transparency : Integer;
begin
  Result := MAX_BLOCK_TRANSPARENCY;
end;

function TAir.NeedDraw: boolean;
begin
  Result:=false;
end;

end.
