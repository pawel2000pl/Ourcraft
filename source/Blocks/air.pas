unit air;

{$mode objfpc}
interface

uses
  OurUtils;

type

  { TAir }

  TAir = class(TBlock)
  public
    function Clone : TBlock; override;
    function Transparency : Integer; override;
    function NeedDraw: boolean; override;
  end;

  { TAirCreator }

  TAirCreator = class(TBlockCreator)
  public
    function CreateNew(const SubID : integer) : TObject; override;
    function getTextID: ansistring; override;

  end;

procedure LoadBlocks(Register : TCreatorRegister; OurGame : TAbstractGame);

implementation

uses
  OurGame;

procedure LoadBlocks(Register : TCreatorRegister; OurGame : TAbstractGame);
begin
  Register(TAirCreator.Create(OurGame));
end;

{ TAirCreator }

function TAirCreator.CreateNew(const SubID : integer) : TObject;
begin
  Result := TAir.Create(self);
end;

function TAirCreator.getTextID: ansistring;
begin
  Result:='Air';
end;

{ TAir }

function TAir.Clone : TBlock;
begin
  Result := Creator.CreateNew(Self.getSubID) as TBlock;
end;

function TAir.Transparency : Integer;
begin
  Result := MAX_BLOCK_TRANSPARENCY;
end;

function TAir.NeedDraw: boolean;
begin
  Result:=false;
end;

end.
