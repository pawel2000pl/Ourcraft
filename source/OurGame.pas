unit OurGame;

{$mode objfpc}
interface

uses
  SysUtils, Classes,
  OurUtils, Models,
  BlocksLoader;

type

  { TOurGame }

  TOurGame = class(TAbstractGame)
  private
    Air : TBlockCreator;
    fTextures : TTextureManager;
    fIDCount : Integer;
    BlocksID : TBlocksLoader;
  public
    property Textures : TTextureManager read fTextures;

    function GetCreator(const ID : integer) : TCustomCreator; override;
    function GetID(const Name: AnsiString): Integer; override;
    function GetIDCount: Integer; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TOurGame }

function TOurGame.GetCreator(const ID : integer) : TCustomCreator;
begin
  if ID = 0 then
  begin
    Result := Air;
    exit;
  end;
  if (ID >= BlocksID.Offset) and (ID < BlocksID.Offset + BlocksID.Count) then
    Result := BlocksID.GetLoader(ID)
  else
    Result := nil;
  //ToDo: Items, Entities
end;

function TOurGame.GetID(const Name: AnsiString): Integer;
begin
  Result := BlocksID.GetLoader(Name).GetID;
  //if Result = nil {TODO:  Items, entities}
end;

function TOurGame.GetIDCount: Integer;
begin
  Result := fIDCount;
end;

constructor TOurGame.Create;
var
  Offset : integer;
begin
  fTextures := TTextureManager.Create();

  Offset := 1;
  BlocksID := TBlocksLoader.Create(Offset, self);
  Air := BlocksID.GetLoader('air');
  //TODO: Items, entities

  fIDCount:=BlocksID.Count; //TODO: suma reszty

  BlocksID.AfterLoadedEvent;

  writeln('OurGame was started');
end;

destructor TOurGame.Destroy;
begin
  BlocksID.Free;
  fTextures.Free;
  inherited Destroy;
end;

end.
