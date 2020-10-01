//This file was generated automatically
unit BlocksLoader;

{$mode objfpc}{$inline on}

interface

uses
  OurUtils, Sorts;

type
  TBlockCreatorArray = array of TBlockCreator;

  { TBlockCreatorSort }

  TBlockCreatorSort = class(specialize TSort<TBlockCreator>)
  public
    class function Compare(const a, b : TValue) : integer; override;
  end;

  { TBlockCreatorSearcher }

  TBlockCreatorSearcher = class(specialize TBSearch<TBlockCreator, ansistring>)
  public
    class function Compare(const a : TValue; const b : TKey) : integer; override;
  end;

  { TBlocksLoader }

  TBlocksLoader = class
  private
    fCount : integer;
    fOffset : integer;
    fLoadedBlocks : TBlockCreatorArray;
    fGame : TAbstractGame;
    procedure LoadBlock(Loader : TCustomCreator);
    procedure FreeBlockCreators;
    procedure LoadBlocks(var IdOffset : integer);
  public
    property Count : integer read fCount;
    property Offset : integer read fOffset;

    procedure AfterLoadedEvent;

    function GetLoader(const ID : integer) : TBlockCreator;
    function GetLoader(const ID : ansistring) : TBlockCreator;

    constructor Create(var IdOffset : integer; OurGame : TAbstractGame);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, Math, OurGame, air, glowstone, grass, stone;

{ TBlockCreatorSearcher }

class function TBlockCreatorSearcher.Compare(const a : TValue; const b : TKey) : integer;
begin
  Result := CompareText(a.getTextID, b);
end;

{ TBlockCreatorSort }

class function TBlockCreatorSort.Compare(const a, b : TValue) : integer;
begin
  Result := CompareText(a.getTextID, b.getTextID);
end;

procedure TBlocksLoader.LoadBlock(Loader : TCustomCreator);
begin
  if not (Loader is TBlockCreator) then
    exit;
  Inc(fCount);
  setlength(fLoadedBlocks, fCount);
  fLoadedBlocks[fCount - 1] := Loader as TBlockCreator;
  Writeln('Loaded class: ', Loader.ClassName, #9'[block]');
end;

procedure TBlocksLoader.LoadBlocks(var IdOffset : integer);
var
  i : integer;
begin
  if fCount <> -1 then
    exit;
  fOffset := IdOffset;
  fCount := 0;
      air.LoadBlocks(@LoadBlock, fGame);
    glowstone.LoadBlocks(@LoadBlock, fGame);
    grass.LoadBlocks(@LoadBlock, fGame);
    stone.LoadBlocks(@LoadBlock, fGame);
    Inc(IdOffset, fCount);
  TBlockCreatorSort.InsertComb(fLoadedBlocks, 0, fCount);
  for i := 0 to fCount - 1 do
    fLoadedBlocks[i].ID := i + fOffset;
end;

procedure TBlocksLoader.AfterLoadedEvent;
var
  i : integer;
begin
  if fCount = -1 then
    exit;
  writeln('Processing blocks...');
  for i := 0 to fCount - 1 do
    fLoadedBlocks[i].AfterLoading;
end;

function TBlocksLoader.GetLoader(const ID : integer) : TBlockCreator;
begin
  Result := fLoadedBlocks[ID - fOffset];
end;

function TBlocksLoader.GetLoader(const ID : ansistring) : TBlockCreator;
begin
  Result := fLoadedBlocks[max(0, TBlockCreatorSearcher.BSearch(
    fLoadedBlocks, ID, 0, fCount - 1))];
end;

procedure TBlocksLoader.FreeBlockCreators;
var
  i : integer;
begin
  for i := 0 to fCount - 1 do
    fLoadedBlocks[i].Free;
  setlength(fLoadedBlocks, 0);
  fCount := -1;
end;

constructor TBlocksLoader.Create(var IdOffset : integer; OurGame : TAbstractGame);
begin
  fCount := -1;
  fOffset := 0;
  fGame := OurGame;
  LoadBlocks(IdOffset);
end;

destructor TBlocksLoader.Destroy;
begin
  FreeBlockCreators;
  inherited Destroy;
end;

end.
