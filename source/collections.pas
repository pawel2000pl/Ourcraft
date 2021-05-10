{
  Copyright (C) 2020 Paweł Bielecki pawelek24@op.pl / pbielecki2000@gmail.com

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}


///TODO: Replace with fgl

unit Collections;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sorts, Incrementations;

type

  { TCustomCollection }

  generic TCustomCollection<TItem> = class abstract
  type
      TItemCollectionEvent = procedure(var Item : TItem) of object;
      TIterationMethod = procedure(Item : TItem) of object;
  private
    FOnCreateItem: TItemCollectionEvent;
    FOnRemoveItem: TItemCollectionEvent;
    procedure SetOnCreateItem(AValue: TItemCollectionEvent);
    procedure SetOnRemoveItem(AValue: TItemCollectionEvent);
  protected
    procedure DoCreateItemEvent(var Item : TItem);
    procedure DoRemoveItemEvent(var Item : TItem);
  public
    property OnCreateItem : TItemCollectionEvent read FOnCreateItem write SetOnCreateItem;
    property OnRemoveItem : TItemCollectionEvent read FOnRemoveItem write SetOnRemoveItem;

    function Get(const i : Integer) : TItem; virtual; abstract;  
    procedure Remove(const i : Integer); virtual; abstract;
    function GetCount : Integer; virtual; abstract;
    procedure Add(Item : TItem); virtual; abstract;
    function IndexOf(const Item : TItem) : Integer; virtual; abstract;
    procedure Clear; virtual; abstract;

    procedure AddCollection(const Collection : TCustomCollection); virtual;
    procedure Iterate(const Event : TIterationMethod);
    function RemoveItem(const Item : TItem) : boolean; virtual;
    function HasNext(const i : Integer) : Boolean; virtual;
    function GetNext(var i : integer) : TItem; overload; virtual;
    function GetNext(var i : integer; var Item : TItem) : Boolean; overload; virtual;

    constructor Create;
  end;

  { TCustomSet }

  generic TCustomSet<TItem> = class(specialize TCustomCollection<TItem>)
  type
    TMySearcher = specialize TMemoryBSearch<TItem>;
  private
    Count : Integer;
    FData : array of TItem;
  public
    function Get(const i: Integer): TItem; override;
    procedure Add(Item: TItem); override;
    function GetCount: Integer; override;
    procedure Remove(const i: Integer); override;
    function IndexOf(const Item: TItem): Integer; override;
    procedure Clear; override;
    constructor Create;
    destructor Destroy; override;
  end;

  { TCustomArray }

  generic TCustomArray<TItem> = class(specialize TCustomCollection<TItem>)
  private
    fData : array of TItem;
  public
    function Get(const i: Integer): TItem; override;
    procedure SetItem(const i : Integer; const NewItem : TItem);
    procedure Add(Item: TItem); override;
    procedure Insert(Item : TItem; const Index : Integer); virtual;
    procedure SetCount(const i : Integer);
    function GetCount: Integer; override;
    procedure Clear; override;
    procedure Remove(const i: Integer); override;
    function IndexOf(const Item: TItem): Integer; override;
    property Data[const Index : Integer] : TItem read Get write SetItem; default;
    property Count : Integer read GetCount write SetCount;
    constructor Create(InitCount : Integer=0);
    constructor Create(const InitValues : array of TItem);
    destructor Destroy; override;
  end;

  { TCustomOrderArray }

  generic TCustomOrderArray<TItem> = class(specialize TCustomArray<TItem>)
  type      
    TStaticSorter = class(specialize TStaticSort<TItem>) 
    {$if (FPC_VERSION >= 3) and (FPC_RELEASE >= 2)}
    type
        TValue = TItem;
    {$ENDIF}
    public
      class function Compare(const a, b: TValue): integer; override;
    end;
    TSorter = class(specialize TSort<PtrUInt>)   
    {$if (FPC_VERSION >= 3) and (FPC_RELEASE >= 2)}
    type
        TValue = PtrUInt;
    {$ENDIF}
    public
      function Compare(const a, b: TValue): integer; override;
    end;
  public
     class function CompareItems(const a, b: TItem): integer; virtual;
     procedure Sort;
     procedure GetSortedIndexList(var List : array of PtrUInt); // todo: test
     procedure Remove(const i: Integer); override;
  end;

  ENotContainKey = class(Exception);

  { TCustomKeySet }

  generic TCustomKeySet<TKey, TItem> = class
  type               
      TItemContainer = record
        Key : TKey;
        Item : TItem;
      end;

      TBSearchComparator = function(const a : TItemContainer; const b : TKey) : Integer of object; 
      TIterationMethod = procedure(const Key : TKey; Item : TItem) of object;
      TKeyList = specialize TCustomArray<TKey>;

      { TSearcher }

      TSearcher = class(specialize TBSearch<TItemContainer, TKey>)
      private
        FComaprator : TBSearchComparator;
      public
        function Compare(const a: TItemContainer; const b: TKey): integer; override;
        constructor Create(const Comparator : TBSearchComparator);
      end;

  private
    BSearcher : TSearcher;
    FCount : Integer;
    FData : array of TItemContainer;
    function BSearchCompare(const Container : TItemContainer; const Key : TKey) : Integer;  
    procedure AddNew(const Key : TKey; const Item : TItem);
  public
    function CompareKeys(const a, b : TKey) : Integer; virtual;

    property Count : Integer read FCount;
    function GetCount : Integer;
    function ContainKey(const Key : TKey) : Boolean;
    procedure Add(const Key : TKey; const Item : TItem);
    procedure Remove(const Key : TKey);
    function Get(const Key : TKey) : TItem; overload;
    property Data[const Key : TKey] : TItem read Get write Add; default;
    procedure KeyList(List : TKeyList);
    function KeyList : TKeyList; //must be free after
    procedure Iterate(const Event : TIterationMethod);

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TCustomKeySet }

function TCustomKeySet.BSearchCompare(const Container: TItemContainer;
  const Key: TKey): Integer;
begin
  Result := CompareKeys(Container.Key, Key);
end;

function TCustomKeySet.CompareKeys(const a, b: TKey): Integer;
begin
  Result := CompareMemRange(@a, @b, sizeof(TKey));
end;

function TCustomKeySet.GetCount: Integer;
begin
  Result := FCount;
end;

function TCustomKeySet.ContainKey(const Key: TKey): Boolean;
begin
  Result := BSearcher.BSearch(FData, Key, 0, FCount-1) >= 0;
end;

procedure TCustomKeySet.AddNew(const Key: TKey; const Item: TItem);
var
  i : Integer;
begin
  i := FCount-1;
  SetLength(FData, PreInc(FCount));
  while BSearchCompare(FData[i], Key) > 0 do
  begin
    FData[i+1] := FData[i];
    Dec(i);
  end;
  FData[i+1].Key := Key;
  FData[i+1].Item := Item;
end;

procedure TCustomKeySet.Add(const Key: TKey; const Item: TItem);
var
   i : Integer;
begin
   i := BSearcher.BSearch(FData, Key, 0, FCount-1);
   if i < 0 then
     Add(Key, Item)
     else
     FData[i].Item:=Item;
end;

procedure TCustomKeySet.Remove(const Key: TKey);
var
   i, m : Integer;
begin
  i := BSearcher.BSearch(FData, Key, 0, FCount-1);
  if i < 0 then
    exit;
  for m := i+1 to Count-1 do
    fData[m-1] := fData[m];
  SetLength(fData, PreDec(FCount));
end;

function TCustomKeySet.Get(const Key: TKey): TItem;
var
   i : Integer;
begin
   i := BSearcher.BSearch(FData, Key, 0, FCount-1);
   if i < 0 then
     raise ENotContainKey.Create('Key not found');
   Result := FData[i].Item;
end;

procedure TCustomKeySet.KeyList(List: TKeyList);
var
   i : Integer;
begin
  for i := 0 to FCount-1 do
    List.Add(FData[i].Key);
end;

function TCustomKeySet.KeyList: TKeyList;
begin
  Result := TKeyList.Create();
  KeyList(Result);
end;

procedure TCustomKeySet.Iterate(const Event: TIterationMethod);
var
   i : Integer;
begin
  for i := 0 to FCount-1 do
    Event(FData[i].Key, FData[i].Item);
end;

constructor TCustomKeySet.Create;
begin
  BSearcher:=TSearcher.Create(@BSearchCompare);
  setlength(FData, 0);
  FCount := 0;
end;

destructor TCustomKeySet.Destroy;
begin
  BSearcher.Free;   
  SetLength(FData, 0);
  inherited Destroy;
end;

{ TCustomKeySet.TSearcher }

function TCustomKeySet.TSearcher.Compare(const a: TItemContainer; const b: TKey
  ): integer;
begin
  Result := FComaprator(a, b);
end;

constructor TCustomKeySet.TSearcher.Create(const Comparator: TBSearchComparator);
begin
  FComaprator:=Comparator;
end;

{ TCustomOrderArray.TSorter }

function TCustomOrderArray.TSorter.Compare(const a, b: TValue): integer;
begin
  Result := TStaticSorter.Compare(FData[a], FData[b]);
end;

{ TCustomOrderArray.TStaticSorter }

class function TCustomOrderArray.TStaticSorter.Compare(const a, b: TValue): integer;
begin
  Result := CompareItems(a, b);
end;

{ TCustomOrderArray }

class function TCustomOrderArray.CompareItems(const a, b: TItem): integer;
begin
  Result := CompareMemRange(@a, @b, sizeof(TItem));
end;

procedure TCustomOrderArray.Sort;
begin
   TStaticSorter.InsertComb(FData, 0, Count);
end;

procedure TCustomOrderArray.GetSortedIndexList(var List: array of PtrUInt);
var
  Sorter : TSorter;
begin              
  Sorter := TSorter.Create;
  Sorter.InsertComb(List);
  Sorter.Free;
end;

procedure TCustomOrderArray.Remove(const i: Integer);
var
  m : Integer;
begin
  for m := i+1 to High(fData) do
    fData[m-1] := fData[m];
  SetLength(fData, High(FData));
end;

{ TCustomArray }

function TCustomArray.Get(const i: Integer): TItem;
begin
  Result := fData[i];
end;

procedure TCustomArray.SetItem(const i: Integer; const NewItem: TItem);
begin
  fData[i] := NewItem;
end;

procedure TCustomArray.Add(Item: TItem);
var
  h : Integer;
begin
  DoCreateItemEvent(Item);
  h := length(fData);
  SetLength(fData, h+1);
  fData[h] := Item;
end;

procedure TCustomArray.Insert(Item: TItem; const Index: Integer);
var
  i, h : Integer;
begin
  DoCreateItemEvent(Item);  
  h := length(fData);
  SetLength(fData, h+1);
  for i := h downto Index+1 do
    fData[i] := fData[i-1];
  fData[Index] := Item;
end;

procedure TCustomArray.SetCount(const i: Integer);
var
  j, oc : Integer;
begin             
  oc := GetCount;
  if (FOnRemoveItem <> nil) and Assigned(FOnRemoveItem) and (i<oc) then
    for j := i to oc-1 do
      FOnRemoveItem(fData[j]);
  SetLength(fData, i);   
  if (FOnCreateItem <> nil) and Assigned(FOnCreateItem) and (i>oc) then
    for j := oc to i-1 do
      FOnCreateItem(fData[j]);
end;

function TCustomArray.GetCount: Integer;
begin
  Result := length(fData);
end;

procedure TCustomArray.Clear;
var
  i : Integer;
begin
  if OnRemoveItem <> nil then
    for i := 0 to GetCount-1 do
      DoRemoveItemEvent(FData[i]);
  SetLength(fData, 0);
end;

procedure TCustomArray.Remove(const i: Integer);    
var
  h : Integer;
begin
  DoRemoveItemEvent(fData[i]);
  h := High(fData);
  fData[i] := fData[h];
  setlength(fData, h);
end;

function TCustomArray.IndexOf(const Item: TItem): Integer;
var
  i : Integer;
begin
  for i := low(fData) to High(fData) do
    if CompareMem(@fData[i], @Item, SizeOf(TItem)) then
      exit(i);
  exit(-1);
end;

constructor TCustomArray.Create(InitCount: Integer);
begin
  inherited Create;
  SetCount(InitCount);
end;

constructor TCustomArray.Create(const InitValues: array of TItem);
var
  i, c : Integer;
begin
  inherited Create;
  c := length(InitValues);
  SetCount(c);
  for i := 0 to c-1 do
    fData[i] := InitValues[i];
end;

destructor TCustomArray.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{ TCustomCollection }

procedure TCustomCollection.SetOnCreateItem(AValue: TItemCollectionEvent);
begin
  if FOnCreateItem=AValue then Exit;
  if not Assigned(AValue) then
    AValue := nil;
  FOnCreateItem:=AValue;
end;

procedure TCustomCollection.SetOnRemoveItem(AValue: TItemCollectionEvent);
begin
  if FOnRemoveItem=AValue then Exit;  
  if not Assigned(AValue) then
    AValue := nil;
  FOnRemoveItem:=AValue;
end;

procedure TCustomCollection.DoCreateItemEvent(var Item: TItem);
begin
  if FOnCreateItem <> nil then
    FOnCreateItem(Item);
end;

procedure TCustomCollection.DoRemoveItemEvent(var Item: TItem);
begin
  if FOnRemoveItem <> nil then
    FOnRemoveItem(Item);
end;

procedure TCustomCollection.AddCollection(const Collection : TCustomCollection);
var
  i : integer;
begin
  for i := 0 to Collection.GetCount-1 do
    Add(Collection.Get(i));
end;

procedure TCustomCollection.Iterate(const Event: TIterationMethod);
var
  i : Integer;
begin
  i := 0;
  while HasNext(i) do
    Event(Get(i));
end;

function TCustomCollection.RemoveItem(const Item: TItem): boolean;
var
  i : Integer;
begin
  i := IndexOf(Item);
  if i < 0 then
    exit(False);
  Remove(i);
  Result := true;
end;

function TCustomCollection.HasNext(const i: Integer): Boolean;
begin
  Result := i<GetCount;
end;

function TCustomCollection.GetNext(var i: integer): TItem;
begin
  Result := Get(PostInc(i));
end;

function TCustomCollection.GetNext(var i: integer; var Item: TItem): Boolean;
begin
  Result := HasNext(i);
  if Result then
    Item := GetNext(i);
end;

constructor TCustomCollection.Create;
begin
  FOnCreateItem:=nil;
  FOnRemoveItem:=nil;
end;

{ TCustomSet }

function TCustomSet.Get(const i: Integer): TItem;
begin
  Result := fData[i];
end;

procedure TCustomSet.Add(Item: TItem);
var
  i : Integer;
begin
  if IndexOf(Item) >= 0 then
    exit;               
  DoCreateItemEvent(Item);   
  i := Count-1;
  SetLength(FData, PreInc(Count));
  while TMySearcher.Compare(FData[i], Item) > 0 do
  begin
    FData[i+1] := FData[i];
    Dec(i);
  end;
  FData[i+1] := Item;
end;

function TCustomSet.GetCount: Integer;
begin
  Result := Count;
end;

procedure TCustomSet.Remove(const i: Integer);
var
  m : Integer;
begin
  DoRemoveItemEvent(FData[i]);
  for m := i+1 to Count-1 do
    fData[m-1] := fData[m];
  SetLength(fData, PreDec(Count));
end;

function TCustomSet.IndexOf(const Item: TItem): Integer;
begin
  Result := TMySearcher.BSearch(FData, Item, 0, Count-1);
end;

procedure TCustomSet.Clear;
var
  i : Integer;
begin
  if OnRemoveItem <> nil then
    for i := 0 to GetCount-1 do
      DoRemoveItemEvent(FData[i]);  
  SetLength(fData, 0);
end;

constructor TCustomSet.Create;
begin
  Count := 0;
  SetLength(FData, 0);
end;

destructor TCustomSet.Destroy;
begin
  Clear;
  inherited Destroy;
end;

end.

