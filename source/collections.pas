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

unit Collections;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sorts, CalcUtils, Incrementations;

type

  { TCustomCollection }

  generic TCustomCollection<TItem> = class
  type
      TItemCollectionEvent = procedure(var Item : TItem) of object;
      TIterationMethod = procedure(Item : TItem) of object;
  private
    FOnAdd: TItemCollectionEvent;
    FOnRemove: TItemCollectionEvent;
    procedure SetOnAdd(AValue: TItemCollectionEvent);
    procedure SetOnRemove(AValue: TItemCollectionEvent);
  protected
    procedure DoAddEvent(var Item : TItem);
    procedure DoRemoveEvent(var Item : TItem);
  public
    property OnAdd : TItemCollectionEvent read FOnAdd write SetOnAdd;
    property OnRemove : TItemCollectionEvent read FOnRemove write SetOnRemove;

    function Get(const i : Integer) : TItem; virtual; abstract;  
    procedure Remove(const i : Integer); virtual; abstract;
    function GetCount : Integer; virtual; abstract;
    procedure Add(Item : TItem); virtual; abstract;
    function IndexOf(const Item : TItem) : Integer; virtual; abstract;

    procedure Iterate(const Event : TIterationMethod);
    function RemoveItem(const Item : TItem) : boolean; virtual;
    function HasNext(const i : Integer) : Boolean; virtual;
    function GetNext(var i : integer) : TItem; virtual;

    constructor Create;
  end;

  { TCustomSet }

  generic TCustomSet<TItem> = class(specialize TCustomCollection<TItem>)
  type
    TMySorter = specialize TMemorySorter<TItem>;
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
    procedure SetCount(const i : Integer);
    function GetCount: Integer; override;
    procedure Remove(const i: Integer); override;
    function IndexOf(const Item: TItem): Integer; override;
    property Data[const Index : Integer] : TItem read Get write SetItem; default;
    property Count : Integer read GetCount write SetCount;    
    constructor Create(InitCount : Integer);
  end;

  { TCustomOrderArray }

  generic TCustomOrderArray<TItem> = class(specialize TCustomArray<TItem>)
  type      
    TStaticSorter = class(specialize TStaticSort<TItem>)
    public
      class function Compare(const a, b: TValue): integer; override;
    end;
    TSorter = class(specialize TSort<PtrUInt>)
    public
      function Compare(const a, b: TValue): integer; override;
    end;
  public
     class function CompareItems(const a, b: TItem): integer; virtual;
     procedure Sort;
     procedure GetSortedIndexList(var List : array of PtrUInt); // todo: test
     procedure Remove(const i: Integer); override;
  end;

implementation

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
  DoAddEvent(Item);
  h := length(fData);
  SetLength(fData, h+1);
  fData[h] := Item;
end;

procedure TCustomArray.SetCount(const i: Integer);
var
  j, oc : Integer;
begin             
  oc := GetCount;
  if (FOnRemove <> nil) and Assigned(FOnRemove) and (i<oc) then
    for j := i to oc-1 do
      FOnRemove(fData[j]);
  SetLength(fData, i);   
  if (FOnAdd <> nil) and Assigned(FOnAdd) and (i>oc) then
    for j := oc to i-1 do
      FOnAdd(fData[j]);
end;

function TCustomArray.GetCount: Integer;
begin
  Result := length(fData);
end;

procedure TCustomArray.Remove(const i: Integer);    
var
  h : Integer;
begin
  DoRemoveEvent(fData[i]);
  h := High(fData);
  fData[i] := fData[h];
  setlength(fData, h);
end;

function TCustomArray.IndexOf(const Item: TItem): Integer;
var
  i : Integer;
begin
  for i := low(fData) to High(fData) do
    if fData[i] = Item then
      exit(i);
  exit(-1);
end;

constructor TCustomArray.Create(InitCount: Integer);
begin
  inherited Create;
  SetCount(InitCount);
end;

{ TCustomCollection }

procedure TCustomCollection.SetOnAdd(AValue: TItemCollectionEvent);
begin
  if FOnAdd=AValue then Exit;
  if not Assigned(AValue) then
    AValue := nil;
  FOnAdd:=AValue;
end;

procedure TCustomCollection.SetOnRemove(AValue: TItemCollectionEvent);
begin
  if FOnRemove=AValue then Exit;  
  if not Assigned(AValue) then
    AValue := nil;
  FOnRemove:=AValue;
end;

procedure TCustomCollection.DoAddEvent(var Item: TItem);
begin
  if FOnAdd <> nil then
    FOnAdd(Item);
end;

procedure TCustomCollection.DoRemoveEvent(var Item: TItem);
begin
  if FOnRemove <> nil then
    FOnRemove(Item);
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
  Result := i+1<GetCount;
end;

function TCustomCollection.GetNext(var i: integer): TItem;
begin
  Result := Get(PreInc(i));
end;

constructor TCustomCollection.Create;
begin
  FOnAdd:=nil;
  FOnRemove:=nil;
end;

{ TCustomSet }

function TCustomSet.Get(const i: Integer): TItem;
begin
  Result := fData[i];
end;

procedure TCustomSet.Add(Item: TItem);
begin
  if IndexOf(Item) >= 0 then
    exit;               
  DoAddEvent(Item);
  SetLength(FData, PreInc(Count));
  FData[Count-1] := Item;
  TMySorter.Insert(FData, 0, Count);
end;

function TCustomSet.GetCount: Integer;
begin
  Result := Count;
end;

procedure TCustomSet.Remove(const i: Integer);
var
  m : Integer;
begin
  DoRemoveEvent(FData[i]);
  for m := i+1 to Count-1 do
    fData[m-1] := fData[m];
  SetLength(fData, PreDec(Count));
end;

function TCustomSet.IndexOf(const Item: TItem): Integer;
begin
  Result := TMySearcher.BSearch(FData, Item, 0, Count-1);
end;

constructor TCustomSet.Create;
begin
  Count := 0;
  SetLength(FData, 0);
end;

destructor TCustomSet.Destroy;
begin
  SetLength(fData, 0);
  inherited Destroy;
end;

end.

