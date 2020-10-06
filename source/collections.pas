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
  Classes, SysUtils, Sorts, CalcUtils;

type

  { TCustomCollection }

  generic TCustomCollection<TItem> = class
  public
    function Get(const i : Integer) : TItem; virtual; abstract;
    procedure Add(const Item : TItem); virtual; abstract;
    function GetCount : Integer; virtual; abstract;
    procedure Remove(const i : Integer); virtual; abstract;
    function IndexOf(const Item : TItem) : Integer; virtual; abstract;

    function RemoveItem(const Item : TItem) : boolean; virtual;
    function HasNext(const i : Integer) : Boolean; virtual;
    function GetNext(var i : integer) : TItem; virtual;
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
    procedure Add(const Item: TItem); override;
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
    procedure Add(const Item: TItem); override;
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
    TSorter = specialize TSort<TItem>;
  public
     procedure Sort;
     procedure Remove(const i: Integer); override;
  end;

implementation

{ TCustomOrderArray }

procedure TCustomOrderArray.Sort;
begin
   TSorter.InsertComb(FData, 0, Count);
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

procedure TCustomArray.Add(const Item: TItem);
var
  h : Integer;
begin
  h := length(fData);
  SetLength(fData, h+1);
  fData[h] := Item;
end;

procedure TCustomArray.SetCount(const i: Integer);
begin
  SetLength(fData, i);
end;

function TCustomArray.GetCount: Integer;
begin
  Result := length(fData);
end;

procedure TCustomArray.Remove(const i: Integer);    
var
  h : Integer;
begin
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

{ TCustomSet }

function TCustomSet.Get(const i: Integer): TItem;
begin
  Result := fData[i];
end;

procedure TCustomSet.Add(const Item: TItem);
begin
  if IndexOf(Item) >= 0 then
    exit;
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

