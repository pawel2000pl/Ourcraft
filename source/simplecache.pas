unit SimpleCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, crc;

type

  { In case of collision, older file will be removed }

  { TSimpleCache }

  generic TSimpleCache<TKey, TValue> = class
    type
    TRecord = record
      Exists : boolean;
      Key : TKey;
      Value : TValue;
    end;
  private
    Count : integer;
    fData : array of TRecord;
    function CreateRecord(const Key : TKey; const Value : TValue) : TRecord; inline;
  protected
    function GetHash(const Key : TKey) : integer; virtual;
    function SameKeys(const a, b : TKey) : Boolean; virtual;
  public
    procedure AddItem(const Key : TKey; const Value : TValue);
    function GetItem(const Key : TKey; var Value : TValue) : boolean;
    constructor Create(const Size : integer = 93179);
    destructor Destroy; override;
  end;

implementation

{ TSimpleCache }

function TSimpleCache.CreateRecord(const Key : TKey; const Value : TValue) : TRecord;
begin
  Result.Key := Key;
  Result.Value := Value;
  Result.Exists := True;
end;

function TSimpleCache.GetHash(const Key : TKey) : integer;
begin
  Result := crc32(613, @Key, SizeOf(TKey));
end;

function TSimpleCache.SameKeys(const a, b: TKey): Boolean;
begin
  Result := CompareMem(@a, @b, SizeOf(TKey));
end;

procedure TSimpleCache.AddItem(const Key : TKey; const Value : TValue);
begin
  fData[GetHash(Key) mod Count] := CreateRecord(Key, Value);
end;

function TSimpleCache.GetItem(const Key : TKey; var Value : TValue) : boolean;
var
  Index : integer;
begin
  Index := GetHash(Key) mod Count;
  Result := (fData[Index].Exists and SameKeys(fData[Index].Key, Key));
  if Result then
     Value := fData[Index].Value;
end;

constructor TSimpleCache.Create(const Size : integer);
begin
  Count := Size;
  setlength(fData, Count);
end;

destructor TSimpleCache.Destroy;
begin
  Setlength(fData, 0);
  inherited Destroy;
end;

end.

