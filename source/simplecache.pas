unit SimpleCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { In case of collision, older keys will be removed }

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
    function GetHash(const Key : TKey) : UInt32; virtual;
    function SameKeys(const a, b : TKey) : Boolean; virtual;
  public
    function GetIndex(const Key : TKey) : PtrUInt; inline;
    procedure AddItem(const Key : TKey; const Value : TValue);
    function GetItem(const Key : TKey; var Value : TValue) : boolean;
    constructor Create(const Size : integer = 93179);
    destructor Destroy; override;
  end;

function ModuloBuf(const Buf : Pointer; const Size : PtrUInt; const InitValue : PtrUInt = 0; const Base : LongWord = 4294967291) : LongWord;

implementation

function ModuloBuf(const Buf : Pointer; const Size : PtrUInt; const InitValue : PtrUInt = 0; const Base : LongWord = 4294967291) : LongWord;
var
  i : PtrUInt;
begin
  Result := InitValue;
  Move(PByte(buf)[Size and (not 3)], Result, Size and 3);
  for i := (Size shr 2) -1 downto 0 do
    Result := ((QWord(Result) shl 32) or PLongWord(Buf)[i]) mod Base;
end;

{ TSimpleCache }

function TSimpleCache.CreateRecord(const Key : TKey; const Value : TValue) : TRecord;
begin
  Result.Key := Key;
  Result.Value := Value;
  Result.Exists := True;
end;

function TSimpleCache.GetHash(const Key: TKey): UInt32;
begin
  Result := ModuloBuf(@Key, Sizeof(TKey));
end;

function TSimpleCache.SameKeys(const a, b: TKey): Boolean;
begin
  Result := CompareMem(@a, @b, SizeOf(TKey));
end;

function TSimpleCache.GetIndex(const Key: TKey): PtrUInt;
begin
  Result := GetHash(Key) mod Count;
end;

procedure TSimpleCache.AddItem(const Key : TKey; const Value : TValue);
begin
  fData[GetIndex(Key)] := CreateRecord(Key, Value);
end;

function TSimpleCache.GetItem(const Key : TKey; var Value : TValue) : boolean;
var
  Index : integer;
begin
  Index := GetIndex(Key);
  Result := (fData[Index].Exists and SameKeys(fData[Index].Key, Key));
  if Result then
     Value := fData[Index].Value;
end;

constructor TSimpleCache.Create(const Size : integer);
begin
  inherited Create;
  Count := Size;
  setlength(fData, Count);
end;

destructor TSimpleCache.Destroy;
begin
  Setlength(fData, 0);
  inherited Destroy;
end;

end.

