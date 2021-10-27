unit TinyHashData;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DataAbstractions;

type

  { TTinyHashEnumerator }

  generic TTinyHashEnumerator<TOwner, TBoxType, TEnumeratorType> = class abstract(specialize TCustomEnumerator<TEnumeratorType>)
  public
    HashIndex, Index : Integer;
    Box : TBoxType;
    Modified : Boolean;
    Owner : TOwner;
  protected
    function GetCurrentPointer : Pointer; override;
  public
    function MoveNext: Boolean; override;
    constructor Create(const S : TOwner);
    destructor Destroy; override;
  end;

  { TTinyHashData }

  generic TTinyHashData<TKey, TValue, TBoxType> = class abstract(specialize TCustomDataContainer<TKey, TValue>)
  const
    MinHashSize = 13;
    MaxHashSize = High(LongWord);
  type

    { TTinyValueEnumerator }

    TTinyValueEnumerator = class(specialize TTinyHashEnumerator<TTinyHashData, TBoxType, TValue>)
    protected
       function GetCurrent: TValue; override;
    end;

    { TTinyKeyEnumerator }

    TTinyKeyEnumerator = class(specialize TTinyHashEnumerator<TTinyHashData, TBoxType, TKey>)
    protected
       function GetCurrent: TKey; override;
    end;

    { TTinyKeyValueEnumerator }

    TTinyKeyValueEnumerator = class(specialize TTinyHashEnumerator<TTinyHashData, TBoxType, TKeyValuePair>)
    protected
       function GetCurrent: TKeyValuePair; override;
    end;

  private
    FCount : PtrUint;
    FHashRange : PtrUInt;
    FData : array of array of TBoxType;   
    FAutoOptimizing : Boolean;
    procedure SetHashRange(const AValue: PtrUInt);
    class var InitCRC32Code : LongWord;
  protected
    procedure AddBox(Box : TBoxType);
    procedure OnSetComparator; override;
  public
    property HashRange : PtrUInt read FHashRange write SetHashRange;
    property AutoOptimizing : Boolean read FAutoOptimizing write FAutoOptimizing;
    function CreateHashInRange(const Key : TKey) : PtrUInt; inline;
    function CreateHash(const Key : TKey) : PtrUInt; virtual;

    procedure Reorder;
    procedure Optimize;
    procedure AutoOptimize;

    function FindFirst(const Key : TKey) : TValue; override; overload;
    function FindAll(const Key : TKey) : TValueArray; override; overload;
    procedure FindAll(const Key : TKey; var List : TValueArray); override; overload;

    procedure FindFirstIndex(const Key : TKey; out HashCode : PtrUInt; out Index : PtrInt);
    function GetCountOnHash(const HashCode : PtrUInt) : PtrUInt;
    function GetValueByIndex(const HashCode : PtrUInt; const Index : PtrUInt) : TValue;

    function Contain(const Key : TKey) : Boolean; override;

    function RemoveFirstKey(const Key: TKey): Boolean; override;
    function RemoveAllKeys(const Key: TKey): PtrUInt; override;

    function CountOf(const Key: TKey): PtrUInt; override;

    procedure Clear; override;
    function Count : Integer; override;

    function EditableEnumerators: Boolean; override;
    function GetValueEnumerator: TValueEnumerator; override;
    function GetKeyEnumerator: TKeyEnumerator; override;
    function GetKeyValueEnumerator: TKeyValueEnumerator; override;

    function CheckCoherence : Boolean; //ONLY DEBUG - SHOULD BE ALWAYS TRUE

    constructor Create; override;
    constructor Create(const HashSize : PtrUInt);
    class constructor Create;

    destructor Destroy; override;
  end;

  { TTinyHashSet }

  generic TTinyHashSet<TValue> = class(specialize TTinyHashData<TValue, TValue, specialize TValueBox<TValue>>)
  type
      TBoxType = specialize TValueBox<TValue>;
  public
      procedure Add(const Value : TValue);
      procedure UpdateFirst(const Value : TValue);
  end;

  { TTinyHashKeyMap }

  generic TTinyHashKeyMap<TKey, TValue> = class(specialize TTinyHashData<TKey, TValue, specialize TKeyValueBox<TKey, TValue>>)
  type
      TBoxType = specialize TKeyValueBox<TKey, TValue>;
  public
      procedure Add(const Key : TKey; const Value : TValue);
      procedure UpdateFirst(const Key : TKey; const Value : TValue);
  end;

  { TTinyHashAnsiStringSet }

  TTinyHashAnsiStringSet = class(specialize TTinyHashSet<AnsiString>)
  public                               
      function CreateHash(const Key: AnsiString): PtrUInt; override;
      constructor Create; override;
  end;

  { TTinyHashKeyAnsiStringMap }

  generic TTinyHashKeyAnsiStringMap<TValue> = class(specialize TTinyHashKeyMap<AnsiString, TValue>)
  public
      function CreateHash(const Key: AnsiString): PtrUInt; override;
      constructor Create; override;
  end;

  { TTinyHashOperatorSet }

  generic TTinyHashOperatorSet<TValue> = class(specialize TTinyHashSet<TValue>)
  type
      TMyComparator = specialize TOperatorComparator<TValue>;
  public
      constructor Create; override;
  end;

  { TTinyHashOperatorKeyMap }

  generic TTinyHashOperatorKeyMap<TKey, TValue> = class(specialize TTinyHashKeyMap<TKey, TValue>)
  type
      TMyComparator = specialize TOperatorComparator<TKey>;
  public
      constructor Create; override;
  end;

implementation

uses
  Math, crc;

{ TTinyHashData.TTinyKeyEnumerator }

function TTinyHashData.TTinyKeyEnumerator.GetCurrent: TKey;
begin
  Exit(Box.Key);
end;

{ TTinyHashData.TTinyKeyValueEnumerator }

function TTinyHashData.TTinyKeyValueEnumerator.GetCurrent: TKeyValuePair;
begin
  Result.Key := Box.Key;
  Result.Value := Box.Value;
end;

{ TTinyHashData.TTinyValueEnumerator }

function TTinyHashData.TTinyValueEnumerator.GetCurrent: TValue;
begin
  Exit(Box.Value);
end;

{ TTinyHashEnumerator }

function TTinyHashEnumerator.GetCurrentPointer: Pointer;
begin
  Modified:=True;
  Exit(Box.GetDataPointer + Box.GetValueOffset);
end;

function TTinyHashEnumerator.MoveNext: Boolean;
begin
  Inc(Index);
  while Index >= Length(Owner.FData[HashIndex]) do
  begin
     Index := 0;
     Inc(HashIndex);
     if HashIndex >= Owner.FHashRange then
        Exit(False);
  end;
  Box:=Owner.FData[HashIndex][Index];
  Exit(True);
end;

constructor TTinyHashEnumerator.Create(const S: TOwner);
begin
  Index := -1;
  HashIndex := 0;
  Modified := False;
  Owner := S;
end;

destructor TTinyHashEnumerator.Destroy;
begin
  if Modified then
     Owner.Reorder;
  inherited Destroy;
end;

{ TTinyHashData }

procedure TTinyHashData.SetHashRange(const AValue: PtrUInt);
begin
  if FHashRange=AValue then Exit;
  FHashRange:=AValue;
  Reorder;
end;

procedure TTinyHashData.AddBox(Box: TBoxType);
var
  c, HashCode : PtrUInt;
begin
  HashCode := {%H-}CreateHashInRange(Box.Key);
  c := Length(FData[HashCode]);
  SetLength(FData[HashCode], c+1);
  FData[HashCode][c] := Box;
  Inc(FCount);
  AutoOptimize;
end;

procedure TTinyHashData.OnSetComparator;
begin
  Reorder;
end;

function TTinyHashData.CreateHashInRange(const Key: TKey): PtrUInt;
begin
  Exit(CreateHash(Key) mod FHashRange);
end;

procedure TTinyHashData.FindFirstIndex(const Key: TKey; out HashCode: PtrUInt; out Index: PtrInt);
var
  i : PtrInt;
begin
  HashCode := CreateHashInRange(Key);
  for i := 0 to Length(FData[HashCode])-1 do
    if Compare(Key, FData[HashCode][i].Key) = 0 then
    begin
       Index := i;
       Exit;
    end;
  Index := -1;
end;

function TTinyHashData.GetCountOnHash(const HashCode: PtrUInt): PtrUInt;
begin
  Exit(Length(FData[HashCode]));
end;

function TTinyHashData.GetValueByIndex(const HashCode: PtrUInt; const Index: PtrUInt): TValue;
begin
  Exit(FData[HashCode][Index].Value);
end;

function TTinyHashData.CreateHash(const Key: TKey): PtrUInt;
begin
   Exit(crc32(InitCRC32Code, @Key, SizeOf(Key)));
end;

procedure TTinyHashData.Reorder;
var
  Items : array of TBoxType;
  i, j, Index : Integer;
  box : TBoxType;
begin
  Items := [];
  SetLength(Items, FCount);
  Index := 0;                
  if FCount > 0 then
    for i := 0 to Length(FData) -1 do
      for j := 0 to Length(FData[i]) -1 do
        Items[specialize PostInc<Integer>(Index)] := FData[i][j];
  SetLength(FData, FHashRange, 0);
  FCount := 0;
  for box in Items do
      AddBox(box);
  SetLength(Items, 0);
end;

procedure TTinyHashData.Optimize;  
var
  NewHashRange : PtrUInt;
begin
  NewHashRange := FHashRange;
  If FCount > FHashRange shl 2 then
     NewHashRange := Min(FHashRange shl 2 -1, MaxHashSize)
  else if (FHashRange > MinHashSize) and (FCount shl 2 +4 < FHashRange) then
     NewHashRange := max(MinHashSize, ((FHashRange+1) shr 2));
  SetHashRange(NewHashRange);
end;

procedure TTinyHashData.AutoOptimize;
begin
  if not FAutoOptimizing then
     Exit;
  FAutoOptimizing := False;
  Optimize;
  FAutoOptimizing := True;
end;

function TTinyHashData.FindFirst(const Key: TKey): TValue;
var
  i : PtrInt;
  HashCode : PtrUInt;
begin
  FindFirstIndex(Key, HashCode, i);
  if i < 0 then
     raise ENotFoundKeyException.Create('Cannot find the key');
  Exit(FData[HashCode][i].Value);
end;

function TTinyHashData.FindAll(const Key: TKey): TValueArray;
begin
  Result := [];
  FindAll(Key, Result);
end;

procedure TTinyHashData.FindAll(const Key: TKey; var List: TValueArray);
var
  b : TBoxType;
begin
  for b in FData[CreateHashInRange(Key)] do
    if Compare(Key, b.Key) = 0 then
       Insert(b.Value, List, Length(List));
end;

function TTinyHashData.Contain(const Key: TKey): Boolean;
var
  b : TBoxType;
begin
  for b in FData[CreateHashInRange(Key)] do
    if Compare(Key, b.Key) = 0 then
       Exit(True);
  Exit(False);
end;

function TTinyHashData.RemoveFirstKey(const Key: TKey): Boolean;
var
  i, e : Integer;
  Hash : PtrUInt;
begin
  Hash := CreateHashInRange(Key);
  e := Length(FData[Hash])-1;
  for i := 0 to e do
    if Compare(Key, FData[Hash][i].Key) = 0 then
    begin
       FData[Hash][i].Free;
       FData[Hash][i] := FData[Hash][e];
       SetLength(FData[Hash], e);
       Dec(FCount);  
       AutoOptimize;
       Exit(True);
    end;
  Exit(False);
end;

function TTinyHashData.RemoveAllKeys(const Key: TKey): PtrUInt;
var
  i, c : Integer;
  Hash : PtrUInt;
begin
  Hash := CreateHashInRange(Key);
  Result := 0;
  c := Length(FData[Hash]);
  i := 0;
  while i < c do
    if Compare(Key, FData[Hash][i].Key) = 0 then
    begin
       Dec(c);
       Dec(FCount);
       Inc(Result);
       FData[Hash][i].Free;
       FData[Hash][i] := FData[Hash][c];
    end
    else
       Inc(i);
    SetLength(FData[Hash], c);
    AutoOptimize;
end;

function TTinyHashData.CountOf(const Key: TKey): PtrUInt;
var
  Box : TBoxType;
begin
  Result := 0;
  for Box in FData[CreateHashInRange(Key)] do
    if Compare(Box.Key, Key) = 0 then
       Inc(Result);
end;

procedure TTinyHashData.Clear;
var
  i, j : Integer;
begin
  for i := 0 to FHashRange-1 do
    for j := 0 to Length(FData[i])-1 do
      FreeAndNil(FData[i][j]);
  FHashRange:=MinHashSize;
  SetLength(FData, FHashRange, 0);
  FCount := 0;
end;

function TTinyHashData.Count: Integer;
begin
  Exit(FCount);
end;

function TTinyHashData.EditableEnumerators: Boolean;
begin
  Exit(True);
end;

function TTinyHashData.GetValueEnumerator: TValueEnumerator;
begin
  Exit(TTinyValueEnumerator.Create(Self));
end;

function TTinyHashData.GetKeyEnumerator: TKeyEnumerator;
begin
  Exit(TTinyKeyEnumerator.Create(Self));
end;

function TTinyHashData.GetKeyValueEnumerator: TKeyValueEnumerator;
begin
  Exit(TTinyKeyValueEnumerator.Create(Self));
end;

function TTinyHashData.CheckCoherence: Boolean;
var
  HashCode : PtrUInt;
  Index : PtrInt;
begin
  for HashCode := 0 to FHashRange-1 do
    for Index := 0 to Length(FData[HashCode])-1 do
      if CreateHashInRange(FData[HashCode][Index].Key) <> HashCode then
         Exit(False);
  Exit(True);
end;

constructor TTinyHashData.Create;
begin
  Create(MinHashSize);
end;

constructor TTinyHashData.Create(const HashSize: PtrUInt);
begin
  FAutoOptimizing := True;
  FHashRange := Max(2, HashSize);
  SetLength(FData, FHashRange, 0);
  inherited Create;
end;

class constructor TTinyHashData.Create;
begin
  InitCRC32Code:=crc32(0, nil, 0);
end;

destructor TTinyHashData.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{ TTinyHashSet }

procedure TTinyHashSet.Add(const Value: TValue);
var
    Box : TBoxType;
begin
    Box := TBoxType.Create;
    Box.Value := Value;
    AddBox(Box);
end;

procedure TTinyHashSet.UpdateFirst(const Value: TValue);
var
    HashCode : PtrUInt;
    i : PtrInt;
begin
  FindFirstIndex(Value, HashCode, i);
  if i < 0 then
     Add(Value)
     else
     FData[HashCode][i].Value := Value;
end;

{ TTinyHashKeyMap }

procedure TTinyHashKeyMap.Add(const Key: TKey; const Value: TValue);
var
    Box : TBoxType;
begin
    Box := TBoxType.Create;
    Box.Key := Key;
    Box.Value := Value;
    AddBox(Box);
end;

procedure TTinyHashKeyMap.UpdateFirst(const Key: TKey; const Value: TValue);
var
    HashCode : PtrUInt;
    i : PtrInt;
begin
  FindFirstIndex(Key, HashCode, i);
  if i < 0 then
     Add(Key, Value)
     else
     FData[HashCode][i].Value := Value;
end;

{ TTinyHashAnsiStringSet }

function TTinyHashAnsiStringSet.CreateHash(const Key: AnsiString): PtrUInt;
begin
  Exit(crc32(InitCRC32Code, @Key[1], Length(Key)));
end;

constructor TTinyHashAnsiStringSet.Create;
begin
  inherited Create;
  StaticComparator := @CompareStr;
end;

{ TTinyHashKeyAnsiStringMap }

function TTinyHashKeyAnsiStringMap.CreateHash(const Key: AnsiString): PtrUInt;
begin
  Exit(crc32(InitCRC32Code, @Key[1], Length(Key)));
end;

constructor TTinyHashKeyAnsiStringMap.Create;
begin
  inherited Create;
  StaticComparator := @CompareStr;
end;

{ TTinyHashOperatorSet }

constructor TTinyHashOperatorSet.Create;
begin
  inherited Create;
  SetComparator(@TMyComparator.Compare);
end;

{ TTinyHashOperatorKeyMap }

constructor TTinyHashOperatorKeyMap.Create;
begin
  inherited Create;
  SetComparator(@TMyComparator.Compare);
end;

end.

