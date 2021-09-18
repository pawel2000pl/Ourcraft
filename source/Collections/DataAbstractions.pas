unit DataAbstractions;

{$Mode ObjFpc}

interface

uses
    SysUtils, Classes;

type
   
    generic TComparing<TKey> = class
    type
        TComparator = function(const a, b : TKey) : Integer of object;
        TStaticComparator = function(const a, b : TKey) : Integer;
    private
        FComparator : TComparator;
        FStaticComparator : TStaticComparator;        
    protected
        procedure OnSetComparator; virtual;
    public        
        procedure SetComparator(const AComparator : TComparator); overload;
        procedure SetComparator(const AComparator : TStaticComparator); overload;
        procedure SetStaticComparator(const AComparator : TStaticComparator);
    
        property Comparator : TComparator read FComparator write SetComparator;
        property StaticComparator : TStaticComparator read FStaticComparator write SetStaticComparator;
        
        class function MemoryComparator(const a, b : TKey) : Integer; static;
        function UseStaticComparator(const a, b : TKey) : Integer;

        function Compare(const a, b : TKey) : Integer; virtual;
        
        procedure AssignComparators(AComparator : TComparing);
        constructor Create; virtual;
    end;

    { TCustomEnumerator }

    generic TCustomEnumerator<TEnumeratorType> = class abstract
    protected
        function GetCurrent : TEnumeratorType; virtual; abstract;
        function GetCurrentPointer : Pointer; virtual; //nil = value cannot be edited
    public
        property Current : TEnumeratorType read GetCurrent;
        property CurrentPointer : Pointer read GetCurrentPointer;
        function GetEnumerator : TCustomEnumerator;
        function MoveNext: Boolean; virtual; abstract;
        function MoveNextAndFreeAtEnd: Boolean;
    end;

    { TCustomDataContainer }

    generic TCustomDataContainer<TKey, TValue> = class abstract(specialize TComparing<TKey>)
    type
        PKey = ^TKey;
        PValue = ^TValue;
        TValueArray = array of TValue;

        TForEachKeyValueProcedure = procedure(const Key : TKey; const Value : TValue) of object;
        TForEachKeyVarValueProcedure = procedure(const Key : TKey; var Value : TValue) of object;
        TForEachValueProcedure = procedure(const Value : TValue) of object;
        TForEachVarValueProcedure = procedure(var Value : TValue) of object;
        TForEachKeyValueStaticProcedure = procedure(const Key : TKey; const Value : TValue);
        TForEachKeyvarValueStaticProcedure = procedure(const Key : TKey; var Value : TValue);
        TForEachValueStaticProcedure = procedure(const Value : TValue);
        TForEachVarValueStaticProcedure = procedure(var Value : TValue);

        TKeyValuePair = record
            Key : TKey;
            Value : TValue;
        end;
        PKeyValuePair = ^TKeyValuePair;

        TKeyEnumerator = specialize TCustomEnumerator<TKey>;
        TValueEnumerator = specialize TCustomEnumerator<TValue>;
        TKeyValueEnumerator = specialize TCustomEnumerator<TKeyValuePair>;

    private
        FWriterLocker : TRTLCriticalSection;
        FReadersCount : LongWord;       
        FIsWriting : Boolean;      
        function CheckIfItIsReading : Boolean; inline;
    public
        property IsReading : Boolean read CheckIfItIsReading;
        property IsWriting : Boolean read FIsWriting;
        procedure BeginRead;
        procedure EndRead;
        function BeginWrite : Boolean;
        procedure EndWrite;

        function GetFirstKey : TKey; virtual;
        function GetFirstValue : TValue; virtual;
        function GetFirstKeyValue : TKeyValuePair; virtual;

        function FindFirst(const Key : TKey) : TValue; virtual; abstract; overload;
        function FindAll(const Key : TKey) : TValueArray; virtual; abstract; overload;
        procedure FindAll(const Key : TKey; var List : TValueArray); virtual; abstract; overload;

        function Contain(const Key : TKey) : Boolean; virtual;

        function RemoveFirstKey(const Key : TKey) : Boolean; virtual; abstract;
        function RemoveAllKeys(const Key : TKey) : PtrUInt; virtual; abstract;

        function CountOf(const Key : TKey) : PtrUInt; virtual;

        procedure Clear; virtual; abstract;
        function Count : Integer; virtual; abstract;

        function EditableEnumerators : Boolean; virtual;

        function GetKeyEnumerator : TKeyEnumerator; virtual; abstract;
        function GetValueEnumerator : TValueEnumerator; virtual; abstract;
        function GetKeyValueEnumerator : TKeyValueEnumerator; virtual; abstract;

        procedure ForEach(const proc : TForEachKeyValueProcedure); virtual; overload;
        procedure ForEach(const proc : TForEachValueProcedure); virtual; overload;
        procedure ForEach(const proc : TForEachKeyValueStaticProcedure); virtual; overload;
        procedure ForEach(const proc : TForEachValueStaticProcedure); virtual; overload;

        //raise exception for EditableEnumerators = False
        procedure ForEach(const proc : TForEachKeyVarValueProcedure); virtual; overload;
        procedure ForEach(const proc : TForEachVarValueProcedure); virtual; overload;
        procedure ForEach(const proc : TForEachKeyVarValueStaticProcedure); virtual; overload;
        procedure ForEach(const proc : TForEachVarValueStaticProcedure); virtual; overload;

        constructor Create; override;
        destructor Destroy; override;
    end;
    
    TBox = class abstract
    public
        class function GetKeyOffset : PtrUInt; virtual; abstract;
        class function GetKeySize : PtrUInt; virtual; abstract;
        class function GetValueOffset : PtrUInt; virtual; abstract;
        class function GetValueSize : PtrUInt; virtual; abstract;
        function GetDataPointer : Pointer; virtual; abstract;
    end;

    generic TTypedBox<TKey, TValue> = class abstract(TBox)
    type
        PKey = ^TKey;   
        PValue = ^TValue;
    protected
        procedure SetKey(const AKey : TKey); virtual;
        function GetKey : TKey; virtual;
        procedure SetValue(const AValue : TValue); virtual;
        function GetValue : TValue; virtual;
    public
        property Key : TKey read GetKey write SetKey;
        property Value : TValue read GetValue write SetValue;
    
        class function GetKeySize : PtrUInt; override;
        class function GetValueSize : PtrUInt; override;
    end;

    generic TKeyValueBox<TKey, TValue> = class(specialize TTypedBox<TKey, TValue>) 
    type
        TContainerType = specialize TCustomDataContainer<TKey, TValue>;
        TData = TContainerType.TKeyValuePair;
    private
        FData : TData;
    protected    
        procedure SetKey(const AKey : TKey); override;
        function GetKey : TKey; override;
        procedure SetValue(const AValue : TValue); override;
        function GetValue : TValue; override;
    public    
        class function GetKeyOffset : PtrUInt; override;
        class function GetValueOffset : PtrUInt; override;

        function GetDataPointer : Pointer; override;
    end;
        
    generic TValueBox<TValue> = class(specialize TTypedBox<TValue, TValue>)
    private
        FValue : TValue;
    protected    
        procedure SetKey(const AKey : TValue); override;
        function GetKey : TValue; override;
        procedure SetValue(const AValue : TValue); override;
        function GetValue : TValue; override;
    public
        class function GetKeyOffset : PtrUInt; override;
        class function GetValueOffset : PtrUInt; override;

        function GetDataPointer : Pointer; override;
    end;

    { TRecurencyTypedBox }

    generic TRecurencyTypedBox<TKey, TValue, TInternalBox> = class(specialize TTypedBox<TKey, TValue>)
    private
        FInternalBox : TInternalBox;
    protected
        procedure SetKey(const AKey : TKey); override;
        function GetKey : TKey; override;
        procedure SetValue(const AValue : TValue); override;
        function GetValue : TValue; override;
    public
        class function GetKeyOffset : PtrUInt; override;
        class function GetKeySize : PtrUInt; override;
        class function GetValueOffset : PtrUInt; override;
        class function GetValueSize : PtrUInt; override;

        function GetDataPointer : Pointer; override;

        constructor Create;
        destructor Destroy; override;
    end;

    generic TOperatorComparator<TKey> = class
    public
        class function Compare(const a, b : TKey) : Integer; static;
    end;
    
    ENotFoundKeyException = class(Exception);
    ECannotBeEdited = class(Exception);

generic function tee<T>(const Value : T; out OutValue : T) : T; inline;
generic procedure Swap<T>(var a, b : T); inline;

generic function PostInc<T>(var X : T) : T; inline; overload;
generic function PostDec<T>(var X : T) : T; inline; overload;
generic function PreInc<T>(var X : T) : T; inline; overload;
generic function PreDec<T>(var X : T) : T; inline; overload;
generic function PostInc<T>(var X : T; const N : T) : T; inline; overload;
generic function PostDec<T>(var X : T; const N : T) : T; inline; overload;
generic function PreInc<T>(var X : T; const N : T) : T; inline; overload;
generic function PreDec<T>(var X : T; const N : T) : T; inline; overload;

implementation

generic function tee<T>(const Value : T; out OutValue : T) : T; inline;
begin
    OutValue := Value;
    Exit(Value);
end;

generic procedure Swap<T>(var a, b : T);
var
   tmp : T;
begin
    tmp := a;
    a := b;
    b := tmp;
end;

generic function PostInc<T>(var X : T) : T;
begin
    Result := X;
    Inc(X);
end;

generic function PostDec<T>(var X : T) : T; 
begin
    Result := X;
    Dec(X);
end;

generic function PreInc<T>(var X : T) : T;   
begin          
    Inc(X);
    Exit(X);
end;

generic function PreDec<T>(var X : T) : T;    
begin
    Dec(X);
    Exit(X);
end;

generic function PostInc<T>(var X : T; const N : T) : T;
begin
    Result := X;
    Inc(X, N);
end;

generic function PostDec<T>(var X : T; const N : T) : T;   
begin
    Result := X;
    Dec(X, N);
end;

generic function PreInc<T>(var X : T; const N : T) : T;     
begin
    Inc(X, N);
    Exit(X);
end;

generic function PreDec<T>(var X : T; const N : T) : T;
begin
    Dec(X, N);
    Exit(X);
end;

{ TCustomDataContainer }

function TCustomDataContainer.CheckIfItIsReading: Boolean;
begin
  Exit(FReadersCount > 0);
end;

procedure TCustomDataContainer.BeginRead;
begin                              
   EnterCriticalSection(FWriterLocker);
   InterlockedIncrement(FReadersCount);
   LeaveCriticalSection(FWriterLocker);
end;

procedure TCustomDataContainer.EndRead;
begin
   InterlockedDecrement(FReadersCount);
end;

function TCustomDataContainer.BeginWrite: Boolean;
begin
   EnterCriticalSection(FWriterLocker);
   while FReadersCount > 0 do
       TThread.Yield;
   FIsWriting := True;
   Exit(True);
end;

procedure TCustomDataContainer.EndWrite;
begin
   FIsWriting := False;
   LeaveCriticalSection(FWriterLocker);
end;

function TCustomDataContainer.GetFirstKey: TKey;
var
   enum : TKeyEnumerator;
begin
   enum := GetKeyEnumerator;
   try
     if enum.MoveNext then
         Result := enum.Current
     else
         raise ENotFoundKeyException.Create('Cannot find the first position');
   finally
     enum.Free;
   end;
end;

function TCustomDataContainer.GetFirstValue: TValue;
var
   enum : TValueEnumerator;
begin
   enum := GetValueEnumerator;
   try
     if enum.MoveNext then
         Result := enum.Current
     else
         raise ENotFoundKeyException.Create('Cannot find the first position');
   finally
     enum.Free;
   end;
end;

function TCustomDataContainer.GetFirstKeyValue: TKeyValuePair;
var
   enum : TKeyValueEnumerator;
begin
   enum := GetKeyValueEnumerator;
   try
     if enum.MoveNext then
         Result := enum.Current
     else
         raise ENotFoundKeyException.Create('Cannot find the first position');
   finally
     enum.Free;
   end;
end;

function TCustomDataContainer.Contain(const Key: TKey): Boolean;
begin
  Exit(CountOf(Key)>0);
end;

function TCustomDataContainer.CountOf(const Key: TKey): PtrUInt;
begin
  Exit(Length(FindAll(Key)));
end;

function TCustomDataContainer.EditableEnumerators: Boolean;
begin
  Exit(False);
end;

procedure TCustomDataContainer.ForEach(const proc: TForEachKeyValueProcedure);
var
   enum : TKeyValuePair;
begin
   BeginRead;
   try
     for enum in GetKeyValueEnumerator do
         proc(enum.Key, enum.Value);
   finally
     EndRead;
   end;
end;

procedure TCustomDataContainer.ForEach(const proc: TForEachValueProcedure);
var
   enum : TValue;
begin
   BeginRead;
   try
     for enum in GetValueEnumerator do
         proc(enum);
   finally
     EndRead;
   end;
end;

procedure TCustomDataContainer.ForEach(const proc: TForEachKeyValueStaticProcedure);
var
   enum : TKeyValuePair;
begin
   BeginRead;
   try
     for enum in GetKeyValueEnumerator do
         proc(enum.Key, enum.Value);
   finally
     EndRead;
   end;
end;

procedure TCustomDataContainer.ForEach(const proc: TForEachValueStaticProcedure);
var
   enum : TValue;
begin
   BeginRead;
   try
     for enum in GetValueEnumerator do
         proc(enum);
   finally
     EndRead;
   end;
end;

procedure TCustomDataContainer.ForEach(const proc: TForEachKeyVarValueProcedure);
var
   enum : TKeyValueEnumerator;
   Ptr : PValue;
begin
   if not EditableEnumerators then
       raise ECannotBeEdited.Create('Class ' + ClassName + ' does not support editable enumerators');
   BeginWrite;
   try
     enum := GetKeyValueEnumerator;
     try
       while enum.MoveNext do
       begin
         Ptr := PValue(enum.GetCurrentPointer);
         proc(enum.Current.Key, Ptr^);
       end;      
     finally
       enum.Free;
     end;
   finally
     EndWrite;
   end;
end;

procedure TCustomDataContainer.ForEach(const proc: TForEachVarValueProcedure);
var
   enum : TValueEnumerator;
   Ptr : PValue;
begin
   if not EditableEnumerators then
       raise ECannotBeEdited.Create('Class ' + ClassName + ' does not support editable enumerators');
   BeginWrite;
   try
     enum := GetValueEnumerator;
     try
       while enum.MoveNext do
       begin
         Ptr := PValue(enum.GetCurrentPointer);
         proc(Ptr^);
       end;
     finally
       enum.Free;
     end;
   finally
     EndWrite;
   end;
end;

procedure TCustomDataContainer.ForEach(const proc: TForEachKeyVarValueStaticProcedure);
var              
   enum : TKeyValueEnumerator;
   Ptr : PValue;
begin
   if not EditableEnumerators then
       raise ECannotBeEdited.Create('Class ' + ClassName + ' does not support editable enumerators');
   BeginWrite;
   try
     enum := GetKeyValueEnumerator;
     try
       while enum.MoveNext do
       begin
         Ptr := PValue(enum.GetCurrentPointer);
         proc(enum.Current.Key, Ptr^);
       end;
     finally
       enum.Free;
     end;
   finally
     EndWrite;
   end;
end;

procedure TCustomDataContainer.ForEach(const proc: TForEachVarValueStaticProcedure);
var
   enum : TValueEnumerator;
   Ptr : PValue;
begin
   if not EditableEnumerators then
       raise ECannotBeEdited.Create('Class ' + ClassName + ' does not support editable enumerators');
   BeginWrite;
   try
     enum := GetValueEnumerator;
     try
       while enum.MoveNext do
       begin
         Ptr := PValue(enum.GetCurrentPointer);
         proc(Ptr^);
       end;
     finally
       enum.Free;
     end;
   finally
     EndWrite;
   end;
end;

constructor TCustomDataContainer.Create;
begin
  InitCriticalSection(FWriterLocker);
  FReadersCount := 0;
  FIsWriting := False;
  inherited Create;
end;

destructor TCustomDataContainer.Destroy;
begin
  DoneCriticalSection(FWriterLocker);
  inherited Destroy;
end;

{ TCustomEnumerator }

function TCustomEnumerator.GetCurrentPointer: Pointer;
begin
  Exit(nil);
end;

function TCustomEnumerator.GetEnumerator: TCustomEnumerator;
begin
  Exit(Self);
end;

function TCustomEnumerator.MoveNextAndFreeAtEnd: Boolean;
begin
  Result := MoveNext;
  if not Result then
      Free;
end;

{ TRecurencyTypedBox }

procedure TRecurencyTypedBox.SetKey(const AKey: TKey);
begin
  FInternalBox.SetKey(AKey);
end;

function TRecurencyTypedBox.GetKey: TKey;
begin
  Exit(FInternalBox.GetKey);
end;

procedure TRecurencyTypedBox.SetValue(const AValue: TValue);
begin
  FInternalBox.SetValue(AValue);
end;

function TRecurencyTypedBox.GetValue: TValue;
begin
  Exit(FInternalBox.GetValue);
end;

class function TRecurencyTypedBox.GetKeyOffset: PtrUInt;
begin
  Exit(TInternalBox.GetKeyOffset);
end;

class function TRecurencyTypedBox.GetKeySize: PtrUInt;
begin
  Exit(TInternalBox.GetKeySize);
end;

class function TRecurencyTypedBox.GetValueOffset: PtrUInt;
begin
  Exit(TInternalBox.GetValueOffset);
end;

class function TRecurencyTypedBox.GetValueSize: PtrUInt;
begin
  Exit(TInternalBox.GetValueSize);
end;

function TRecurencyTypedBox.GetDataPointer: Pointer;
begin
  Exit(FInternalBox.GetDataPointer);
end;

constructor TRecurencyTypedBox.Create;
begin
   FInternalBox:=TInternalBox.Create;
end;

destructor TRecurencyTypedBox.Destroy;
begin
  FInternalBox.Free;
  inherited Destroy;
end;

class function TComparing.MemoryComparator(const a, b : TKey) : Integer; static;    
begin
    Exit(CompareMemRange(@a, @b, SizeOf(TKey)));
end;    

procedure TComparing.AssignComparators(AComparator : TComparing);
begin
    FComparator := AComparator.FComparator;
    FStaticComparator := AComparator.FStaticComparator;
    OnSetComparator;
end;

procedure TComparing.OnSetComparator; 
begin
    //do nothing;
end;

function TComparing.UseStaticComparator(const a, b : TKey) : Integer;
begin 
    Result := (FStaticComparator(a, b));
end;

procedure TComparing.SetComparator(const AComparator : TComparator);
begin
    if AComparator = FComparator then
        Exit;
    if (AComparator <> nil) and Assigned(AComparator) and (AComparator <> @Compare) then
        FComparator := AComparator
    else
        FComparator := @UseStaticComparator;
    OnSetComparator;
end;

procedure TComparing.SetComparator(const AComparator : TStaticComparator);
begin
    SetStaticComparator(AComparator);
end;

procedure TComparing.SetStaticComparator(const AComparator : TStaticComparator);
begin
    if AComparator = FStaticComparator then
        Exit;
    FComparator := @UseStaticComparator;
    if (AComparator <> nil) then
        FStaticComparator := AComparator    
    else
        FStaticComparator := @MemoryComparator;
    OnSetComparator;
end;

function TComparing.Compare(const a, b : TKey) : Integer; 
begin 
    Exit(FComparator(a, b));
end;

constructor TComparing.Create;
begin
    FStaticComparator := @MemoryComparator;
    FComparator := @UseStaticComparator;
end;

procedure TTypedBox.SetKey(const AKey : TKey);
begin
    PKey(GetDataPointer + GetKeyOffset)^ := AKey;
end;

function TTypedBox.GetKey : TKey;
begin
    Exit(PKey(GetDataPointer + GetKeyOffset)^);
end;

procedure TTypedBox.SetValue(const AValue : TValue);
begin
    PValue(GetDataPointer + GetValueOffset)^ := AValue;
end;

function TTypedBox.GetValue : TValue;
begin
    Exit(PValue(GetDataPointer + GetValueOffset)^);
end;

class function TTypedBox.GetKeySize : PtrUInt; 
begin
    Exit(SizeOf(TKey));
end;

class function TTypedBox.GetValueSize : PtrUInt; 
begin
    Exit(SizeOf(TValue));
end;

class function TKeyValueBox.GetKeyOffset : PtrUInt; 
begin
    Exit(0);
end;

class function TKeyValueBox.GetValueOffset : PtrUInt; 
begin
    Exit(SizeOf(TKey));
end;

function TKeyValueBox.GetDataPointer : Pointer;
begin
    Exit(@FData);
end;

procedure TKeyValueBox.SetKey(const AKey : TKey); 
begin
    FData.Key := AKey;
end;

function TKeyValueBox.GetKey : TKey; 
begin
    Exit(FData.Key);
end;

procedure TKeyValueBox.SetValue(const AValue : TValue); 
begin
    FData.Value := AValue;
end;

function TKeyValueBox.GetValue : TValue; 
begin
    Exit(FData.Value);
end;

class function TValueBox.GetKeyOffset : PtrUInt; 
begin
    Exit(0);
end;

class function TValueBox.GetValueOffset : PtrUInt; 
begin
    Exit(0);
end;

function TValueBox.GetDataPointer : Pointer;
begin
    Exit(@FValue);
end;

procedure TValueBox.SetKey(const AKey : TValue); 
begin
    FValue := AKey;
end;

function TValueBox.GetKey : TValue; 
begin
    Exit(FValue);
end;

procedure TValueBox.SetValue(const AValue : TValue); 
begin
    FValue := AValue;
end;

function TValueBox.GetValue : TValue; 
begin
    Exit(FValue);
end;

class function TOperatorComparator.Compare(const a, b : TKey) : Integer;
begin
    if a < b then
        Exit(-1);
    if a = b then
        Exit(0);
    Exit(1);
end;

end.
