unit BitStream;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TBitStream }

  TBitStream = class
  const
    BufOverallocating = 256;
  private
    fBuffer : PByte;
    fBufferSize : LongWord;
    fCount : LongWord;
    fPosition : LongWord;
    function GetBits(Index : LongWord): Boolean; inline;
    procedure SetBits(Index : LongWord; AValue: Boolean); inline;
    procedure Reallocate;
    procedure SetCount(AValue: LongWord);
    procedure SetPosition(AValue: LongWord);
  public
    property Bits[Index : LongWord] : Boolean read GetBits write SetBits; default;
    property Count : LongWord read FCount write SetCount;
    property Position : LongWord read FPosition write SetPosition;

    procedure SaveToStream(Stream : TStream; const WriteSize : Boolean = True);
    procedure LoadFromStream(Stream : TStream; const ReadSize : Boolean = True);

    procedure WriteBitStream(BitStream : TBitStream);

    function WriteBit(Value : Boolean) : TBitStream;
    function ReadBit : Boolean;

    function Clone : TBitStream;
    procedure Clear;

    function ToString: ansistring; override;

    constructor Create(InitBuf : LongWord = 16);
    destructor Destroy; override;
  end;

implementation

{ TBitStream }

function TBitStream.GetBits(Index : LongWord): Boolean;
begin
  Exit((fBuffer[Index shr 3] and (1 shl (Index and 7))) <> 0);
end;

procedure TBitStream.SetBits(Index : LongWord; AValue: Boolean);
begin
  if AValue then
     fBuffer[Index shr 3] := fBuffer[Index shr 3] or (1 shl (Index and 7))
  else
     fBuffer[Index shr 3] := fBuffer[Index shr 3] and (not (1 shl (Index and 7)));
end;

procedure TBitStream.Reallocate;
begin
  while fCount shr 3 +1 > fBufferSize do
  begin
     Inc(fBufferSize, BufOverallocating);
     ReAllocMem(fBuffer, fBufferSize);
  end;
end;

procedure TBitStream.SetCount(AValue: LongWord);
begin
  fCount := AValue;
  Reallocate;
end;

procedure TBitStream.SetPosition(AValue: LongWord);
begin
  if FPosition=AValue then Exit;
  FPosition:=AValue;
  if fPosition > fCount then
     fCount:=fPosition;
  Reallocate;
end;

procedure TBitStream.SaveToStream(Stream: TStream; const WriteSize: Boolean);
const
  SizeTab : array[0..7] of Integer = (0, 1, 1, 1, 1, 1, 1, 1);
begin
  if WriteSize then
     Stream.WriteDWord(fCount);
  Stream.WriteBuffer(fBuffer^, fCount shr 3 + SizeTab[fCount and 7]);
end;

procedure TBitStream.LoadFromStream(Stream: TStream; const ReadSize: Boolean);
const
  SizeTab : array[0..7] of Integer = (0, 1, 1, 1, 1, 1, 1, 1);
begin
  if ReadSize then
  begin
     fCount := Stream.ReadDWord;
     Reallocate;
  end;
  Stream.ReadBuffer(fBuffer^, fCount shr 3 + SizeTab[fCount and 7]);
end;

procedure TBitStream.WriteBitStream(BitStream: TBitStream);
var
  i : LongWord;
begin
  if BitStream.Count = 0 then
     Exit;
  fCount:=BitStream.Count;
  Reallocate;
  for i := 0 to BitStream.Count-1 do
      WriteBit(BitStream.Bits[i]);
end;

function TBitStream.WriteBit(Value: Boolean): TBitStream; 
begin          
  if fPosition >= fCount then
  begin
     fCount:=fPosition+1;
     Reallocate;
  end;
  Assert(fPosition<fCount);
  Bits[fPosition] := Value;
  Inc(fPosition);
  Exit(Self);
end;

function TBitStream.ReadBit: Boolean;
begin           
  if fPosition >= fCount then
  begin
     fCount:=fPosition+1;
     Reallocate;
  end;             
  Assert(fPosition<fCount);
  Result := Bits[fPosition];
  Inc(fPosition);
end;

function TBitStream.Clone: TBitStream;
begin
  Result := TBitStream.Create(fBufferSize);
  Move(fBuffer^, Result.fBuffer^, fBufferSize);
  Result.fPosition:=fPosition;
  Result.fCount := fCount;
end;

procedure TBitStream.Clear;
begin
  fCount:=0;
  fPosition:=0;
  FreeMemAndNil(fBuffer);
  fBufferSize:=4;
  fBuffer := AllocMem(fBufferSize);
end;

function TBitStream.ToString: ansistring;
var
  i : PtrUInt;
begin
  Result := '';
  if fCount = 0 then
     Exit;
  SetLength(Result, fCount);
  for i := 0 to fCount-1 do
      Result[i+1] := specialize IfThen<Char>(Bits[i], '1', '0');
end;

constructor TBitStream.Create(InitBuf: LongWord);
begin             
  fBufferSize:=InitBuf;
  fBuffer := AllocMem(fBufferSize);
  fPosition:=0;
  fCount := 0;
end;

destructor TBitStream.Destroy;
begin
  if fBuffer <> nil then
    FreeMemAndNil(fBuffer);
  inherited Destroy;
end;

end.

