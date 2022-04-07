unit HuffmanTree;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, BitStream;

type

  { THuffmanTreeNode }

  THuffmanTreeNode = class sealed
  {$IfDef ASSERTIONS}
  private
    Assigned : Boolean;
  {$EndIf}
  public
    ZeroValue, OneValue : THuffmanTreeNode;
    Value : Byte;
    function EndNode : Boolean;
    procedure Push(NewValue : Byte; Position : PtrUInt; BitStream : TBitStream);
    function Get(BitStream : TBitStream) : Byte;
    constructor Create;
    destructor Destroy; override;
  end;

  { THuffmanTree }

  THuffmanTree = class
  private
    fMode : PtrUInt;
    fHistogram : array[Byte] of LongWord;
    fSortedHistogram : array[Byte] of Byte;
    fIntegral : array[Byte] of PtrUInt;
    fCodes : array[Byte] of TBitStream;
    fTree : THuffmanTreeNode;
    fStream : TBitStream;
    procedure Sort;
    procedure CreateIntegral;
    procedure AssignCodes;
    procedure AssignCodesMk2;
    procedure CreateTree;
    procedure Init;
  public
    property Stream : TBitStream read fStream;
    function BytesInHistogram : PtrUInt;

    procedure WriteByte(Value : Byte);
    function ReadByte : Byte;

    procedure WriteFromStream(AStream : TStream; Size : PtrUInt);
    procedure ReadToStream(AStream : TStream; Size : PtrUInt);

    procedure SaveToStream(AStream : TStream);

    function Efficiency : Double;
    function ToString: ansistring; override;
    procedure AfterConstruction; override;
    constructor Create(const Buf : PByte; const Size : PtrUInt; Mode : Byte = 0);
    constructor Create(AStream : TStream);
    destructor Destroy; override;

    class procedure CompressStream(Source, Dest : TStream; const Size : PtrUInt; Mode : Byte = 0);
    class procedure DecompressStream(Source, Dest : TStream);
  end;


implementation

uses
  Math;

{ THuffmanTree }

function Sum(const T : array of LongWord) : PtrUInt; overload;
var
  l : LongWord;
begin
  Result := 0;
  for l in T do
      Inc(Result, l);
end;

generic procedure Swap<T>(var a, b : T);
var
    c : T;
begin
   c := a;
   a := b;
   b := c;
end;

function InsertCombSortGetGap(const n : Integer) : Double; inline;
begin
   case n of
        2..1000: Exit(0.2);
        1001..9000000: Exit(0.3);
        else Exit(9/23);
   end;
end;

{ THuffmanTreeNode }

function THuffmanTreeNode.EndNode: Boolean;
begin
  Exit((ZeroValue = nil) and (OneValue = nil));
end;

procedure THuffmanTreeNode.Push(NewValue: Byte; Position: PtrUInt; BitStream: TBitStream);
begin
  if Position >= BitStream.Count then
  begin
    Value:=NewValue;
    {$IfDef ASSERTIONS}
    Assert(not Assigned);
    Assigned:=True;
    {$EndIf}
    Exit;
  end;
  if OneValue = nil then OneValue := THuffmanTreeNode.Create;
  if ZeroValue = nil then ZeroValue := THuffmanTreeNode.Create;
  if BitStream[Position] then
     OneValue.Push(NewValue, Position+1, BitStream)
     else
     ZeroValue.Push(NewValue, Position+1, BitStream);
end;

function THuffmanTreeNode.Get(BitStream: TBitStream): Byte;
begin
  if EndNode then
     Exit(Value);
  if BitStream.ReadBit then
     Exit(OneValue.Get(BitStream));
  Exit(ZeroValue.Get(BitStream));
end;

constructor THuffmanTreeNode.Create;
begin
  ZeroValue := nil;
  OneValue := nil;
  {$IfDef ASSERTIONS}
  Assigned:=False;
  {$EndIf}
end;

destructor THuffmanTreeNode.Destroy;
begin
  if ZeroValue <> nil then FreeAndNil(ZeroValue);
  if OneValue <> nil then FreeAndNil(OneValue);
  inherited Destroy;
end;

procedure THuffmanTree.Sort;
const
  n = 256;
var
    gap, i, j : integer;
    x : Byte;
    d : Double;
begin
    d := InsertCombSortGetGap(n);
    gap := round(d**round(logn(d, n)));
    while gap>1 do
    begin
        gap := round(gap*d);
        for i := gap to n-1 do
        begin
            x := fSortedHistogram[i];
            j := i;
            while ((j >= gap) and (fHistogram[x] < fHistogram[fSortedHistogram[j-gap]])) do
            begin
                fSortedHistogram[j] := fSortedHistogram[j-gap];
                dec(j, gap);
            end;
            fSortedHistogram[j] := x;
        end;
    end;
end;

procedure THuffmanTree.CreateIntegral;
var
  i : PtrUInt;
begin
  fIntegral[0] := fHistogram[fSortedHistogram[0]];
  for i := 1 to 255 do
    fIntegral[i] := fIntegral[i-1] + fHistogram[fSortedHistogram[i]];
end;

procedure THuffmanTree.AssignCodes;
type
  TAssignCodeRecord = record
    Lower, Higher : PtrUInt;
    CurrentCode: TBitStream;
  end;

var
  Index, l, h : PtrUInt;
  Tab : array[0..512] of TAssignCodeRecord;

  procedure AssignCodes2(Lower, Higher: PtrUInt; CurrentCode: TBitStream);
  var
    Half : PtrUInt;
  begin
    if Higher = Lower then
    begin
      fCodes[fSortedHistogram[Higher]] := CurrentCode;
      Exit;
    end;
    if Higher = Succ(Lower) then
    begin
      fCodes[fSortedHistogram[Higher]] := CurrentCode.Clone.WriteBit(True);
      fCodes[fSortedHistogram[Lower]] := CurrentCode.Clone.WriteBit(False);
      CurrentCode.Free;
      Exit;
    end;

    l := Lower;
    h := Higher;

    while l+1 < h do
    begin
      Half := (h+l) shr 1;
      case sign((PtrInt(fIntegral[Higher]) - PtrInt(fIntegral[Half+1])) - (PtrInt(fIntegral[Half]) - PtrInt(fIntegral[Lower]))) of
        -1: h := Half;
        0: Break;
        1: l := Half;
      end;
    end;

    if Half = Higher then
       Dec(Half);

    Tab[Index].Lower:=Lower;
    Tab[Index].Higher:=Half;
    Tab[Index].CurrentCode := CurrentCode.Clone.WriteBit(True);
    Inc(Index);
    Tab[Index].Lower:=Half+1;
    Tab[Index].Higher:=Higher;
    Tab[Index].CurrentCode := CurrentCode.Clone.WriteBit(False);
    Inc(Index);

    CurrentCode.Free;
  end;

var
  i, j : Integer;
  element : TAssignCodeRecord;
begin
  Index := 0;
  Tab[Index].Lower:=0;
  Tab[Index].Higher:=255;
  Tab[Index].CurrentCode := TBitStream.Create();
  Inc(Index);

  While Index > 0 do
  begin                                                                       
    Dec(Index);
    element := Tab[Index];
    AssignCodes2(element.Lower, element.Higher, element.CurrentCode);
  end;

  for i := 255 downto 1 do
  begin
    j := i;
    while (j<256) and (fCodes[fSortedHistogram[j-1]].Count < fCodes[fSortedHistogram[j]].Count) do
    begin
      specialize Swap<TBitStream>(fCodes[fSortedHistogram[j-1]], fCodes[fSortedHistogram[j]]);
      Inc(j);
    end;
  end;

end;

procedure THuffmanTree.AssignCodesMk2;
type
  TSetOfByte = set of Byte;
  TCodeRec = record
    Value : Double;
    Indexes : TSetOfByte;
    code : Boolean;
  end;

var
  Tab : array[Byte] of TCodeRec;
  Used : array[0..513] of TCodeRec;
  Len, UsedCount : PtrUInt;
  i, minIndex1, minIndex2 : PtrUInt;
  b : Byte;
begin
  Len := Length(Tab);
  UsedCount := 0;
  for i := 0 to High(fCodes) do
    fCodes[i] := TBitStream.Create();
  FillByte(Used{%H-}, SizeOf(Used), 0);
  for i := 0 to 255 do
  begin
    Tab[i].Value:=fHistogram[i];
    Tab[i].Indexes := [i];
  end;

  while Len > 1 do
  begin
    minIndex1 := 0;
    minIndex2 := 1;
    if Tab[MinIndex1].Value < Tab[MinIndex2].Value then
        specialize Swap<TCodeRec>(Tab[MinIndex1], Tab[minIndex2]);
    for i := 2 to Len-1 do
      if Tab[i].Value < Tab[minIndex1].Value then
      begin                                  
         if Tab[i].Value < Tab[minIndex2].Value then
         begin
            minIndex1:=minIndex2;
            minIndex2 := i;
         end
         else
            minIndex1 := i;
      end;
    Assert(minIndex1 <> minIndex2);
    Assert(Tab[MinIndex1].Value >= Tab[MinIndex2].Value);
    Used[UsedCount] := Tab[minIndex1];
    Used[UsedCount+1] := Tab[minIndex2];
    Used[UsedCount].code:=True;
    Used[UsedCount+1].code:=False;
    Tab[minIndex1].Indexes+=Tab[minIndex2].Indexes;
    Tab[minIndex1].Value+=Tab[minIndex2].Value + 1/1024;
    specialize Swap<TCodeRec>(Tab[Len-1], Tab[minIndex2]);
    Dec(Len);
    Inc(UsedCount, 2);
  end;

  Used[UsedCount] := Tab[0];

  for i := UsedCount downto 0 do
      for b in Used[i].Indexes do
          fCodes[b].WriteBit(Used[i].code);
end;

procedure THuffmanTree.CreateTree;
var
  i : Integer;
begin
  for i := 0 to 255 do
     fTree.Push(i, 0, fCodes[i]);
  {$IfDef ASSERTIONS}
  for i := 0 to 255 do
  begin
    fCodes[i].Position:=0;
    Assert(fTree.Get(fCodes[i]) = i);
  end;
  {$EndIf}
end;

procedure THuffmanTree.WriteByte(Value: Byte);
begin
  fStream.WriteBitStream(fCodes[Value]);
end;

function THuffmanTree.ReadByte: Byte;
begin
  Exit(fTree.Get(fStream));
end;

procedure THuffmanTree.WriteFromStream(AStream: TStream; Size: PtrUInt);
var
  i : PtrUInt;
begin
  if Size = 0 then
     Exit;
  for i := 0 to Size-1 do
      WriteByte(AStream.ReadByte);
end;

procedure THuffmanTree.ReadToStream(AStream: TStream; Size: PtrUInt);
var
  i : PtrUInt;
begin
  if Size = 0 then
     Exit;
  for i := 0 to Size-1 do
      AStream.WriteByte(ReadByte);
end;

procedure THuffmanTree.SaveToStream(AStream: TStream);
begin
  AStream.WriteByte(fMode);
  AStream.WriteBuffer(fHistogram, SizeOf(fHistogram));    ;
end;

function THuffmanTree.Efficiency: Double;
var
  i, Size : PtrUInt;
begin
  Size := 0;
  for i := 0 to 255 do
     Inc(Size, fCodes[i].Count * fHistogram[i]);
  Exit(Size/8/fIntegral[255]);
end;

function THuffmanTree.ToString: ansistring;
var
  i : PtrUInt;
begin
  Result:='';
  for i := 0 to 255 do
      Result := Result + (IntToStr(fSortedHistogram[i]) + ' (' + IntToStr(fHistogram[fSortedHistogram[i]]) + ' / ' + IntToStr(fCodes[fSortedHistogram[i]].Count*fHistogram[fSortedHistogram[i]]) + '):'#9 + fCodes[fSortedHistogram[i]].ToString) + #10;
  Result := Result + ('Efficiency: ' + FormatFloat('0.0000', Efficiency));
end;

procedure THuffmanTree.AfterConstruction;
{$IfDef ASSERTIONS}
var
  i : PtrUInt;
{$EndIF}
begin                       
  Sort;
  CreateIntegral;
  if fMode = 0 then
    AssignCodes()
    else
    AssignCodesMk2;
  CreateTree;

  {$IfDef ASSERTIONS}
  for i := 0 to 255 do
      Assert(fCodes[i] <> nil);
  Writeln(ToString);
  {$EndIf}
  inherited AfterConstruction;
  Assert(BytesInHistogram = Sum(fHistogram));
end;

procedure THuffmanTree.Init;
var
  i : PtrUInt;
begin
  fTree := THuffmanTreeNode.Create;
  fStream := TBitStream.Create(256);  
  for i := 0 to 255 do
      fSortedHistogram[i] := i;
  for i := 0 to 255 do
      fCodes[i] := nil;
end;

function THuffmanTree.BytesInHistogram: PtrUInt;
begin
  Exit(fIntegral[High(fIntegral)]);
end;

constructor THuffmanTree.Create(const Buf: PByte; const Size: PtrUInt; Mode: Byte);
var
  i : PtrUInt;
begin
  Init;
  for i := 0 to Size-1 do
      Inc(fHistogram[Buf[i]]);
  fMode:=Mode;
end;

constructor THuffmanTree.Create(AStream: TStream);
begin
  Init;
  fMode:=AStream.ReadByte;
  AStream.ReadBuffer(fHistogram, SizeOf(fHistogram));
end;

destructor THuffmanTree.Destroy;
var
  i : PtrUInt;
begin
  FreeAndNil(fTree);
  FreeAndNil(fStream);
  for i := 0 to 255 do
    if fCodes[i] <> nil then
       FreeAndNil(fCodes[i]);
  inherited Destroy;
end;

class procedure THuffmanTree.CompressStream(Source, Dest: TStream;
  const Size: PtrUInt; Mode: Byte);
var
  MS : TCustomMemoryStream;
  HT : THuffmanTree;
  InitPosition : PtrUInt;
begin
  if Source is TCustomMemoryStream then
     MS := Source as TCustomMemoryStream
     else
     begin
        MS := TMemoryStream.Create;  
        InitPosition := Source.Position;
        MS.CopyFrom(Source, Size);    
        Source.Position := InitPosition;
     end;

  HT := THuffmanTree.Create(MS.Memory, MS.Size, Mode);
  HT.WriteFromStream(Source, Size);
  HT.SaveToStream(Dest);
  HT.Stream.SaveToStream(Dest, True);
  FreeAndNil(HT);

  if Source <> MS then
     FreeAndNil(MS);
end;

class procedure THuffmanTree.DecompressStream(Source, Dest: TStream);
var
  HT : THuffmanTree;
begin
  HT := THuffmanTree.Create(Source);
  HT.Stream.LoadFromStream(Source, True);
  HT.Stream.Position := 0;
  HT.ReadToStream(Dest, HT.BytesInHistogram);
  HT.Free;
end;

end.

