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

    procedure SaveHistogram(Stream : TStream);
    procedure LoadHistogram(Stream : TStream);

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
    constructor Create(const Buf : PByte; const Size : PtrUInt; Mode : Byte = 0); //Create Histogram
    constructor Create(AStream : TStream; Size : PtrUInt; Mode : Byte = 0); //Create Histogram
    constructor Create(AStream : TStream); //Load Histogram
    destructor Destroy; override;

    class procedure CompressStream(Source, Dest : TStream; const Size : PtrUInt; Mode : Byte = 0);
    class procedure DecompressStream(Source, Dest : TStream);      
    class procedure RecurencyCompressStream(Source, Dest : TStream; const Size : PtrUInt; Mode : Byte = 0);
    class procedure RecurencyDecompressStream(Source, Dest : TStream);
  end;


implementation

uses
  Math;         

type
  TTriByteUInt = 0..16777215;

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
  Assert(((ZeroValue = nil) and (OneValue = nil) and EndNode) or ((ZeroValue <> nil) and (OneValue <> nil) and (not EndNode)));
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

procedure THuffmanTree.SaveHistogram(Stream: TStream);
var
   i : PtrUInt;
   c0, c1, c2, c3 : Byte;
begin
  c0 := 0;
  while (c0<High(c0)) and (fHistogram[c0] = 0) do Inc(c0);
  c1 := c0;
  while (c1<High(c1)) and (fHistogram[c1] <= High(Byte)) do Inc(c1);
  c2 := c1;
  while (c2<High(c2)) and (fHistogram[c2] <= High(Word)) do Inc(c2);
  c3 := c2;
  while (c3<High(c3)) and (fHistogram[c3] <= High(TTriByteUInt)) do Inc(c3);

  Stream.WriteByte(c0);
  Stream.WriteByte(c1);
  Stream.WriteByte(c2);
  Stream.WriteByte(c3);

  if c1 > 0 then
    for i := c0 to c1-1 do
          Stream.WriteByte(fHistogram[i]);
  if c2 > 0 then
    for i := c1 to c2-1 do
          Stream.WriteWord(fHistogram[i]);
  if c3 > 0 then
    for i := c2 to c3-1 do
          Stream.WriteBuffer(fHistogram[i], 3);
  for i := c3 to High(fHistogram) do
        Stream.WriteDWord(fHistogram[i]);
end;

procedure THuffmanTree.LoadHistogram(Stream: TStream);
var
  i : PtrUInt;
  c0, c1, c2, c3 : Byte;
begin
  c0 := Stream.ReadByte;
  c1 := Stream.ReadByte;
  c2 := Stream.ReadByte;
  c3 := Stream.ReadByte;

  if c0 > 0 then
    for i := 0 to c0-1 do
      fHistogram[i] := 0;
  if c3 > 0 then
    for i := c2 to c3-1 do
      fHistogram[i] := 0;
                   
  if c1 > 0 then
    for i := c0 to c1-1 do
          fHistogram[i] := Stream.ReadByte;
  if c2 > 0 then
    for i := c1 to c2-1 do
          fHistogram[i] := Stream.ReadWord;
  if c3 > 0 then
    for i := c2 to c3-1 do
          Stream.ReadBuffer(fHistogram[i], 3);
  for i := c3 to High(fHistogram) do
        fHistogram[i] := Stream.ReadDWord;
end;

procedure THuffmanTree.Sort;
const
  n = Length(fSortedHistogram);
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
  for i := 1 to High(fIntegral) do
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
  Tab : array[0..2*Length(fHistogram)] of TAssignCodeRecord;

  procedure QuickAssignCodes(Lower, Higher: PtrUInt; CurrentCode: TBitStream);
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
      case sign(PtrInt(fIntegral[Higher] - fIntegral[Half+1]) - PtrInt(fIntegral[Half] - fIntegral[Lower])) of
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
  Tab[Index].Higher:=High(fHistogram);
  Tab[Index].CurrentCode := TBitStream.Create();
  Inc(Index);

  While Index > 0 do
  begin                                                                       
    Dec(Index);
    element := Tab[Index];
    QuickAssignCodes(element.Lower, element.Higher, element.CurrentCode);
  end;

  for i := High(fHistogram) downto 1 do
  begin
    j := i;
    while (j<Length(fHistogram)) and (fCodes[fSortedHistogram[j-1]].Count < fCodes[fSortedHistogram[j]].Count) do
    begin
      specialize Swap<TBitStream>(fCodes[fSortedHistogram[j-1]], fCodes[fSortedHistogram[j]]);
      Inc(j);
    end;
  end;

  {$IfDef ASSERTIONS}
  for j := High(fHistogram) downto 1 do
      Assert(fCodes[fSortedHistogram[j-1]].Count >= fCodes[fSortedHistogram[j]].Count);
  {$EndIf}
end;

procedure THuffmanTree.AssignCodesMk2;
type
  TSetOfByte = set of Byte;
  TCodeRec = record
    Value : QWord;
    Indexes : TSetOfByte;
    code : QWordBool;
  end;

var
  Tab : array[0..Length(fHistogram)-1] of TCodeRec;
  Used : array[0..2*Length(Tab)+1] of TCodeRec;
  Len, UsedCount : PtrUInt;
  i, minIndex1, minIndex2, ShlOffset : PtrUInt;
  b : Byte;
begin
  Len := Length(Tab);
  UsedCount := 0;
  for i := 0 to High(fCodes) do
    fCodes[i] := TBitStream.Create();
  ShlOffset := Round(1+Log2(Length(Tab)));
  for i := 0 to Len-1 do
  begin
    Tab[i].code := False;
    Tab[i].Value := PtrUInt(fHistogram[i]) shl ShlOffset;
    Tab[i].Indexes := [i];
  end;

  Assert(Length(Tab) = Length(fHistogram));

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
    Tab[minIndex1].Value+=Tab[minIndex2].Value + 1;
    specialize Swap<TCodeRec>(Tab[Len-1], Tab[minIndex2]);
    Dec(Len);
    Inc(UsedCount, 2);
  end;

  Assert(UsedCount < Length(Used));

  for i := UsedCount-1 downto 0 do
      for b in Used[i].Indexes do
          fCodes[b].WriteBit(Used[i].code);
end;

procedure THuffmanTree.CreateTree;
var
  i : Integer;
begin
  for i := 0 to Length(fCodes)-1 do
     fTree.Push(i, 0, fCodes[i]);
  {$IfDef ASSERTIONS}
  for i := 0 to Length(fCodes)-1 do
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
  SaveHistogram(AStream);
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
    AssignCodesMk2();
  CreateTree;

  {$IfDef ASSERTIONS}
  for i := 0 to 255 do
      Assert(fCodes[i] <> nil);
  //Writeln(ToString);
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
  fMode := 0;
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
  FillByte(fHistogram, SizeOf(fHistogram), 0);
  for i := 0 to Size-1 do
      Inc(fHistogram[Buf[i]]);
  fMode:=Mode;
end;

constructor THuffmanTree.Create(AStream: TStream; Size: PtrUInt; Mode: Byte);
const
  BufSize = 65536;
var
  i, CurrentSize : PtrUInt;
  TempBuf : array[0..BufSize-1] of Byte;
begin
  Init;
  FillByte(fHistogram, SizeOf(fHistogram), 0);
  while Size > 0 do
  begin
    CurrentSize := Min(PtrUInt(BufSize), Size);
    AStream.ReadBuffer(TempBuf{%H-}, CurrentSize);
    for i := 0 to CurrentSize-1 do
        Inc(fHistogram[TempBuf[i]]);
    Dec(Size, CurrentSize);
  end;
  fMode:=Mode;
end;

constructor THuffmanTree.Create(AStream: TStream);
begin
  Init;
  fMode:=AStream.ReadByte;
  LoadHistogram(AStream);
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

class procedure THuffmanTree.CompressStream(Source, Dest: TStream; const Size: PtrUInt; Mode: Byte);
var
  HT : THuffmanTree;
begin
  HT := THuffmanTree.Create(Source, Size, Mode);
  Source.Seek(-PtrInt(Size), soFromCurrent);
  HT.WriteFromStream(Source, Size);
  HT.SaveToStream(Dest);
  HT.Stream.SaveToStream(Dest, True);
  FreeAndNil(HT);
end;

class procedure THuffmanTree.DecompressStream(Source, Dest: TStream);
var
  HT : THuffmanTree;
begin
  HT := THuffmanTree.Create(Source);
  HT.Stream.LoadFromStream(Source, True);
  HT.Stream.Position := 0;
  HT.ReadToStream(Dest, HT.BytesInHistogram);
  FreeAndNil(HT);
end;

class procedure THuffmanTree.RecurencyCompressStream(Source, Dest: TStream; const Size: PtrUInt; Mode: Byte);
var
  i : Integer;
  c : Integer;
  MS1, MS2 : TMemoryStream;
begin
  MS1 := TMemoryStream.Create;
  MS2 := TMemoryStream.Create;
  CompressStream(Source, MS1, Size, Mode);
  c := 0;
  for i := 0 to 255 do
  begin
   MS1.Position:=0;
   CompressStream(MS1, MS2, MS1.Size, Mode);
   if MS2.Size < MS1.Size then
   begin
       Inc(c);
       MS1.Free;
       MS1 := MS2;
       MS2 := TMemoryStream.Create;
   end
   else
     Break;
  end;
  Dest.WriteByte(c);
  MS1.Position:=0;
  Dest.CopyFrom(MS1, MS1.Size);
  MS1.Free;
  MS2.Free;
end;

class procedure THuffmanTree.RecurencyDecompressStream(Source, Dest: TStream);
var
  i : Integer;
  c : Integer;
  MS1, MS2 : TMemoryStream;
begin
  c := Source.ReadByte;
  MS1 := TMemoryStream.Create;
  MS2 := TMemoryStream.Create;
  DecompressStream(Source, MS1);

  for i := 0 to c-1 do
  begin
    MS1.Position:=0;
    DecompressStream(MS1, MS2);
    MS1.Free;
    MS1 := MS2;
    MS2 := TMemoryStream.Create;
  end;

  MS1.Position:=0;
  Dest.CopyFrom(MS1, MS1.Size);
  MS1.Free;
  MS2.Free;
end;

end.

