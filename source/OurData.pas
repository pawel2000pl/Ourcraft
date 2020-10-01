unit OurData;

{$mode objfpc}
{$inline on}

interface

uses
  SysUtils, Classes, StrUtils, Variants, Math, Sorts;

const
  CRLF = #13 + #10;

type

  { TOurData }

  TOurData = class
  private
    fName : ansistring;
    fData : ansistring;
    fSectionCount : integer;
    fSections : array of TOurData;
    Cache : TOurData;
    fSorted : boolean;
  public
    procedure Clear;
    procedure Load(const s : ansistring); overload;
    procedure Load(const s : ansistring; var Position : integer;
      const Len : integer); overload;
    function Save : ansistring;
    function AddSection(const SectionName : ansistring) : TOurData;
    //warning: section cannot exists
    function GetSection(const SectionName : ansistring;
      const CreateIfNotExists : boolean) : TOurData; overload;
    function GetSection(const SectionName : ansistring) : TOurData; overload;
    function GetSectionList : TStringList; //free after
    procedure DeleteSection(const SectionName : ansistring);
    function Sorted(const All : boolean) : boolean;
    procedure Sort(const All : boolean);

    procedure WriteString(const Str : ansistring);
    function ReadString : ansistring;
    procedure WriteInteger(const i : integer);
    function ReadInteger : integer;
    function ReadIntegerDef(const def : integer) : integer;
    procedure WriteDouble(const d : double);
    function ReadDouble : double;
    function ReadDoubleDef(const def : double) : double;
    procedure WriteBoolean(const b : boolean);
    function ReadBoolean : boolean;
    function ReadBooleanDef(const Def : boolean) : boolean;

    constructor Create(const MyName : ansistring = '');
    destructor Destroy; override;

    property Value : ansistring read fData write fData;
    property Name : ansistring read fName;
    property Sections[SectionName : ansistring] : TOurData read GetSection; default;
  end;

  { TOurDataSort }

  TOurDataSort = class(specialize TSort<TOurData>)
  public
    class function Compare(const a, b : TValue) : integer; override;
  end;

  { TOurDataSearcher }

  TOurDataSearcher = class(specialize TBSearch<TOurData, ansistring>)
  public
    class function Compare(const a : TValue; const b : TKey) : integer; override;
  end;

function BinaryToString(const P; const Size : integer) : ansistring;
function StringToBinary(const s : ansistring; var P) : integer;
function ReadDataString(const Str : ansistring; var Position : integer) : ansistring;
function WriteDataString(const Raw : ansistring;
  const Simple : boolean = False) : ansistring;

implementation

function IToH(const i : byte) : char; inline;
begin
  case i of
    0..9: Result := char(i + 48);
    10..15: Result := char(i + 55);
    else
      Result := '?';
  end;
end;

function HToI(const c : char) : byte; inline;
begin
  case c of
    '0'..'9': Result := byte(c) - 48;
    'A'..'F': Result := byte(c) - 55;
    else
      Result := byte(-1);
  end;
end;

function BinaryToString(const P; const Size : integer) : ansistring;
var
  i : integer;
begin
  setlength(Result, 2 * Size);
  for i := 0 to Size - 1 do
  begin
    Result[2 * i + 1] := IToH(PByte(@P)[i] shr 4);
    Result[2 * i + 2] := IToH(PByte(@P)[i] and 15);
  end;
end;

function StringToBinary(const s : ansistring; var P) : integer;
var
  i, l : integer;
begin
  l := length(s);
  Result := l shr 2;
  for i := 0 to l shr 2 do
    PByte(@P)[i] := (HToI(s[2 * i + 1])) shl 4 + (HToI(s[2 * i + 2]));
end;

function ConvertSpecial(const c : char) : char;
begin
  case c of
    'a': Result := #7;
    '\': Result := '\';
    'b': Result := #8;
    'r': Result := #13;
    '"': Result := '"';
    'f': Result := #12;
    't': Result := #9;
    'n': Result := #13;
    '0': Result := #0;
    '''': Result := '''';
    'v': Result := #11;
    '?': Result := '?';
    '%': Result := '%';
    #7: Result := 'a';
    #8: Result := 'b';
    #13: Result := 'n';
    #12: Result := 'f';
    #9: Result := 't';
    #0: Result := '0';
    #11: Result := 'v';
    else
      Result := c;
  end;
end;

function ReadDataString(const Str : ansistring; var Position : integer) : ansistring;
const
  BufSize = 256;
  SpecialTerminator = '\';
  StringTerminators =
    ['/', ',', '.', '=', '+', '*', '(', ')', '<', '>', '[', ']',
    '{', '}', ' ', #7, #8, #9, #10, #12, #13];
  Borders = '"';
  SpecialNum = '%';
  WhiteChars =
    [#1, #2, #3, #4, #5, #6, #7, #8, #9, #10, #11, #12, #13, #14, #15,
    #16, #17, #18, #19, #20, #21, #22, #23, #24, #25, #26, #27, #28,
    #29, #30, #31, #32, #133, #160];
var
  OutPos, OutLen, Len : integer;
  TerminateSet : set of char;
  c : char;
begin
  Result := '';
  Len := length(Str);
  OutPos := 1;
  OutLen := 0;
  setlength(Result, OutLen);
  //writeln('Reading: ', #9, copy(str, Position, Length(str)-Position));

  while (Position <= Len) and (Str[Position] in WhiteChars) do
    Inc(Position);
  if (Position > Len) then
    exit;

  if Str[Position] <> Borders then
    TerminateSet := WhiteChars + StringTerminators
  else
  begin
    TerminateSet := [Borders];
    Inc(Position);
  end;

  while (Position <= Len) and (not (Str[Position] in TerminateSet)) do
  begin
    if OutPos >= OutLen then
    begin
      Inc(OutLen, BufSize);
      setlength(Result, OutLen);
    end;
    if Str[Position] = SpecialTerminator then
    begin
      Inc(Position);
      if not (Position < Len) then
        break;
      Result[OutPos] := ConvertSpecial(Str[Position]);
    end
    else if Str[Position] = SpecialNum then
    begin
      if not (Position + 3 < Len) then
        break;
      c := char((byte(Str[Position + 1]) - 48) * 10 + byte(Str[Position + 2]) - 48);
      Inc(Position, 2);
      Result[OutPos] := c;
    end
    else
      Result[OutPos] := Str[Position];
    Inc(OutPos);
    Inc(Position);
  end;

  setlength(Result, OutPos - 1);
  //writeln('Readed: ', OutPos-1, #9, Result);
end;

function WriteDataString(const Raw : ansistring;
  const Simple : boolean = False) : ansistring;
const
  UnPrintableChars =
    [#1, #2, #3, #4, #5, #6, #7, #8, #9, #10, #11, #12, #13, #14, #15,
    #16, #17, #18, #19, #20, #21, #22, #23, #24, #25, #26, #27, #28,
    #29, #30, #31, #133, #160];
  StringTerminators =
    ['/', '\', ',', '.', '=', '+', '*', '(', ')', '<', '>', '[',
    ']', '{', '}', ' ', #7, #8, #9, #10, #12, #13];
  SpecialTerminator = '\';
  Borders = '"';
  SpecialNum = '%';
  BufSize = 256;
var
  OutPos, OutLen, Position, Len : integer;
begin
  Result := '';

  Position := 1;
  len := length(Raw);
  OutPos := 1;
  OutLen := 0;

  while (Position <= len) do
  begin
    if (OutPos + 8 >= OutLen) then
    begin
      Inc(OutLen, BufSize);
      setlength(Result, OutLen);
    end;
    if (Simple and (Raw[Position] in StringTerminators)) then
    begin
      Result[OutPos] := SpecialTerminator;
      Inc(OutPos);
      Result[OutPos] := ConvertSpecial(Raw[Position]);
    end
    else
    if Raw[Position] in UnPrintableChars then
    begin
      Result[OutPos] := SpecialNum;
      Inc(OutPos);
      Result[OutPos] := char(byte(Raw[Position]) div 10 + 48);
      Inc(OutPos);
      Result[OutPos] := char(byte(Raw[Position]) mod 10 + 48);
    end
    else if Raw[Position] in [SpecialNum, Borders, SpecialTerminator] then
    begin
      Result[OutPos] := SpecialTerminator;
      Inc(OutPos);
      Result[OutPos] := Raw[Position];
    end
    else
      Result[OutPos] := Raw[Position];
    Inc(OutPos);
    Inc(Position);
  end;

  if Simple then
    Result := copy(Result, 1, OutPos - 1)
  else
    Result := Borders + copy(Result, 1, OutPos - 1) + Borders;
end;

{ OurDataSearchet }

class function TOurDataSearcher.Compare(const a : TValue; const b : TKey) : integer;
begin
  Result := CompareText((a as TOurData).Name, b);
end;

{ OurDataSort }

class function TOurDataSort.Compare(const a, b : TValue) : integer;
begin
  Result := CompareText((a as TOurData).Name, (b as TOurData).Name);
end;

constructor TOurData.Create(const MyName : ansistring);
begin
  inherited Create;
  fSectionCount := 0;
  fName := MyName;
  fData := '';
  Cache := nil;
end;

destructor TOurData.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TOurData.Clear;
var
  i : integer;
begin
  for i := 0 to fSectionCount - 1 do
    fSections[i].Free;
  fSectionCount := 0;
  fName := '';
  fData := '';
  Cache := nil;
end;

procedure TOurData.Load(const s : ansistring);
var
  Position : integer;
begin
  Position := 1;
  Load(s, Position, length(s));
end;

procedure TOurData.Load(const s : ansistring; var Position : integer; const Len : integer);
var
  a, b : integer;
begin
  Clear;
  if s[Position] = '(' then
    Inc(Position);
  fName := ReadDataString(s, Position);
  Position := PosEx(',', s, Position) + 1;
  fData := ReadDataString(s, Position);

  a := PosEx(',', s, Position);
  b := PosEx(')', s, Position);
  if (a > 0) and (a < b) then
    Position := a
  else
    Position := b;

  while (Position <= Len) and (s[Position] <> ')') do
  begin
    Inc(Position);
    Inc(fSectionCount);
    setlength(fSections, fSectionCount);
    fSections[fSectionCount - 1] := TOurData.Create();
    fSections[fSectionCount - 1].Load(s, Position, len);
    a := PosEx(',', s, Position);
    b := PosEx(')', s, Position);
    if (a > 0) and (a < b) then
      Position := a
    else
      Position := b;
  end;

  Inc(Position);
  if not Sorted(False) then
    Sort(False);
  fSorted := True;
end;

function TOurData.Save : ansistring;
var
  i : integer;
begin
  if not Sorted(False) then
    Sort(False);
  fSorted := True;

  Result :=
    '(' + WriteDataString(fName, True) + ',' + WriteDataString(fData, False);

  if fSectionCount > 0 then
  begin
    for i := 0 to fSectionCount - 1 do
      Result := Result + ',' + fSections[i].Save;
  end;

  Result := Result + ')';
end;

function TOurData.AddSection(const SectionName : ansistring) : TOurData;
begin
  Inc(fSectionCount);
  setlength(fSections, fSectionCount);
  fSections[fSectionCount - 1] := TOurData.Create(SectionName);
  Cache := fSections[fSectionCount - 1];
  Result := Cache;
  fSorted := False;
end;

function TOurData.GetSection(const SectionName : ansistring;
  const CreateIfNotExists : boolean) : TOurData;
var
  i : integer;
begin
  if (Cache <> nil) and (sametext(SectionName, Cache.Name)) then
  begin
    Result := Cache;
    exit;
  end;

  if fSorted then
  begin
    i := TOurDataSearcher.BSearch(fSections, SectionName, 0, fSectionCount - 1);
    if i <> -1 then
    begin
      Cache := fSections[i];
      Result := fSections[i];
    end;
  end
  else
    for i := 0 to fSectionCount - 1 do
      if sametext(fSections[i].fName, SectionName) then
      begin
        Cache := fSections[i];
        Result := fSections[i];
        exit;
      end;

  if CreateIfNotExists then
    Result := AddSection(SectionName)
  else
    Result := nil;
end;

function TOurData.GetSection(const SectionName : ansistring) : TOurData;
begin
  Result := GetSection(SectionName, True);
end;

procedure TOurData.DeleteSection(const SectionName : ansistring);
var
  i : integer;
begin
  Cache := nil;
  for i := 0 to fSectionCount - 1 do
    if sametext(fSections[i].fName, SectionName) then
    begin
      fSections[i].Free;
      Dec(fSectionCount);
      fSections[i] := fSections[fSectionCount];
      setlength(fSections, fSectionCount);
      exit;
    end;
end;

function TOurData.Sorted(const All : boolean) : boolean;
var
  i : integer;
begin
  Result := TOurDataSort.Sorted(fSections, 0, fSectionCount);
  if All then
  begin
    if fSectionCount > 0 then
      for i := 0 to fSectionCount - 1 do
        Result := Result and fSections[i].Sorted(True);

    fSorted := Result;
  end;
end;

procedure TOurData.Sort(const All : boolean);
var
  i : integer;
begin
  if All and (fSectionCount > 0) then
  begin
    for i := 0 to fSectionCount - 1 do
      fSections[i].Sort(True);
    fSorted := True;
  end;
  TOurDataSort.Quick(fSections, 0, fSectionCount - 1);
end;

function TOurData.GetSectionList : TStringList;
var
  i : integer;
begin
  Result := TStringList.Create;
  for i := 0 to fSectionCount - 1 do
    Result.Add(fSections[i].fName);
end;

procedure TOurData.WriteString(const Str : ansistring);
begin
  fData := Str;
end;

function TOurData.ReadString : ansistring;
begin
  Result := fData;
end;

procedure TOurData.WriteInteger(const i : integer);
begin
  fData := IntToStr(i);
end;

function TOurData.ReadInteger : integer;
begin
  Result := StrToInt(fData);
end;

function TOurData.ReadIntegerDef(const def : integer) : integer;
begin
  Result := StrToIntDef(fData, def);
end;

procedure TOurData.WriteDouble(const d : double);
begin
  fData := FloatToStr(d);
end;

function TOurData.ReadDouble : double;
begin
  Result := StrToFloat(fData);
end;

function TOurData.ReadDoubleDef(const def : double) : double;
begin
  Result := StrToFloatDef(fData, def);
end;

procedure TOurData.WriteBoolean(const b : boolean);
begin
  fData := BoolToStr(b);
end;

function TOurData.ReadBoolean : boolean;
begin
  Result := StrToBool(fData);
end;

function TOurData.ReadBooleanDef(const Def : boolean) : boolean;
begin
  Result := StrToBoolDef(fData, Def);
end;

end.
