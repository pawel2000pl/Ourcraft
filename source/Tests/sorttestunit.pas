unit SortTestUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Sorts;

type

  { TSortTestCase }

  TSortTestCase = class(TTestCase)
  type
      TSortClass = specialize TMemorySorter<LongWord>;
      TTestArray = array of LongWord;
      TSortFunction = procedure(var tab : array of LongWord) of object;
  private
      procedure RandomArray(const Count, Range : LongWord; var TestArray : TTestArray);
      procedure TestSortAlghorithm(const Count, Range : LongWord; const Algorithm : TSortFunction); overload;
      procedure BackTestSortAlghorithm(const Count : LongWord; const Algorithm : TSortFunction);
      procedure TestSortAlghorithm(const Name : AnsiString; const Algorithm : TSortFunction; const MaxCount : LongWord = 20); overload;
  published
      procedure Insert;
      procedure InsertComb;
      procedure InsertCombF;
      procedure Quick;
      procedure Quick2;
      procedure Merge;
  end;

implementation


{ TSortTestCase }

procedure TSortTestCase.RandomArray(const Count, Range: LongWord; var TestArray: TTestArray);
var
  i : Integer;
begin
  SetLength(TestArray, Count);
  for i := 0 to Count-1 do
      TestArray[i] := Random(Range);
end;

procedure TSortTestCase.TestSortAlghorithm(const Count, Range: LongWord;
  const Algorithm: TSortFunction);
var
  tab : TTestArray;
  t : QWord;
  desc : AnsiString;
begin
  RandomArray(Count, Range, Tab{%H-});
  t := GetTickCount64;
  Algorithm(tab);
  t := GetTickCount64 - t;
  desc := 't = ' + IntToStr(t) + ' ms' + #9 + 'Range/Count: ' + IntToStr(Range) + ' / ' + IntToStr(Count);
  writeln(desc);
  AssertTrue(desc, TSortClass.Sorted(tab));
end;

procedure TSortTestCase.BackTestSortAlghorithm(const Count: LongWord;
  const Algorithm: TSortFunction);
var
  tab : TTestArray;
  t : QWord;
  i : Integer;
  desc : AnsiString;
begin
  SetLength(tab, Count);
  for i := 0 to Count-1 do
      tab[i] := Count-i;
  t := GetTickCount64;
  Algorithm(tab);
  t := GetTickCount64 - t;
  desc := 't = ' + IntToStr(t) + ' ms' + #9 + 'BackTest: count: ' +  IntToStr(Count);
  writeln(desc);
  AssertTrue(desc, TSortClass.Sorted(tab));
end;

procedure TSortTestCase.TestSortAlghorithm(const Name: AnsiString;
  const Algorithm: TSortFunction; const MaxCount: LongWord);
var
  x, y : Integer;
begin
  writeln(Name);
  for x := 4 to MaxCount do
  begin
      for y := 4 to MaxCount+4 do
          TestSortAlghorithm(1 shl x, 1 shl y, Algorithm);
      BackTestSortAlghorithm(1 shl x, Algorithm);
  end;
  writeln;
  writeln;
end;

procedure TSortTestCase.Insert;
begin
  TestSortAlghorithm('Insert', TSortFunction(@TSortClass.Insert), 10);
end;

procedure TSortTestCase.InsertComb;
begin
  TestSortAlghorithm('InsertComb', TSortFunction(@TSortClass.InsertComb));
end;

procedure TSortTestCase.InsertCombF;
begin
  TestSortAlghorithm('InsertCombF', TSortFunction(@TSortClass.InsertCombF));
end;

procedure TSortTestCase.Quick;
begin
  TestSortAlghorithm('Quick', TSortFunction(@TSortClass.Quick));
end;

procedure TSortTestCase.Quick2;
begin
  TestSortAlghorithm('Quick2', TSortFunction(@TSortClass.Quick2));
end;

procedure TSortTestCase.Merge;
begin
  TestSortAlghorithm('Merge', TSortFunction(@TSortClass.Merge));
end;

initialization
  Randomize;
  RegisterTest(TSortTestCase);
end.

