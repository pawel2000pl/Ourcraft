unit RSAMultiKeyGenerator;

{$mode objfpc}{$H+}{$ModeSwitch AdvancedRecords}

interface

uses
  cThreads, Classes, SysUtils, RSAKeys, Queues;

type

  { TRSAKeySearcher }

  generic TRSAKeySearcher<TuInt> = class
  private
    FGenerated: Boolean;
    fieldD: TuInt;
    fieldE: TuInt;
    fieldN: TuInt;
    FRandomBuf : Pointer;
    FRandomBufSize : PtrUInt;
    procedure Generate;
    {%H-}constructor Create(const ARandomBuf : Pointer = nil; const ARandomBufSize : PtrUInt = 0);
  public
    property n : TuInt read fieldN;
    property d : TuInt read fieldD;
    property e : TuInt read fieldE;
    property Generated : Boolean read FGenerated;
  end;

  generic TArrayOfRSAKeySearcher<TuInt> = array of specialize TRSAKeySearcher<TuInt>;


generic procedure GenerateRSAKeys<TuInt>(const Count : Integer; out Searchers : specialize TArrayOfRSAKeySearcher<TuInt>; const RandomBuf : Pointer = nil; const RandomBufSize : PtrUInt = 0);

implementation

generic procedure GenerateRSAKeys<TuInt>(const Count : Integer; out Searchers : specialize TArrayOfRSAKeySearcher<TuInt>; const RandomBuf : Pointer = nil; const RandomBufSize : PtrUInt = 0);
var
   queue : TQueueManager;
   i : Integer;
begin
   queue := TQueueManager.Create(1, 1);
   queue.RemoveRepeated:=False;
   SetLength(Searchers{%H-}, Count);
   for i := 0 to Count-1 do
   begin
     Searchers[i] := specialize TRSAKeySearcher<TuInt>.Create(RandomBuf, RandomBufSize);
     queue.AddMethod(@Searchers[i].Generate);
   end;
   for i := 0 to Count-1 do
   begin
       while not Searchers[i].Generated do
             sleep(1);
   end;
   queue.Clear;
   queue.Free;
end;

{ TRSAKeySearcher }

procedure TRSAKeySearcher.Generate;
begin
    if not FGenerated then
        specialize GenerateRSAKey<TuInt>(fieldD, fieldE, fieldN, FRandomBuf, FRandomBufSize);
    FGenerated := True;
end;

constructor TRSAKeySearcher.Create(const ARandomBuf: Pointer;
  const ARandomBufSize: PtrUInt);
begin
    FRandomBufSize:=ARandomBufSize;
    FRandomBuf:=ARandomBuf;
    fieldD:=0;
    fieldE:=0;
    fieldN:=0;
    FGenerated:=False;
end;

end.

