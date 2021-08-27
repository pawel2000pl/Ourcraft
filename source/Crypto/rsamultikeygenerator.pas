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
    procedure Generate;
    {%H-}constructor Create;
  public
    property n : TuInt read fieldN;
    property d : TuInt read fieldD;
    property e : TuInt read fieldE;
    property Generated : Boolean read FGenerated;
  end;

  generic TArrayOfRSAKeySearcher<TuInt> = array of specialize TRSAKeySearcher<TuInt>;


generic procedure GenerateRSAKeys<TuInt>(const Count : Integer; out Searchers : specialize TArrayOfRSAKeySearcher<TuInt>);

implementation

generic procedure GenerateRSAKeys<TuInt>(const Count : Integer; out Searchers : specialize TArrayOfRSAKeySearcher<TuInt>);
var
   queue : TQueueManager;
   i : Integer;
begin
   queue := TQueueManager.Create(1, 1, DefaultStackSize);
   queue.RemoveRepeated:=False;
   SetLength(Searchers{%H-}, Count);
   for i := 0 to Count-1 do
   begin
     Searchers[i] := specialize TRSAKeySearcher<TuInt>.Create();
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
        specialize GenerateRSAKey<TuInt>(fieldD, fieldE, fieldN);
    FGenerated := True;
end;

constructor TRSAKeySearcher.Create;
begin
    fieldD:=0;
    fieldE:=0;
    fieldN:=0;
    FGenerated:=False;
end;

end.

