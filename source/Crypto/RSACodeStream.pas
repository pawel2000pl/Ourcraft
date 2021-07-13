unit RSACodeStream;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RSAKeys, math;

generic function CodeStream<TuInt>(Source, Dest : TStream; Size : PtrUInt; const e, m : TuInt) : PtrUInt;
generic function DeCodeStream<TuInt>(Source, Dest : TStream; Size : PtrUInt; const d, m : TuInt) : PtrUInt;

implementation

generic function CodeStream<TuInt>(Source, Dest : TStream; Size : PtrUInt; const e, m : TuInt) : PtrUInt;
var
  blockBuf : PQWord;
  data, codedData : TuInt;
  BufSize : PtrUInt;
  GainedSize : PtrUInt;
begin
  BufSize := SizeOf(TuInt);
  blockBuf:=Pointer(@data);
  data := 0;
  Result := 0;
  while Size > 0 do
  begin
      GainedSize := Min(Size, BufSize-2*SizeOf(QWord)-1);
      blockBuf[0] := GainedSize;
      blockBuf[1] := Random($7FFFFFFFFFFFFFFF);
      Source.ReadBuffer(blockBuf[2], GainedSize);
      codedData := specialize CodeRSA<TuInt>(data, e, m);
      Dest.WriteBuffer(codedData, SizeOf(codedData));
      Inc(Result, SizeOf(codedData));
      Dec(Size, GainedSize);
  end;
end;

generic function DeCodeStream<TuInt>(Source, Dest : TStream; Size : PtrUInt; const d, m : TuInt) : PtrUInt;
var
  blockBuf : PQWord;
  data, codedData : TuInt;
  BufSize : PtrUInt;
  GainedSize : PtrUInt;
begin
  BufSize := SizeOf(TuInt);
  blockBuf:=Pointer(@data);
  codedData := 0;

  Result := 0;
  while Size >= BufSize do
  begin
      Source.ReadBuffer(codedData, SizeOf(codedData));
      data := specialize CodeRSA<TuInt>(codedData, d, m);
      GainedSize:=blockBuf[0];
      Dest.WriteBuffer(blockBuf[2], GainedSize);
      Inc(Result, GainedSize);
      Dec(Size, SizeOf(codedData));
  end;
end;

end.

