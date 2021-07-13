program Demo;

{$Mode ObjFpc}

uses
    cthreads, Classes, DoubleInt, RSAKeys, AnyBase2OtherBase, RSACodeStream,
    PrimeLib, RSAMultiKeyGenerator;


function PowerMod(const x, n, m : QWord) : QWord;
begin
  if n = 0 then
     exit(1);
  if (n and 1) = 1 then
    exit(sqr(PowerMod(x, n shr 1, m)) mod m * x mod m)
  else
    exit(sqr(PowerMod(x, n shr 1, m)) mod m);
end;


var
    d, e, data, n : uInt512;
    FS1, FS2, FS3 : TFileStream;
    i : Integer;
    codes : specialize TArrayOfRSAKeySearcher<uInt512>;
begin
    randomize;        
    writeln('Started, ', random(10000));
     {
    specialize GenerateRSAKey<uInt512>(d, e, n);

    writeln('d = ', Base2Base(d.ToBinaryString, 2, 10));
    writeln('e = ', Base2Base(e.ToBinaryString, 2, 10));
    writeln('n = ', Base2Base(n.ToBinaryString, 2, 10));

    writeln;

    data := 10;
    writeln('data1 = ', Base2Base(data.ToBinaryString, 2, 10));
    data := specialize CodeRSA<uInt512>(data, e, n);
    writeln('data2 = ', Base2Base(data.ToBinaryString, 2, 10));
    data := specialize CodeRSA<uInt512>(data, d, n);
    writeln('data3 = ', Base2Base(data.ToBinaryString, 2, 10));


    Writeln('/////////////////// file tests ///////////////////////////');

    FS1 := TFileStream.Create('Ciastka brownie.odt', fmOpenRead);
    FS2 := TFileStream.Create('Coded.bin', fmCreate);
    FS3 := TFileStream.Create('Ciastka brownie Decoded.odt', fmCreate);

    Writeln('Saved ', specialize CodeStream<uInt512>(FS1, FS2, FS1.Size, e, n), ' bytes');
    FS2.Position := 0;                                       
    Writeln('Saved ', specialize DeCodeStream<uInt512>(FS2, FS3, FS2.Size, d, n), ' bytes');

    FS1.Free;
    FS2.Free;
    FS3.Free;

    }


    Writeln('/////////////////// multiGenerator ///////////////////////////');

    specialize GenerateRSAKeys<uInt512>(16, codes);
    for i := 0 to 15 do
    begin
      writeln(i);
      writeln('d = ', Base2Base(codes[i].d.ToBinaryString, 2, 10));
      writeln('e = ', Base2Base(codes[i].e.ToBinaryString, 2, 10));
      writeln('n = ', Base2Base(codes[i].n.ToBinaryString, 2, 10));
      codes[i].Free;
      writeln;
    end;

    writeln('Done');
end.

