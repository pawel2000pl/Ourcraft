program Demo;

{$Mode ObjFpc}

uses
    cthreads, SysUtils, Classes, DoubleInt, RSAKeys, AnyBase2OtherBase, RSACodeStream,
    PrimeLib, RSAMultiKeyGenerator, MainOurcraftDirectory, crc;

var
    t : QWord;
    ba, bb, bc : uInt65536;
    d, e, data, n : uInt512;
    FS1, FS2, FS3 : TMemoryStream;
    i : Integer;
    codes : specialize TArrayOfRSAKeySearcher<uInt512>;
begin

    bb := 150;
    writeln((bb*17+3) mod 17 = 3);

    t := GetTickCount64;
    ba := 10;
    bb := 102;
    bc := 17;
    bc := ba*bb*bc+11;
    bc := ba*bb*bc+11;
    bc := ba*bb*bc+11;
    bc := ba*bb*bc+11;
    bc := ba*bb*bc+11;
    bc := ba*bb*bc mod (bc-1)+11;
    bc := ba*bb*bc mod (bc-1)+11;
    bc := ba*bb*bc mod (bc-1)+11;
    bc := ba*bb*bc mod (bc-1)+11;
    bc := ba*bb*bc mod (bc-1)+11;
    bc := ba*bb*bc mod (bc-1)+11;
    bc := ba*bb*bc mod (bc-1)+11;
    bc := ba*bb*bc mod (bc-1)+11;
    bc := ba*bb*bc mod (bc-1)+11;

    writeln(GetTickCount64-t);       
    writeln((bb*17+3) mod 17 = 3);

    SetCurrentDir('source/Crypto/');
    randomize;
    writeln('Started, ', random(10000));
    t := GetTickCount64;

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

    writeln('Generated in: ', GetTickCount64 - t);

    Writeln('/////////////////// file tests ///////////////////////////');

    t := GetTickCount64;
    FS1 := TMemoryStream.Create;
    FS2 := TMemoryStream.Create;
    FS3 := TMemoryStream.Create;

    FS1.LoadFromFile('Ciastka brownie.odt');

    Writeln('Saved ', specialize CodeStream<uInt512>(FS1, FS2, FS1.Size, e, n), ' bytes'); 
    FS2.SaveToFile('Coded.bin');
    FS2.Position := 0;                                       
    Writeln('Saved ', specialize DeCodeStream<uInt512>(FS2, FS3, FS2.Size, d, n), ' bytes');

    t :=  GetTickCount64 - t;
    writeln('Done, efficient-level: ', FS1.Size/(t/2000)/1024:4:4, ' kB/s');

    FS3.SaveToFile('Ciastka brownie Decoded.odt');
    FS1.Free;
    FS2.Free;
    FS3.Free;

{
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
    }
    writeln('Done');
end.

