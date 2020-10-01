program OutCraft;

{$mode objfpc}
{$RangeChecks off}

uses
    SysUtils, Classes, OurUtils, OurData;

type
    TTest = class
    public
        procedure Ab; virtual; abstract;
    end;

var
    data : TOurData;
    test : ansistring = '(abc,"",(2abc2,2d\ ef2),(3abc3,3def3))';
    list : TStringList;
    i : Integer;

    ot : TTest;
begin
    data := TOurData.Create;

    data.load(test);
    writeln;
    writeln(data.save);    
    writeln;

    list := data.GetSectionList;
    for i := 0 to list.Count-1 do
        writeln(list[i]);
    list.free;

    i := 20;
    writeln(BinaryToString(i, 4));
    
    data.free;

    ot := TTest.Create;
    writeln(TMethod(@ot.ab).Code = @system.AbstractErrorProc);
    
    writeln('done.');
end.
