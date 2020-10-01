program preprocesor;

uses
  SysUtils,
  Classes;

  function ReadString(var s : ansistring) : boolean;
  var
    chr : char;
  begin
    s := '';
    repeat
      Read(chr);
      s := s + chr;
    until chr in [#13, #10, #26, #3];

    Result := chr in [#26, #3];
  end;

const
  Modes : array[0..2, 0..2] of ansistring = (
    ('./source/BlocksLoader.pas', 'Block', 'Blocks'),
    ('./source/ItemsLoader.pas', 'Item', 'Items'),
    ('./source/EnititiesLoader.pas', 'Entity', 'Entities'));


var
  Source : TStringList;
  Units, Loaders : TStringList;
  List : TStringList;
  s, fn : ansistring;
  i, c, mode : integer;
begin
  Source := TStringList.Create;
  Units := TStringList.Create;
  Loaders := TStringList.Create;

  if ParamStr(1) = 'blocks' then
    mode := 0
  else if ParamStr(1) = 'items' then
    mode := 1
  else
    mode := 2;


  fn := Modes[mode, 0];
  Source.LoadFromFile(fn + '.generic');
  Source.Insert(0, '//This file was generated automatically');

  List := TStringList.Create;

  c := 0;
  while not ReadString(s) do
  begin
    s := trim(s);
    if (s = '') or (extractFileExt(s) <> '.pas') then
      continue;
    List.add(s);
    Inc(c);
  end;

  for i := 0 to c - 1 do
  begin
    s := copy(List[i], 1, pos('.', List[i]) - 1);
    writeln('Detected ' + Modes[mode, 1] + ': ' + s);
    units.add(', ' + s);
    Loaders.Add('    ' + s + '.Load' + Modes[mode, 2] + '(@LoadBlock, fGame);');
  end;

  s := StringReplace(Units.Text, #13, '', [rfReplaceAll]);
  s := StringReplace(s, #10, '', [rfReplaceAll]);

  Source.Text := StringReplace(StringReplace(Source.Text, '{%units}',
    s, [rfReplaceAll, rfIgnoreCase]), '{%loaders}', Loaders.Text,
    [rfReplaceAll, rfIgnoreCase]);

  List.Free;
  Units.Free;
  Loaders.Free;
  Source.SaveToFile(fn);
  Source.Free;
end.
