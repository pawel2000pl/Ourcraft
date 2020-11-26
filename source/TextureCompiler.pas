program TextureCompiler;

{$Mode objfpc}

uses
  SysUtils,
  Classes,
  UniversalImage,
  CalcUtils,
  Math;


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

var
    Area : array of array of boolean;
    AreaHeight : Integer = 256;

  function FindFreePlace2(Texture : TUniversalImage;
  const Width, Height : integer) : TPoint;
  var
    x, y, i, j : integer;
    ok : boolean;
begin 
    for x := 0 to length(Area) - Width do
      for y := 0 to AreaHeight - Height do
      begin
        ok := True;
        for i := 0 to Width - 1 do
          if ok then
            for j := 0 to Height - 1 do
              if (Area[x + i, y + j]) then
              begin
                ok := False;
                break;
              end;

        if not ok then
          continue;
        Result := Point(x, y);
        exit;
      end;

    Result := Point(Texture.Width, 0);

    j := length(Area);

    Texture.Width := Texture.Width + Width;
    Setlength(Area, length(Area) + Width, AreaHeight);
    for i := j to length(Area) - 1 do
      for y := 0 to AreaHeight - 1 do
        Area[i, y] := False; 
  end;

var
  FileName : ansistring;
  Texture, ToAdd, framed : TUniversalImage;
  p : TPoint;
  i, j, x, y, wx, wy : integer;
begin
  Texture := TUniversalImage.CreateEmpty;

  Texture.SetSize(34, AreaHeight);
  setlength(Area, 34, AreaHeight);

  try
    while not ReadString(FileName) do
    begin
      FileName := trim(FileName);
      ToAdd := TUniversalImage.CreateEmpty;
      ToAdd.LoadFromFile(FileName);

      wx := ToAdd.Width;
      wy := ToAdd.Height;
      framed := TUniversalImage.Create(wx+2, wy+2);
      framed.draw(1, 1, ToAdd);
      for x := 1 to wx do
      begin
          framed.DirectColor[x, 0] := framed.DirectColor[x, 1];
          framed.DirectColor[x, wy+1] := framed.DirectColor[x, wy];
      end;

      for y := 0 to wy+1 do
      begin
          framed.DirectColor[0, y] := framed.DirectColor[1, y];
          framed.DirectColor[wx+1, y] := framed.DirectColor[wx, y];
      end;
      
      ToAdd.free;
      ToAdd := framed;

      if ToAdd.Height > AreaHeight then
      begin
          AreaHeight := ToAdd.Height;
          Texture.Height := AreaHeight;
          setlength(Area, length(Area), AreaHeight);
      end;
    
      p := FindFreePlace2(Texture, ToAdd.Width, ToAdd.Height);

      Texture.Draw(p.X, p.Y, ToAdd);
      
      for x := p.x to p.x + ToAdd.Width - 1 do
        for y := p.y to p.y + ToAdd.Height - 1 do
          area[x, y] := True;

      writeln(p.X+1, #9, p.Y+1, #9, p.X + wx, #9, p.Y + wy,
        #9, FileName);
      ToAdd.Free;
    end; 
  finally

    Texture.SaveToFile('Rendered.png');
    Texture.Free;
  end;

end.
