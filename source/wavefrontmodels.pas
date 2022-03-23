unit WavefrontModels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Models, CalcUtils, TextureMode, LightTypes;

type

  TWaveFrontObject = record
    Name : AnsiString;
    Vertex : array of TVector3;
    VertexTextures : array of TTexture2d;
    VertexNormals : array of TVector3;
    Faces : array of array of array[0..2] of Integer;
  end;
  PWaveFrontObject = ^TWaveFrontObject;

  { TWaveFrontModel }

  TWaveFrontModel = class
  private
    fObjects : array of TWaveFrontObject;
  public
    function GetObject(const name : AnsiString) : PWaveFrontObject;
    procedure LoadFromStream(Stream : TStream);
    procedure LoadFromFile(const FileName : AnsiString);
    procedure RenderToModel(const ObjectName : AnsiString; Model : TVertexModel; const Tex: PTextureRect; const Light : TRealLight);
  end;

implementation

{ TWaveFrontModel }

function TWaveFrontModel.GetObject(const name: AnsiString): PWaveFrontObject;
var
  i : Integer;
begin
  for i := 0 to Length(fObjects)-1 do
      if SameStr(fObjects[i].Name, name) then
         Exit(@fObjects[i]);
  Exit(nil);
end;

procedure TWaveFrontModel.LoadFromStream(Stream: TStream);
var
  source : TStringList;
  line : AnsiString;
  parts, subparts : TStringArray;
  i, j : Integer;
begin                        
  fObjects := [];
  source := TStringList.Create;
  source.LoadFromStream(Stream);
  try
    for i := 0 to source.Count-1 do
    begin
      line := trim(source[i]);
      if (Length(line) = 0) or (line[1] = '#') then
         Continue;
      parts := line.Split([#9, #32]);

      if parts[0] = 'o' then
      begin
        SetLength(fObjects, Length(fObjects)+1);
        fObjects[High(fObjects)].Name:=parts[1];
        Continue;
      end;

      if parts[0] = 'v' then
        with fObjects[High(fObjects)] do
        begin
          SetLength(Vertex, Length(Vertex)+1);
          Vertex[High(Vertex)] := Vector3(StrToFloat(parts[1]), StrToFloat(parts[2]), StrToFloat(parts[3]));
          Continue;
        end;

      if parts[0] = 'vt' then
        with fObjects[High(fObjects)] do
        begin
          SetLength(VertexTextures, Length(VertexTextures)+1);
          VertexTextures[High(VertexTextures)][axisX] := StrToFloat(parts[1]);
          VertexTextures[High(VertexTextures)][axisY] := StrToFloat(parts[2]);
          Continue;
        end;

      if parts[0] = 'vn' then
        with fObjects[High(fObjects)] do
        begin
          SetLength(VertexNormals, Length(VertexNormals)+1);
          VertexNormals[High(VertexNormals)] := Vector3(StrToFloat(parts[1]), StrToFloat(parts[2]), StrToFloat(parts[3]));
          Continue;
        end;

      if parts[0] = 'f' then
        with fObjects[High(fObjects)] do
        begin
          SetLength(Faces, Length(Faces)+1);
          for j := 1 to Length(parts)-1 do
          begin
            subparts := (parts[j] + '///').Split(['/']);
            SetLength(Faces[High(Faces)], Length(Faces[High(Faces)]) + 1);
            Faces[High(Faces)][High(Faces[High(Faces)])][0] := StrToIntDef(subparts[0], 0);
            Faces[High(Faces)][High(Faces[High(Faces)])][1] := StrToIntDef(subparts[1], 0);
            Faces[High(Faces)][High(Faces[High(Faces)])][2] := StrToIntDef(subparts[2], 0);
          end;
          Continue;
        end;
    end;
  finally   
     source.Free;
  end;
end;

procedure TWaveFrontModel.LoadFromFile(const FileName: AnsiString);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TWaveFrontModel.RenderToModel(const ObjectName: AnsiString;
  Model: TVertexModel; const Tex: PTextureRect; const Light: TRealLight);
var
  Obj : PWaveFrontObject;
  i, j, vertexIndex, textureIndex, normalIndex : Integer;
  subModel : TVertexModel;
begin
  Obj := GetObject(ObjectName);
  if Obj = nil then
   Exit;
  subModel := TVertexModel.Create;

  for i := 0 to Length(Obj^.Faces)-1 do
  begin
    subModel.Clear;              

    for j := 0 to Length(Obj^.Faces[i])-1 do
    begin
      vertexIndex := Obj^.Faces[i][j][0];
      textureIndex := Obj^.Faces[i][j][1];
      normalIndex := Obj^.Faces[i][j][2];
      if vertexIndex = 0 then Inc(vertexIndex);
      if textureIndex = 0 then Inc(textureIndex);
      if normalIndex = 0 then Inc(normalIndex);
      if vertexIndex > 0 then
         Dec(vertexIndex)
         else
         vertexIndex := Length(Obj^.Vertex) + vertexIndex;
      if textureIndex > 0 then
         Dec(textureIndex)
         else
         textureIndex := Length(Obj^.VertexTextures) + textureIndex;
      if normalIndex > 0 then
         Dec(normalIndex)
         else
         normalIndex := Length(Obj^.VertexNormals) + normalIndex;

      subModel.AddVertex(Obj^.Vertex[vertexIndex], Light, Obj^.VertexTextures[textureIndex], Tex);
    end;

    Model.AddVertexModel(subModel);
  end;
  Model.CullFace:=Model.cfNone;
  subModel.Free;
end;

end.

