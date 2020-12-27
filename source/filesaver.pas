unit FileSaver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustomSaver;

type

  { TFileSaver }

  TFileSaver = class(TCustomSaver)
  private
    FPath : AnsiString;
    function ArrayPathToSystemPath(const Path : array of AnsiString) : AnsiString;
  public
    function Exists(const Path: array of AnsiString): Boolean; override;
    procedure Load(const Path: array of AnsiString; Stream: TStream); override; overload;
    procedure Load(const Path: array of AnsiString; Method: TSaveMethod); override; overload;
    procedure Load(const Path: array of AnsiString); override; overload;
    procedure Save(const Path: array of AnsiString; Stream: TStream); override;
    procedure Save(const Path: array of AnsiString; Method : TSaveMethod); override;

    constructor Create(const APath : AnsiString);
    destructor Destroy; override;
  end;

implementation

{ TFileSaver }

function TFileSaver.ArrayPathToSystemPath(const Path: array of AnsiString): AnsiString;
var
  s : AnsiString;
begin
  Result := FPath;
  for s in Path do
    Result := Result + s + DirectorySeparator;
  Delete(Result, length(Result), 1);
  ForceDirectories(ExtractFilePath(Result));
end;

function TFileSaver.Exists(const Path: array of AnsiString): Boolean;
begin
  Result := FileExists(ArrayPathToSystemPath(Path));
end;

procedure TFileSaver.Load(const Path: array of AnsiString; Stream: TStream);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(ArrayPathToSystemPath(Path), fmOpenRead);
  FS.Position:=0;
  Stream.CopyFrom(FS, FS.Size);
  FS.Free;
end;

procedure TFileSaver.Load(const Path: array of AnsiString; Method: TSaveMethod);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(ArrayPathToSystemPath(Path), fmOpenRead);  
  FS.Position:=0;
  Method(FS);
  FS.Free;
end;

procedure TFileSaver.Load(const Path: array of AnsiString);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(ArrayPathToSystemPath(Path), fmOpenRead);
  FS.Position:=0;
  DoLoadEvent(Path, FS);
  FS.Free;
end;

procedure TFileSaver.Save(const Path: array of AnsiString; Stream: TStream);
var
  FS : TFileStream;
begin
  Stream.Position:=0;
  FS := TFileStream.Create(ArrayPathToSystemPath(Path), fmCreate);
  FS.CopyFrom(Stream, Stream.Size);
  FS.Free;
end;

procedure TFileSaver.Save(const Path: array of AnsiString; Method: TSaveMethod);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(ArrayPathToSystemPath(Path), fmCreate);
  Method(FS);
  FS.Free;
end;

constructor TFileSaver.Create(const APath: AnsiString);
begin
  inherited Create;
  FPath:=APath;
  if not ForceDirectories(FPath) then
    EDirectoryNotFoundException.Create('Cannot found or create directory: "' + APath + '"');
  if FPath[length(FPath)] <> DirectorySeparator then
    FPath:=FPath+DirectorySeparator;
end;

destructor TFileSaver.Destroy;
begin
  FPath := EmptyStr;
  inherited Destroy;
end;

end.

