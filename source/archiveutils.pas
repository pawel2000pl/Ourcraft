unit ArchiveUtils;

{$mode objfpc}{$H+}{$Rangechecks off}

interface

uses
 Classes, SysUtils, Math;

type
 TArchiveProgress = procedure(Sender : TObject; const Position, Max : Integer) of object;
 TBuffEvent = procedure(Sender : TObject; const Buff : Pointer; const Size : Integer) of object;

 TClusterID = Int32;

 { TArchive }

 TArchive = class
 protected
  Stream : TStream;
  Clusters : array of TClusterID;
  Files : array of ansistring;
  function GetID(const Name : ansistring) : TClusterID;
  function AddCluster(const ID : TClusterID) : integer;
  procedure ListClusters;
  procedure ListFiles;
  procedure SaveFileList;
  function CutCluster(const Index : integer) : Pointer;
  procedure PasteCluster(const Index : integer; var Cluster : Pointer);
 public
  procedure SaveAll;
  procedure SaveChanges;
  procedure Defragment;
  procedure RenameFile(const FileName, NewFileName : ansistring);
  procedure DeleteFile(const FileName : ansistring);
  procedure GetFileList(List : TStrings);
  function FileExists(const FileName : ansistring) : boolean;
  constructor Create(Str : TStream);
  destructor Destroy; override;
 end;

 { TArchiveStream }

 TArchiveStream = class(TStream)
 private
  ClusterIndex, ByteIndex : integer;
  V_Size : QWord;
  Archive : TArchive;
  ID : TClusterID;
  Clusters : array of integer;
  ClusterCount : integer;
 protected
  function GetPosition : Int64; override;
  procedure SetPosition(const Pos : Int64); override;
  function GetSize : Int64; override;
  procedure SetSize64(const NewSize : Int64); override;
  procedure SetSize(const NewSize : Int64); override;
 public
  function Seek(const Offset : Int64; Origin : TSeekOrigin) : Int64; override;
  function Seek(Offset : LongInt; Origin : Word) : LongInt; override; overload;
  function Read(var Buffer; Count : LongInt) : LongInt; override;
  function Write(const Buffer; Count : LongInt) : LongInt; override;
  procedure SaveChanges;
  constructor Create(const FileName : ansistring; A : TArchive);
  destructor Destroy; override;
 end;

 TArchiveWorker = class(TArchiveStream)
 public
   OnProgress : TArchiveProgress;
   OnBuffRead : TBuffEvent;
   OnFinish : TNotifyEvent;
   OnBeginning : TNotifyEvent;
   procedure LoadFromFile(const FileName : AnsiString); virtual;
   procedure LoadFromStream(Stream : TStream); virtual;
   procedure SaveToFile(const FileName : AnsiString);
   procedure SaveToStream(Stream : TStream); virtual;
 end;

implementation

const
 BufSize = 4096 - SizeOf(TClusterID);
 ConfFN = '{$ArchiveConfig}';
 NoFile = '{$Erased file}';
 ConfID = -16;
 EmptyCluster = -1;

type
 TCluster = record
  ID : TClusterID;
  Data : array[0..BufSize - 1] of Byte;
 end;
 PCluster = ^TCluster;

const
 ClusterSize = SizeOf(TCluster);

procedure TArchiveWorker.LoadFromStream(Stream : TStream);
var
 Progress, Max : int64;
 m : integer;
 Buff : Array[0..BufSize-1] of byte;
begin
 If Assigned(OnBeginning) then
  OnBeginning(Self);
 Stream.Position:=0;
 Progress := Stream.Size;
 Max := Progress;
 while Progress > 0 do
 begin
    m := Min(Progress, BufSize);
    Stream.Read(Buff[0], m);
    If Assigned(OnBuffRead) then
     OnBuffRead(Self, @Buff[0], m);
    Self.Write(Buff[0], m);
 //  Self.CopyFrom(Stream, Min(Progress, BufSize));
   If Assigned(OnProgress) then
    OnProgress(Self, Max-Progress, Max);
   Dec(Progress, BufSize);
 end;
 If Assigned(OnFinish) then
   OnFinish(self);
end;

procedure TArchiveWorker.SaveToStream(Stream : TStream);
var
 Progress, Max : int64;
 m : integer;
 Buff : array[0..BufSize-1] of byte;
begin
 If Assigned(OnBeginning) then
  OnBeginning(Self);
 Self.Position:=0;
 Progress := Self.Size;
 Max := Progress;
 while Progress > 0 do
 begin
    m := Min(Progress, BufSize);
    Self.Read(Buff[0], m);
    If Assigned(OnBuffRead) then
     OnBuffRead(Self, @Buff[0], m);
    Stream.Write(Buff[0], m);
   //Stream.CopyFrom(Self, Min(Progress, BufSize));
   If Assigned(OnProgress) then
    OnProgress(Self, Max-Progress, Max);
   Dec(Progress, BufSize);
 end;
 If Assigned(OnFinish) then
   OnFinish(self);
end;

procedure TArchiveWorker.LoadFromFile(const FileName : AnsiString);
var
 FS : TFileStream;
begin
 FS := TFileStream.Create(FileName, fmOpenRead);
 LoadFromStream(FS);
 FS.Free;
end;

procedure TArchiveWorker.SaveToFile(const FileName : AnsiString);
var
 FS : TFileStream;
begin
 FS := TFileStream.Create(FileName, fmCreate);
 SaveToStream(FS);
 FS.Free;
end;

destructor TArchiveStream.Destroy;
begin
 Position:=-8;
 WriteQWord(V_Size);
 setlength(Clusters, 0);
end;

function TArchiveStream.Seek(const Offset : Int64; Origin : TSeekOrigin) : Int64;
begin
 case Origin of
  soBeginning: SetPosition(Offset);
  soCurrent: SetPosition(GetPosition + Offset);
  soEnd: SetPosition(GetSize - Offset);
 end;
 Result := Position;
end;

function TArchiveStream.Seek(Offset : LongInt; Origin : Word) : LongInt;
begin
 case Origin of
  soFromBeginning: SetPosition(Offset);
  soFromCurrent: SetPosition(GetPosition + Offset);
  soFromEnd: SetPosition(GetSize - Offset);
 end;
 Result := Position;
end;

function TArchiveStream.GetSize : Int64;
begin
 Result := V_Size;//ClusterCount * BufSize;
end;

procedure TArchiveStream.SetSize64(const NewSize : Int64);
begin
 SetSize(NewSize);
end;

procedure TArchiveStream.SetSize(const NewSize : Int64);
var
 i, c, s : integer;
 NewCount : Int64;
begin
 V_Size := NewSize;
 NewCount := NewSize div BufSize;
 if NewSize mod BufSize > 0 then
  Inc(NewCount);
 if NewCount > ClusterCount then
 begin
  s := Length(Archive.Clusters);
  c := s + (NewCount - ClusterCount);
  setlength(Archive.Clusters, c);
  for i := s to c - 1 do
   Archive.Clusters[i] := ID;
 end;
 if NewCount < ClusterCount then
 begin
  s := 0;
  c := Length(Archive.Clusters);
  for i := 0 to c - 1 do
   if Archive.Clusters[i] = ID then
   begin
    Inc(s);
    if s > NewCount then
     Archive.Clusters[i] := EmptyCluster;
   end;
 end;
 ClusterCount := NewCount;
end;

function TArchiveStream.GetPosition : Int64;
begin
 Result := ClusterIndex * BufSize + ByteIndex -8;
end;

procedure TArchiveStream.SetPosition(const Pos : Int64);
begin
 ClusterIndex := (Pos+8) div BufSize;
 ByteIndex := (Pos+8) mod BufSize;
end;

function TArchiveStream.Write(const Buffer; Count : LongInt) : LongInt;
var
 P : Pointer;
 Writed : LongInt;
 i : integer;
begin
 Writed := 0;
 P := @Buffer;
 while Count > 0 do
 begin
  if ClusterIndex >= ClusterCount then
  begin
   Inc(ClusterCount);
   SetLength(Clusters, ClusterCount);
   Clusters[ClusterCount - 1] := Archive.AddCluster(ID);
  end;
  i := Min(Count, BufSize - ByteIndex);
  Archive.Stream.Seek(Clusters[ClusterIndex] * ClusterSize + SizeOf(TCluster.ID) + ByteIndex,
   soBeginning);
  Archive.Stream.Write(P^, i);
  Inc(P, i);
  Dec(Count, i);
  Inc(ByteIndex, i);
  Inc(Writed, i);
  if ByteIndex >= BufSize then
  begin
   Inc(ClusterIndex);
   ByteIndex := 0;
  end;
 end;
 Result := Writed;
 If Position > V_Size then
  V_Size := Position;
end;

procedure TArchiveStream.SaveChanges;
var
 Poz : int64;
begin
 Poz := Position;
 Position:=-8;
 WriteQWord(V_Size);
 Position:=Poz;
end;

function TArchiveStream.Read(var Buffer; Count : LongInt) : LongInt;
var
 P : Pointer;
 Readed : LongInt;
 i : integer;
begin
 Readed := 0;
 P := @Buffer;
 while Count > 0 do
 begin
  if ClusterIndex >= ClusterCount then
   Break;
  i := Min(Count, BufSize - ByteIndex);
  Archive.Stream.Seek(Clusters[ClusterIndex] * ClusterSize + SizeOf(TClusterID) + ByteIndex,
   soBeginning);
  Archive.Stream.Read(P^, i);
  Inc(P, i);
  Dec(Count, i);
  Inc(ByteIndex, i);
  Inc(Readed, i);
  if ByteIndex >= BufSize then
  begin
   Inc(ClusterIndex);
   ByteIndex := 0;
  end;
 end;
 Result := Readed;
end;

constructor TArchiveStream.Create(const FileName : ansistring; A : TArchive);
var
 i, c : integer;
begin
 inherited Create();
 Archive := A;
 ID := Archive.GetID(FileName);
 c := Length(Archive.Clusters);
 ClusterCount := 0;
 setlength(Clusters, ClusterCount);
 if c > 0 then
 begin
  for i := 0 to c - 1 do
   if Archive.Clusters[i] = ID then
    Inc(ClusterCount);
  setlength(Clusters, ClusterCount);
  ClusterCount := 0;
  for i := 0 to c - 1 do
   if Archive.Clusters[i] = ID then
   begin
    Clusters[ClusterCount] := i;
    Inc(ClusterCount);
   end;
 end;
 ClusterIndex := 0;
 ByteIndex := 0;
 If ClusterCount > 0 then
  V_Size := ReadQWord
  else
  begin
  V_Size := 0;
  WriteQWord(V_Size);
  end;
end;

constructor TArchive.Create(Str : TStream);
begin
 Stream := Str;
 ListClusters;
 SetLength(Files, 0);
 ListFiles;
end;

procedure TArchive.ListFiles;
var
 S : TArchiveStream;
 i, c : integer;
begin
 S := TArchiveStream.Create(ConfFN, Self);
 if S.Size > 0 then
 begin
  c := S.ReadDWord;
  setlength(Files, c);
  if c > 0 then
  begin
   for i := 0 to c - 1 do
    Files[i] := S.ReadAnsiString;
  end;
 end;
 S.Free;
end;

procedure TArchive.SaveFileList;
var
 S : TArchiveStream;
 i, c : integer;
begin
 S := TArchiveStream.Create(ConfFN, Self);
 S.Position := 0;
 S.Size := 0;
 c := Length(Files);
 S.WriteDWord(c);
 if c > 0 then
  for i := 0 to c - 1 do
   S.WriteAnsiString(Files[i]);
 S.Free;
end;

function TArchive.GetID(const Name : ansistring) : TClusterID;
var
 i, c : TClusterID;
begin
 if Name = ConfFN then
 begin
  Result := ConfID;
  Exit;
 end;
 c := Length(Files);
 if c > 0 then
  for i := 0 to c - 1 do
   if Files[i] = Name then
   begin
    Result := i;
    Exit;
   end;
 if c > 0 then
  for i := 0 to c - 1 do
   if Files[i] = NoFile then
   begin
    Files[i] := Name;
    Result := i;
    Exit;
   end;
 Result := c;
 Inc(c);
 Setlength(Files, c);
 Files[Result] := Name;
end;

procedure TArchive.ListClusters;
var
 Size : int64;
 i, Count : integer;
begin
 Size := Stream.Size;
 Count := Size div SizeOf(TCluster);
 setlength(Clusters, Count);
 if Count > 0 then
  for i := 0 to Count - 1 do
  begin
   Stream.Seek(i * ClusterSize, soFromBeginning);
   Stream.Read(Clusters[i], SizeOf(TClusterID));
  end;
end;

function TArchive.AddCluster(const ID : TClusterID) : integer;
var
 c : integer;
 TMP : TCluster;
begin
 c := Length(Clusters);
 setlength(Clusters, c + 1);
 Clusters[c] := ID;
 TMP.ID:=ID;
 Stream.Seek(c * ClusterSize, soBeginning);
 Stream.Write(TMP, SizeOf(TCluster));
 Result := c;
end;

function TArchive.CutCluster(const Index : integer) : Pointer;
var
 TMP : PCluster;
begin
 TMP := AllocMem(ClusterSize);
 Stream.Seek(Index * ClusterSize, soBeginning);
 Stream.Read(TMP^, ClusterSize);
 Stream.Seek(Index * ClusterSize, soBeginning);
 Clusters[index] := EmptyCluster;
 Stream.Write(Clusters[index], SizeOf(TClusterID));
 Result := TMP;
end;

procedure TArchive.PasteCluster(const Index : integer; var Cluster : Pointer);
begin
 Stream.Seek(Index * ClusterSize, soBeginning);
 Clusters[Index] := PCluster(Cluster)^.ID;
 Stream.Write(Cluster^, ClusterSize);
 Freemem(Cluster, ClusterSize);
end;

procedure TArchive.SaveAll;
begin
  SaveFileList;
  Defragment;
  SaveChanges;
end;

procedure TArchive.SaveChanges;
var
 i, c : integer;
begin
 c := Length(Clusters);
 if c > 0 then
  for i := 0 to c - 1 do
  begin
   Stream.Seek(i * ClusterSize, soBeginning);
   Stream.Write(Clusters[i], SizeOf(TClusterID));
  end;
end;

procedure TArchive.Defragment;
var
 i, Size, i2 : integer;
 ID : integer;
 b : boolean;
 Cluster : PCluster;
begin
 SaveChanges;
 Size := Stream.Size div ClusterSize;
 if Size = 0 then
  Exit;
 i := -1;
 repeat
  Inc(i);
  ID := Clusters[i];
  if ID = EmptyCluster then
  begin
   b := False;
   for i2 := i to Size - 1 do
   begin
    ID := Clusters[i2];
    if ID <> EmptyCluster then
    begin
     Cluster := CutCluster(i2);
     PasteCluster(i, Cluster);
     b := True;
     Break;
    end;
   end;
   if not b then
    Break;
  end;
 until i >= Size - 1;
 Stream.Size := i * ClusterSize;
 ListClusters;
end;

procedure TArchive.RenameFile(const FileName, NewFileName : ansistring);
var
 i, c : integer;
begin
 c := Length(Files);
 if c > 0 then
  for i := 0 to c - 1 do
   if Files[i] = FileName then
   begin
    Files[i] := NewFileName;
    Exit;
   end;
end;

procedure TArchive.DeleteFile(const FileName : ansistring);
var
 ID : TClusterID;
 i, c : integer;
begin
 if not Self.FileExists(FileName) then
  Exit;
 ID := GetID(FileName);
 c := Length(Clusters);
 if c > 0 then
  for i := 0 to c - 1 do
   if Clusters[i] = ID then
   begin
    Clusters[i] := EmptyCluster;
    Stream.Seek(i * ClusterSize, soBeginning);
    Stream.Write(Clusters[i], SizeOf(TClusterID));
   end;
 RenameFile(FileName, NoFile);
end;

procedure TArchive.GetFileList(List : TStrings);
var
 i, c : integer;
begin
 c := Length(Files);
 if c > 0 then
  for i := 0 to c - 1 do
   if (Files[i] <> NoFile) and (Files[i] <> ConfFN) then
    List.Add(Files[i]);
end;

function TArchive.FileExists(const FileName : ansistring) : boolean;
var
 i, c : integer;
begin
 c := Length(Files);
 if c > 0 then
  for i := 0 to c - 1 do
   if (Files[i] = FileName) then
   begin
    Result := True;
    Exit;
   end;
 Result := False;
end;

destructor TArchive.Destroy;
var
 i, c : integer;
begin
 Defragment;
 SaveFileList;
 SaveChanges;
 c := Length(Files);
 if c > 0 then
  for i := 0 to c - 1 do
   Setlength(Files[i], 0);
 SetLength(Files, 0);
 SetLength(Clusters, 0);
end;

end.
