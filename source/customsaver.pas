unit CustomSaver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCustomSaver = class;
  TSaverEvent = procedure(Saver : TCustomSaver; const Path : array of AnsiString; Stream : TStream) of object;
  TSaveMethod = procedure(Stream : TStream) of object;
  TLoadMethod = TSaveMethod;

  { TCustomSaver }

  TCustomSaver = class abstract
  private
    FDestroyWithOwner: Boolean;
    FOnLoad: TSaverEvent;
    procedure SetOnLoad(AValue: TSaverEvent);
  protected
    procedure DoLoadEvent(const Path : array of AnsiString; Stream : TStream);
  public
    property OnLoad : TSaverEvent read FOnLoad write SetOnLoad;
    property DestroyWithOwner : Boolean read FDestroyWithOwner write FDestroyWithOwner;
                                                                            
    function Exists(const Path : array of AnsiString) : Boolean; virtual; abstract;
    procedure Save(const Path : array of AnsiString; Stream : TStream); virtual; abstract; overload;
    procedure Save(const Path : array of AnsiString; Method : TSaveMethod); virtual; overload;
    ///do not execute OnLoad
    procedure Load(const Path : array of AnsiString; Stream : TStream); virtual; abstract; overload;
    procedure Load(const Path : array of AnsiString; Method : TSaveMethod); virtual; overload;
    ///do execute OnLoad
    procedure Load(const Path : array of AnsiString); virtual; overload;

    constructor Create;
  end;

implementation


{ TCustomSaver }

procedure TCustomSaver.DoLoadEvent(const Path: array of AnsiString;
  Stream: TStream);
begin
  if FOnLoad <> nil then
    FOnLoad(Self, Path, Stream);
end;

procedure TCustomSaver.Save(const Path: array of AnsiString; Method: TSaveMethod);
var
  MS : TMemoryStream;
begin
  MS := TMemoryStream.Create;
  Method(MS);
  Save(Path, MS);
  MS.Free;
end;

procedure TCustomSaver.Load(const Path: array of AnsiString; Method: TSaveMethod);
var
  MS : TMemoryStream;
begin
  MS := TMemoryStream.Create;
  Load(Path, MS);
  MS.Position:=0;
  Method(MS);
  MS.Free;
end;

procedure TCustomSaver.SetOnLoad(AValue: TSaverEvent);
begin
  if FOnLoad=AValue then Exit;
  if (not Assigned(AValue)) then
    AValue := nil;
  FOnLoad:=AValue;
end;

procedure TCustomSaver.Load(const Path: array of AnsiString);
var
  ms : TMemoryStream;
begin
  ms := TMemoryStream.Create;
  Load(Path, ms);
  ms.Position:=0;
  DoLoadEvent(Path, ms);
  ms.Free;
end;

constructor TCustomSaver.Create;
begin
   FDestroyWithOwner:=true;
end;

end.

