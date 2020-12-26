unit CustomSaver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCustomSaver = class;
  TSaverEvent = procedure(Saver : TCustomSaver; const Path : array of AnsiString; Stream : TStream) of object;

  { TCustomSaver }

  TCustomSaver = class abstract
  private
    FDestroyWithOwner: Boolean;
    FOnLoad: TSaverEvent;
    procedure DoLoadEvent(const Path : array of AnsiString; Stream : TStream);
    procedure SetOnLoad(AValue: TSaverEvent);
  public
    property OnLoad : TSaverEvent read FOnLoad write SetOnLoad;
    property DestroyWithOwner : Boolean read FDestroyWithOwner write FDestroyWithOwner;
                                                                            
    function Exists(const Path : array of AnsiString) : Boolean; virtual; abstract;
    procedure Save(const Path : array of AnsiString; Stream : TStream); virtual; abstract;
    ///do not execute OnLoad
    procedure Load(const Path : array of AnsiString; Stream : TStream); virtual; abstract; overload;
    ///do execute OnLoad
    procedure Load(const Path : array of AnsiString); overload;

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

