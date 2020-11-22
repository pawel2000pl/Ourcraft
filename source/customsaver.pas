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
    FOnLoad: TSaverEvent;
    procedure SetOnLoad(AValue: TSaverEvent);
  public
    property OnLoad : TSaverEvent read FOnLoad write SetOnLoad;

    procedure Save(const Path : array of AnsiString; Stream : TStream); virtual; abstract;
    procedure Load(const Path : array of AnsiString; Stream : TStream); virtual; abstract; overload;
    procedure Load(const Path : array of AnsiString); virtual; abstract; overload;
    function Exists(const Path : array of AnsiString) : Boolean; virtual; abstract;
  end;

implementation


{ TCustomSaver }

procedure TCustomSaver.SetOnLoad(AValue: TSaverEvent);
begin
  if FOnLoad=AValue then Exit;
  if (not Assigned(AValue)) then
    AValue := nil;
  FOnLoad:=AValue;
end;

end.

