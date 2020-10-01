unit Chain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TChain }

  generic TChain<TValue> = class
  type
    TChainLink = record
      Value : TValue;
      Prev : ^TChainLink;
    end;
    PChainLink = ^TChainLink;
  private
    fEnd, fBegin : PChainLink;
  public
    procedure Add(const Value : TValue);
    function Get : TValue;
    function GetAndSwitch : TValue;
    function SwitchNext : Boolean;
    function IsEmpty : Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  EEmptyChain = class(Exception);

implementation

{ TChain }

procedure TChain.Add(const Value: TValue);
var
  tmp : PChainLink;
begin
  tmp := AllocMem(sizeof(TChainLink));
  tmp^.Prev:=nil;
  tmp^.Value:=Value;
  if fBegin <> nil then
    fBegin^.Prev:=tmp
    else
    fEnd := tmp;
  fBegin := tmp;
end;

function TChain.Get : TValue;
begin
  if IsEmpty then
    EEmptyChain.Create('Chain is empty')
    else
    Result := fEnd^.Value;
end;

function TChain.GetAndSwitch: TValue;
begin
  Result := Get;
  SwitchNext;
end;

function TChain.SwitchNext: Boolean;
var
  tmp : PChainLink;
begin
  if IsEmpty then
    exit(false);
  tmp := fEnd;
  fEnd:=tmp^.Prev;
  FreeMem(tmp);
  if fEnd = nil then
    fBegin := nil;
  Result := true;
end;

function TChain.IsEmpty: Boolean;
begin
  Result := fEnd = nil;
end;

constructor TChain.Create;
begin
  fBegin:=nil;
  fEnd:=nil;
end;

destructor TChain.Destroy;
begin
  while SwitchNext do;
  inherited Destroy;
end;

end.

