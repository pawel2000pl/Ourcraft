unit Freerer;

{$mode objfpc}{$H+}
{$Interfaces CORBA}

interface

uses
  cThreads, Classes, SysUtils, Locker, PostPreOperations;

const
  SimpleFreeInterfaceGUID = '{AA6DF291-4739-469A-9A88-EA09DAE0670E}';
  FreeInterfaceGUID = '{EAC41D3E-F7D0-467C-BAAC-A1EB5C04802C}';

type

  TSimpleFreeObject = class
  protected
    procedure Finalize(const DelayTime : QWord); virtual; abstract;
  end;

  { TFreeObject }

  TFreeObject = class(TSimpleFreeObject)
  private
    fFinishing : boolean;
  protected
    procedure Finalize(const {%H-}DelayTime : QWord); override;
  public            
    property Finishing : boolean read fFinishing;
    constructor Create;
  end;

  ISimpleFreeInterface = interface
    [SimpleFreeInterfaceGUID]
    procedure Finalize(const DelayTime : QWord); virtual;
  end;

  IFreeInterface = interface(ISimpleFreeInterface)
    [FreeInterfaceGUID]
    property Finishing : boolean;
  end;
          
  TFreeRecord = record
    Obj : TObject;
    FreeAfter : QWord;
  end;

  { TFree }

  TFree = class(TThread)
  private
    n : integer;
    objs : array of TFreeRecord;
    time : QWord;
    fCriticalCestion : TLocker;
    procedure Finalize(Obj : TObject; const Delay : QWord);
  protected
    terminating : boolean;
  public
    property QueueCount : Integer read n;
    procedure FreeObject(Obj : TObject; const Delay : QWord = 100);
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

{ TFreeObject }

procedure TFreeObject.Finalize(const DelayTime: QWord);
begin
  fFinishing := true;
end;

constructor TFreeObject.Create;
begin
  inherited Create();
  fFinishing := false;
end;

{ TFree }

procedure TFree.Finalize(Obj: TObject; const Delay: QWord);
var
  fi : ISimpleFreeInterface;
begin
  if Obj is TSimpleFreeObject then
    (Obj as TSimpleFreeObject).Finalize(Delay);
  if Obj.GetInterface(SimpleFreeInterfaceGUID, fi) then
    fi.Finalize(Delay);
end;

procedure TFree.FreeObject(Obj: TObject; const Delay: QWord);
begin
  Finalize(Obj, Delay);
  fCriticalCestion.Lock;
  try
    setlength(objs, PreInc(n));
    objs[n-1].Obj := Obj;   
    objs[n-1].FreeAfter := time + Delay;
  finally
    fCriticalCestion.Unlock;
  end;
end;

constructor TFree.Create;
begin
  inherited Create(True);
  Start;
end;

destructor TFree.Destroy;
begin
  terminating:=true;
  WaitFor;
  inherited Destroy;
end;

procedure TFree.Execute;
var
  i : integer;
  o : TObject;
begin
  terminating := False;
  n := 0;
  setlength(Objs, n);
  fCriticalCestion := TLocker.Create;

  repeat
    time := GetTickCount64;
        
    fCriticalCestion.Lock;
    i := 0;
    while (i < n) do
    begin
      if objs[i].FreeAfter < time then
      begin
        o := objs[i].Obj;   
        objs[i] := objs[PreDec(n)];
        fCriticalCestion.Unlock;
        if Assigned(o) then
          try
            o.Free;
          except
            on E : Exception do ;
          end;
        fCriticalCestion.Lock;
      end
      else
        Inc(i);
    end;

    setlength(objs, n);
    fCriticalCestion.Unlock;

    sleep(100);
  until terminating;

  fCriticalCestion.Free;

  for i := 0 to Length(objs) - 1 do
  begin
    while objs[i].FreeAfter >= GetTickCount64 do
      Yield;
    try
       objs[i].Obj.Free;
    except
      on E : Exception do ;
    end;
  end;
  terminating := False;
end;

end.
