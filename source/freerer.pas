unit Freerer;

{$mode objfpc}{$H+}

interface

uses
  cThreads, Classes, SysUtils, Locker, Incrementations;

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

procedure TFree.FreeObject(Obj: TObject; const Delay: QWord);
begin                     
  if Obj is TSimpleFreeObject then
    (Obj as TSimpleFreeObject).Finalize(Delay);
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
          o.Free;
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
    objs[i].Obj.Free;
  terminating := False;
end;

end.
