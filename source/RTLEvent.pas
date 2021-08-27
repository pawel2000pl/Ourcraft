unit RTLEvent;

{$mode objfpc}{$H+}

interface

type

  { TRTLEvent }

  TRTLEvent = class
  private
    FEvent : PRTLEvent;
  public
    procedure SetUp;
    procedure Reset;
    procedure WaitFor; overload;
    procedure WaitFor(const TimeOut : LongWord); overload;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TRTLEvent }

procedure TRTLEvent.SetUp;
begin
  RTLEventSetEvent(FEvent);
end;

procedure TRTLEvent.Reset;
begin
  RTLEventResetEvent(FEvent);
end;

procedure TRTLEvent.WaitFor;
begin
  RTLEventWaitFor(FEvent);
end;

procedure TRTLEvent.WaitFor(const TimeOut: LongWord);
begin
  RTLEventWaitFor(FEvent, TimeOut);
end;

constructor TRTLEvent.Create;
begin
   FEvent:=RTLEventCreate;
   Reset;
end;

destructor TRTLEvent.Destroy;
begin
   RTLEventDestroy(FEvent);
  inherited Destroy;
end;

end.

