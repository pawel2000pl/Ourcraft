unit SynchronizedCounter;

{$Mode ObjFpc}

interface

uses Locker;

type
    TSynchronizedCounter = class
    private
        fValue : Integer;
        Locker : TLocker;

        procedure procSetValue(const AValue : Integer);
    public
        property Value : Integer read fValue write procSetValue;

        procedure IncValue(const count : Integer = 1);
        procedure DecValue(const count : Integer = 1);
        function PostInc(const count : Integer = 1) : Integer;
        function PostDec(const count : Integer = 1) : Integer;
        function PreInc(const count : Integer = 1) : Integer;
        function PreDec(const count : Integer = 1) : Integer;
        function SetValue(const AValue : Integer) : Integer;
    
        constructor Create(const AValue : Integer = 0);
        destructor Destroy; override;

    end;

implementation

function TSynchronizedCounter.PostInc(const count : Integer) : Integer;
begin
    Locker.Lock;
    Result := FValue;
    Inc(FValue);
    Locker.Unlock;
end;

function TSynchronizedCounter.PostDec(const count : Integer) : Integer;
begin
    Locker.Lock;
    Result := FValue;
    Dec(FValue);
    Locker.Unlock;
end;

function TSynchronizedCounter.PreInc(const count : Integer) : Integer;
begin
    Locker.Lock;
    Inc(FValue);
    Result := FValue;
    Locker.Unlock;
end;

function TSynchronizedCounter.PreDec(const count : Integer) : Integer;
begin
    Locker.Lock;
    Dec(FValue);
    Result := FValue;
    Locker.Unlock;
end;

function TSynchronizedCounter.SetValue(const AValue : Integer) : Integer;
begin
    Locker.Lock;
    Result := FValue;
    FValue := AValue;
    Locker.Unlock;
end;

procedure TSynchronizedCounter.IncValue(const count : Integer);
begin
    Locker.Lock;
    Inc(fValue, count);
    Locker.Unlock
end;

procedure TSynchronizedCounter.DecValue(const count : Integer);
begin
    Locker.Lock;
    Dec(fValue, count);
    Locker.Unlock
end;

procedure TSynchronizedCounter.procSetValue(const AValue : Integer);
begin
    Locker.Lock;
    fValue := AValue;
    Locker.Unlock;
end;

constructor TSynchronizedCounter.Create(const AValue : Integer);
begin
    fValue := AValue;
    Locker := TLocker.Create;
end;

destructor TSynchronizedCounter.Destroy;
begin
    Locker.Free;
    inherited;
end;

end.
