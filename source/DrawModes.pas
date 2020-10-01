unit DrawModes;

interface

{$mode objfpc}

uses 
    math, fpImage;

type
    TDrawMode = (dmNormal, dmDissolve, dmMultiply, dmDivide, dmScreen, dmOverlay, dmDodge, dmBurn, dmHardLight, dmSoftLight, dmGrainExtract, dmGrainMerge, dmDifference, dmAddition, dmSubtract, dmDarkenOnly, dmLightenOnly, dmHue, dmSaturation, dmColor, dmValue);

    TDrawFunction = function(const a, b : TFPColor) : TFPColor;
    
function GetDrawFunction(const mode : TDrawMode) : TDrawFunction;
function min(const a, b : QWord) : QWord; overload; inline;
function max(const a, b : QWord) : QWord; overload; inline;
function min(const a, b : int64) : int64; overload; inline;
function max(const a, b : int64) : int64; overload; inline;

implementation

function min(const a, b : QWord) : QWord; overload; inline;
begin
    if a<b then result := a else result := b;
end;

function max(const a, b : QWord) : QWord; overload; inline;
begin
    if a>b then result := a else result := b;
end;

function min(const a, b : int64) : int64; overload; inline;
begin
    if a<b then result := a else result := b;
end;

function max(const a, b : int64) : int64; overload; inline;
begin
    if a<b then result := a else result := b;
end;

function Normalize(const x : Int64) : word; inline;
begin
    if x <= 0 then Result := 0
    else if x >= 65536 then Result := 65535 
    else Result := x;
end;

function FpColor(red,green,blue,alpha : word) : TFPColor; inline;
begin
    Result.red := red;
    Result.green := green;
    Result.blue := blue;
    Result.alpha := alpha;
end;

function Normal(const {%H-}a, b : TFPColor) : TFPColor;
begin
    Result := b;
end;

function Dissolve(const a, b : TFPColor) : TFPColor; //need randomize!
begin
    if random(2)=0 then
        Result := b
    else
        Result := a;
end;

function Multiply(const a, b : TFPColor) : TFPColor;
begin
    Result.Alpha := b.Alpha;
    Result.red := (QWord(a.red)*QWord(b.red)) div 65535;
    Result.green := (QWord(a.green)*QWord(b.green)) div 65535;
    Result.blue := (QWord(a.blue)*QWord(b.blue)) div 65535;
end;

function Divide(const a, b : TFPColor) : TFPColor;
begin
    Result.Alpha := b.Alpha;
    Result.red := min(65535, QWord(a.red) shl 16 div (b.red+1));
    Result.green := min(65535, QWord(a.green) shl 16 div (b.green+1));
    Result.blue := min(65535, QWord(a.blue) shl 16 div (b.blue+1));
end;

function Screen(const a, b : TFPColor) : TFPColor;
begin
    Result.Alpha := b.Alpha;
    Result.red := 65535 - (65535-QWord(b.red))*(65535-QWord(a.red)) div 65535;
    Result.green := 65535 - (65535-QWord(b.green))*(65535-QWord(a.green)) div 65535;
    Result.blue := 65535 - (65535-QWord(b.blue))*(65535-QWord(a.blue)) div 65535;
end;

function Overlay(const a, b : TFPColor) : TFPColor;
begin
    Result.Alpha := b.Alpha;
    Result.red := QWord(a.red)*(QWord(a.red)+2*QWord(b.red)*(65535-a.red) div 65535) div 65535;
    Result.green := QWord(a.green)*(QWord(a.green)+2*QWord(b.green)*(65535-a.green) div 65535) div 65535;
    Result.blue := QWord(a.blue)*(QWord(a.blue)+2*QWord(b.blue)*(65535-a.blue) div 65535) div 65535;
end;

function Dodge(const a, b : TFPColor) : TFPColor;
begin
    Result.Alpha := b.Alpha;
    Result.red := Normalize(65536*QWord(a.red) div (65536-b.red));
    Result.green := Normalize(65536*QWord(a.green) div (65536-b.green));
    Result.blue := Normalize(65536*QWord(a.blue) div (65536-b.blue));
end;

function Burn(const a, b : TFPColor) : TFPColor;
begin
    Result.Alpha := b.Alpha;
    Result.red := Normalize(65535 - (65536*(65535-QWord(a.red))) div (QWord(b.red+1)));
    Result.green := Normalize(65535 - (65536*(65535-QWord(a.green))) div (QWord(b.green+1)));
    Result.blue := Normalize(65535 - (65536*(65535-QWord(a.blue))) div (QWord(b.blue+1)));
end;

function HardLight(const a, b : TFPColor) : TFPColor;
const
    Border = 65536 div 2;
begin
    Result.Alpha := b.Alpha;
    if (b.red + b.green + b.blue)>3*Border then
    begin 
        Result.red := 65535 - ((65535-2*max(0, int64(b.red)-Border)) * (65535-a.red)) div 65536;
        Result.green := 65535 - ((65535-2*max(0, int64(b.green)-Border)) * (65535-a.green)) div 65536;
        Result.blue := 65535 - ((65535-2*max(0, int64(b.blue)-Border)) * (65535-a.blue)) div 65536;
    end
    else
    begin
        Result.red := Normalize(2*QWord(a.red)*QWord(b.red) div 65536);
        Result.green := Normalize(2*QWord(a.green)*QWord(b.green) div 65536);
        Result.blue := Normalize(2*QWord(a.blue)*QWord(b.blue) div 65536);
    end;
end;

function SoftLight(const a, b : TFPColor) : TFPColor;
var
    sc : TFPColor;
begin
    Result.Alpha := b.Alpha;
    sc := Screen(a, b);
    Result.red := Normalize(QWord(a.red)*((65535 - QWord(a.red))*QWord(b.red) div 65535 + Word(sc.red)) div 65535);
    Result.green := Normalize(QWord(a.green)*((65535 - QWord(a.green))*QWord(b.green) div 65535 + QWord(sc.green)) div 65535);
    Result.blue := Normalize(QWord(a.blue)*((65535 - QWord(a.blue))*QWord(b.blue) div 65535 + QWord(sc.blue)) div 65535); 
end;

function GrainExtract(const a, b : TFPColor) : TFPColor;
const 
    Border = 65536 div 2;
begin
    Result.Alpha := b.Alpha;
    Result.red := Normalize(int64(a.red)-int64(b.red)+Border);
    Result.green := Normalize(int64(a.green)-int64(b.green)+Border);
    Result.blue := Normalize(int64(a.blue)-int64(b.blue)+Border);
end;

function GrainMerge(const a, b : TFPColor) : TFPColor;
const 
    Border = 65536 div 2;
begin
    Result.Alpha := b.Alpha;
    Result.red := Normalize(int64(a.red)+int64(b.red)-Border);
    Result.green := Normalize(int64(a.green)+int64(b.green)-Border);
    Result.blue := Normalize(int64(a.blue)+int64(b.blue)-Border);
end;

function Difference(const a, b : TFPColor) : TFPColor;
begin
    Result.Alpha := b.Alpha;
    Result.red := abs(int64(a.red)-int64(b.red));
    Result.green := abs(int64(a.green)-int64(b.green));
    Result.blue := abs(int64(a.blue)-int64(b.blue));
end;

function Addition(const a, b : TFPColor) : TFPColor;
begin
    Result.Alpha := b.Alpha;
    Result.red := Normalize(int64(a.red)+int64(b.red));
    Result.green := Normalize(int64(a.green)+int64(b.green));
    Result.blue := Normalize(int64(a.blue)+int64(b.blue));
end;

function Subtract(const a, b : TFPColor) : TFPColor;
begin
    Result.Alpha := b.Alpha;
    Result.red := Normalize(int64(a.red)-int64(b.red));
    Result.green := Normalize(int64(a.green)-int64(b.green));
    Result.blue := Normalize(int64(a.blue)-int64(b.blue));
end;

function DarkenOnly(const a, b : TFPColor) : TFPColor;
begin
    Result.Alpha := b.Alpha;
    Result.red := min(a.red, b.red);
    Result.green := min(a.green, b.green);
    Result.blue := min(a.blue, b.blue);
end;

function LightenOnly(const a, b : TFPColor) : TFPColor;
begin
    Result.Alpha := b.Alpha;
    Result.red := max(a.red, b.red);
    Result.green := max(a.green, b.green);
    Result.blue := max(a.blue, b.blue);
end;

function Hue(const a, b : TFPColor) : TFPColor;
var
    S, V, hR, hG, hB : Double;
begin
    Result.Alpha := b.Alpha;
    
    S := (abs(b.red-b.blue) + abs(b.red-b.green) + abs(b.green-b.blue))/3+1e-10;
    V := (b.red+b.green+b.blue)/3;
    hR := (b.red-V)/S;
    hG := (b.green-V)/S;
    hB := (b.blue-V)/S;

    S := (abs(a.red-a.blue) + abs(a.red-a.green) + abs(a.green-a.blue))/3+1e-10;
    V := (a.red+a.green+a.blue)/3;

    Result.red := Normalize(round(hR*S+V));
    Result.green := Normalize(round(hG*S+V));
    Result.blue := Normalize(round(hB*S+V));
end;

function Saturation(const a, b : TFPColor) : TFPColor;
var
    S, V, hR, hG, hB : Double;
begin
    Result.Alpha := b.Alpha;
    
    S := (abs(a.red-a.blue) + abs(a.red-a.green) + abs(a.green-a.blue))/3+1e-10;
    V := (a.red+a.green+a.blue)/3;
    hR := (a.red-V)/S;
    hG := (a.green-V)/S;
    hB := (a.blue-V)/S;

    S := (abs(b.red-b.blue) + abs(b.red-b.green) + abs(b.green-b.blue))/3+1e-10;
   
    Result.red := Normalize(round(hR*S+V));
    Result.green := Normalize(round(hG*S+V));
    Result.blue := Normalize(round(hB*S+V));
end;

function Color(const a, b : TFPColor) : TFPColor;
var
    S, V, hR, hG, hB : Double;
begin
    Result.Alpha := b.Alpha;
    
    S := (abs(b.red-b.blue) + abs(b.red-b.green) + abs(b.green-b.blue))/3+1e-10;
    V := (b.red+b.green+b.blue)/3;
    hR := (b.red-V)/S;
    hG := (b.green-V)/S;
    hB := (b.blue-V)/S;

    V := (a.red+a.green+a.blue)/3;
    
    Result.red := Normalize(round(hR*S+V));
    Result.green := Normalize(round(hG*S+V));
    Result.blue := Normalize(round(hB*S+V));
end;

function Value(const a, b : TFPColor) : TFPColor;
var
    S, V, hR, hG, hB : Double;
begin
    Result.Alpha := b.Alpha;
    
    S := (abs(a.red-a.blue) + abs(a.red-a.green) + abs(a.green-a.blue))/3+1e-10;
    V := (a.red+a.green+a.blue)/3;
    hR := (a.red-V)/S;
    hG := (a.green-V)/S;
    hB := (a.blue-V)/S;

    V := (b.red+b.green+b.blue)/3;

    Result.red := Normalize(round(hR*S+V));
    Result.green := Normalize(round(hG*S+V));
    Result.blue := Normalize(round(hB*S+V));
end;

function GetDrawFunction(const mode : TDrawMode) : TDrawFunction;
begin
    case mode of
        dmNormal : Result := @Normal;
        dmDissolve : Result := @Dissolve;
        dmMultiply : Result := @Multiply;
        dmDivide : Result := @Divide;
        dmScreen : Result := @Screen;
        dmOverlay : Result := @Overlay;
        dmDodge : Result := @Dodge;
        dmBurn : Result := @Burn;
        dmHardLight : Result := @HardLight;
        dmSoftLight : Result := @SoftLight;
        dmGrainExtract : Result := @GrainExtract;
        dmGrainMerge : Result := @GrainMerge;
        dmDifference : Result := @Difference;
        dmAddition : Result := @Addition;
        dmSubtract : Result := @Subtract;
        dmDarkenOnly : Result := @DarkenOnly;
        dmLightenOnly : Result := @LightenOnly;
        dmHue : Result := @Hue;
        dmSaturation : Result := @Saturation;
        dmColor: Result := @Color;
        dmValue : Result := @Value;
        else Result := @Normal;
    end;
end;

end.
    
