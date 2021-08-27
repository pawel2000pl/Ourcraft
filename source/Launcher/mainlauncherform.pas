unit MainLauncherForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LoginFrame, OurAccounts;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure Frame1_1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  Account : TUserAccount;
begin
  Randomize;
  Account := TUserAccount.Create;
  Account.GeneratePassword;
  Account.Free;
end;

procedure TForm1.Frame1_1Click(Sender: TObject);
begin

end;

end.

