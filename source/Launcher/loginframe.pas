unit LoginFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, MaskEdit, ExtCtrls, Buttons;

type

  { TLoginFrm }

  TLoginFrm = class(TFrame)
    AccountCB: TComboBox;
    ProfileImage: TImage;
    PasswordEdit: TEdit;
    LoginBox: TGroupBox;
    LoginBtn: TSpeedButton;
    NewAccountBtn: TSpeedButton;
    AddUserBtn: TSpeedButton;
    procedure LoginBoxClick(Sender: TObject);
    procedure LoginBtnClick(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

{ TLoginFrm }

procedure TLoginFrm.LoginBtnClick(Sender: TObject);
begin

end;

procedure TLoginFrm.LoginBoxClick(Sender: TObject);
begin

end;

end.

