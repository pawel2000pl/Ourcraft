unit MainOurcraftDirectory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure SetToMainOurcraftDirectory;

implementation

procedure SetToMainOurcraftDirectory;
const
  CheckFile = '.OURCRAFT_MAIN_DIRECTORY_DO_NOT_DELETE_THIS_FILE';
begin
  while (GetCurrentDir <> '/') and (not fileexists(CheckFile)) do
    SetCurrentDir('..');
end;

initialization
  SetToMainOurcraftDirectory;

end.

