program SortTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, SortTestUnit;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

