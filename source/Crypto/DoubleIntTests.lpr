program DoubleIntTests;

{$mode objfpc}{$H+}

uses
  HeapTrc, cThreads, Interfaces, Forms, GuiTestRunner, DoubleIntTestUnit1;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

