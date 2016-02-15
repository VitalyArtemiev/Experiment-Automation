program StepF;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='StepF';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TStepForm, StepForm);
  Application.Run;
end.

