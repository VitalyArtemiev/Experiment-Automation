program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, {tachartlazaruspkg,} MainF, memof, stepf, optionf, ReadingsF,
  CustomCommandF, StatusF, GenConst, DetConst, serconf, AxisSource;

{$R *.res}

begin
  Application.Title:='Generator Control';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TMemoForm, MemoForm);
  Application.CreateForm(TStepForm, StepForm);
  Application.CreateForm(TOptionForm, OptionForm);
  Application.CreateForm(TReadingsForm, ReadingsForm);
  Application.CreateForm(TCustomCommandForm, CustomCommandForm);

  Application.MainFormOnTaskBar:= true;
  Application.CreateForm(TStatusForm, StatusForm);
  Application.Run;
end.

