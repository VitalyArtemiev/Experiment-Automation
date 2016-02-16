{ Function Generator and Lock-In Amplifier Control

  Copyright (C) 2015 Vitaly Artemiev vitaly.artemiev@yandex.ru

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

program GenCtrl;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, {tachartlazaruspkg,} MainF, memof, stepf, optionf, ReadingsF,
  CustomCommandF, StatusF, GenConst, DetConst, serconf, AxisSource, AboutF;

{$R *.res}

begin
  Application.Title:= 'Generator Control';
  RequireDerivedFormResource:= True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TMemoForm, MemoForm);
  Application.CreateForm(TStepForm, StepForm);
  Application.CreateForm(TOptionForm, OptionForm);
  Application.CreateForm(TReadingsForm, ReadingsForm);
  Application.CreateForm(TCustomCommandForm, CustomCommandForm);

  Application.MainFormOnTaskBar:= true;
  Application.CreateForm(TStatusForm, StatusForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.Run;
end.

