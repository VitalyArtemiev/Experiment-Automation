{ Function Generator and Lock-In Amplifier Control

  Copyright (C) 2015 Vitaly Artemiev vitaly.artemiev@yandex.ru

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

program GenCtrl;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Windows, Forms, menuh, MainF, memof, stepf, optionf,
DetControlF, CustomCommandF, StatusF,
  serconf, AboutF, DeviceF, dialogs, lazcontrols,
  OffsetF, TempControlF{, FileM};

{$R *.res}

var
  Ex: integer;

begin
  Application.Title:= 'Experiment Automation';
  RequireDerivedFormResource:= True;
  Application.Initialize;
  Ex:= GetWindowLong(FindWindow(nil, 'Experiment Automation'), GWL_EXSTYLE);
  SetWindowLong(FindWindow(nil, 'Experiment Automation'), GWL_EXSTYLE, Ex or WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW);

  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDeviceForm, DeviceForm);
  Application.CreateForm(TDetControlForm, DetControlForm);
  Application.CreateForm(TMemoForm, MemoForm);
  Application.CreateForm(TStepForm, StepForm);
  Application.CreateForm(TOptionForm, OptionForm);
  Application.CreateForm(TCustomCommandForm, CustomCommandForm);
  Application.CreateForm(TStatusForm, StatusForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TOffsetForm, OffsetForm);
  Application.CreateForm(TTempControlForm, TempControlForm);
  Application.Run;
end.

