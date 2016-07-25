unit AboutF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLintf, Clipbrd, ExtCtrls
  // FPC trunk fileinfo reads exe resources as long as you register the appropriate units
  , fileinfo
  , winpeimagereader {need this for reading exe info}
  , elfreader {needed for reading ELF executables}
  , machoreader {needed for reading MACH-O executables}
  ;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    btCopyEmail: TButton;
    btOpenGitHub: TButton;
    btReportIssue: TButton;
    InfoPanel: TPanel;
    lCompany: TLabel;
    lContactInfo: TLabel;
    lCopyRight: TLabel;
    lProgramVersion: TLabel;
    procedure btCopyEmailClick(Sender: TObject);
    procedure btOpenGitHubClick(Sender: TObject);
    procedure btReportIssueClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

uses
  BaseConF;

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.btReportIssueClick(Sender: TObject);
begin
  OpenURL('https://github.com/VitalyArtemiev/Experiment-Automation/issues');
end;

procedure TAboutForm.FormCreate(Sender: TObject);
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo:= TFileVersionInfo.Create(nil);
  try
    {$IFOPT D+}
    FileVerInfo.FileName:= 'EADebug.exe';
    {$ELSE}
    FileVerInfo.FileName:= 'Experiment-Automation.exe';
    {$ENDIF}
    try
      FileVerInfo.ReadFileInfo;
    except
      on E: Exception do
        WriteProgramLog(E.Message);
    end;
    lProgramVersion.Caption:= 'Версия программы:  ' +  FileVerInfo.VersionStrings.Values['FileVersion'];
    lCompany.Caption:= LineEnding + FileVerInfo.VersionStrings.Values['CompanyName'];
    lCopyright.Caption:= LineEnding + FileVerInfo.VersionStrings.Values['LegalCopyright'];
  finally
    FileVerInfo.Free;
  end;
end;

procedure TAboutForm.btOpenGitHubClick(Sender: TObject);
begin
  OpenURL('https://github.com/VitalyArtemiev/Experiment-Automation');
end;

procedure TAboutForm.btCopyEmailClick(Sender: TObject);
begin
  ClipBoard.AsText:= 'vitaly.artemiev@yandex.ru';
end;

end.

