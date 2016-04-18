unit AboutF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLintf, Clipbrd
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
    Label1: TLabel;
    lProgramInfo: TLabel;
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

var
  FileVerInfo: TFileVersionInfo;
{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.btReportIssueClick(Sender: TObject);
begin
  OpenURL('https://github.com/VitalyArtemiev/Generator-Amplifier-Control/issues');
end;

procedure TAboutForm.FormCreate(Sender: TObject);   { TODO 1 -cBug : Program version
http://wiki.freepascal.org/Show_Application_Title,_Version,_and_Company }
begin
  if Paramcount=0 then
  begin
    writeln('Missing executable filename parameters. Aborting.');
    halt(1);
  end;
  FileVerInfo:= TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName:= paramstr(0);
    FileVerInfo.ReadFileInfo;
    {writeln('Company: ',FileVerInfo.VersionStrings.Values['CompanyName']);
    writeln('File description: ',FileVerInfo.VersionStrings.Values['FileDescription']); }
    lProgramInfo.Caption:= 'Версия программы: ' +  FileVerInfo.VersionStrings.Values['FileVersion'];
    {writeln('Internal name: ',FileVerInfo.VersionStrings.Values['InternalName']);
    writeln('Legal copyright: ',FileVerInfo.VersionStrings.Values['LegalCopyright']);
    writeln('Original filename: ',FileVerInfo.VersionStrings.Values['OriginalFilename']);
    writeln('Product name: ',FileVerInfo.VersionStrings.Values['ProductName']);
    writeln('Product version: ',FileVerInfo.VersionStrings.Values['ProductVersion']); }
  finally
    FileVerInfo.Free;
  end;
end;

procedure TAboutForm.btOpenGitHubClick(Sender: TObject);
begin
  OpenURL('https://github.com/VitalyArtemiev/Generator-Amplifier-Control');
end;

procedure TAboutForm.btCopyEmailClick(Sender: TObject);
begin
  ClipBoard.AsText:= 'vitaly.artemiev@yandex.ru';
end;

end.

