unit AboutF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLintf, Clipbrd;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    btCopyEmail: TButton;
    btOpenGitHub: TButton;
    btReportIssue: TButton;
    Label1: TLabel;
    procedure btCopyEmailClick(Sender: TObject);
    procedure btOpenGitHubClick(Sender: TObject);
    procedure btReportIssueClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.btReportIssueClick(Sender: TObject);
begin
  OpenURL('https://github.com/VitalyArtemiev/Generator-Amplifier-Control/issues');
end;

procedure TAboutForm.btOpenGitHubClick(Sender: TObject);
begin
  OpenURL('https://github.com/VitalyArtemiev/Generator-Amplifier-Control');
end;

procedure TAboutForm.btCopyEmailClick(Sender: TObject);
begin
  ClipBoard.AsText:= 'vitaly.artemiev@yandex.ru';
end;

  { TODO 1 -cFeature : Program version
http://wiki.freepascal.org/Show_Application_Title,_Version,_and_Company }
end.

