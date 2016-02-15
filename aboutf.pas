unit AboutF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLintf;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    btCopyEmail: TButton;
    btOpenGitHub: TButton;
    btReportIssue: TButton;
    Label1: TLabel;
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

end.

