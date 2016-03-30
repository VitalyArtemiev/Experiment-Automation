unit MemoF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TMemoForm }

  TMemoForm = class(TForm)
    btSave: TButton;
    mComment: TMemo;
    procedure btSaveClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
  private
    Save: boolean;
    { private declarations }
  public
    { public declarations }
  end;

var
  MemoForm: TMemoForm;

implementation

{$R *.lfm}

{ TMemoForm }

procedure TMemoForm.btSaveClick(Sender: TObject);
begin
  Save:= true;
  MemoForm.Close;
end;

procedure TMemoForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if not Save then mComment.Lines.Clear;
end;

procedure TMemoForm.FormShow(Sender: TObject);
begin
  Save:= false;
end;

end.

