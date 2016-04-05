unit CustomCommandF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ActnList, Spin, synaser;

type

  { TCustomCommandForm }

  TCustomCommandForm = class(TForm)
    btSend: TButton;
    btAdd: TButton;
    btClear: TButton;
    cbCommands: TComboBox;
    cbQuery: TCheckBox;
    cbInt1: TCheckBox;
    cbInt2: TCheckBox;
    cbFloat: TCheckBox;
    cbTerminator: TComboBox;
    cbAwaitResponse: TCheckBox;
    eInt2: TSpinEdit;
    eFloat: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    mCustomCommand: TMemo;
    eInt1: TSpinEdit;
    eTimeout: TSpinEdit;



    procedure btAddClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure btSendClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure GetCommands;
    { private declarations }
  public
    Form: pointer;
    { public declarations }
  end;

var
  CustomCommandForm: TCustomCommandForm;
  Command: shortstring;

implementation

uses MainF, SerConF;

{$R *.lfm}

{ TCustomCommandForm }

procedure TCustomCommandForm.btSendClick(Sender: TObject);
var
  i: longint;
  s: ansistring;
begin

  with TSerConnectForm(Form) do
    if SerPort.InstanceActive then
    begin
      SerPort.RaiseExcept:= false;
      for i:= 0 to mCustomCommand.Lines.Count - 1 do
        Command+= mCustomCommand.Lines[i];
      case cbTerminator.ItemIndex of
        0: SerPort.SendString(Command  + CR);
        1: SerPort.SendString(Command  + LF);
        2: SerPort.SendString(Command  + CRLF);
      end;
      Command:= '';
      if cbAwaitResponse.Checked then
      begin
        s:= SerPort.Recvstring(eTimeOut.Value);
        ShowMessage(s);
      end;
      SerPort.RaiseExcept:= true;
    end;
end;

procedure TCustomCommandForm.FormCreate(Sender: TObject);
begin

end;

procedure TCustomCommandForm.btAddClick(Sender: TObject);
var
  Params, s: string;
begin
  Params:= '';

  if cbQuery.Checked then Params:= '?';

  str(eInt1.Value, s);
  if cbInt1.Checked then Params+= ' ' + s;    //;!!

  str(eInt2.Value, s);
  if cbInt2.Checked then Params+= ' ' + s;

  str(eFloat.Value, s);
  if cbFloat.Checked then Params+= ' ' + s;
  if mCustomCommand.Lines.Count > 0 then
    mCustomCommand.Lines.Append(';');
    mCustomCommand.Lines.AddText(cbCommands.Text + Params);
end;

procedure TCustomCommandForm.btClearClick(Sender: TObject);
begin
  mCustomCommand.Clear;
end;

procedure TCustomCommandForm.FormShow(Sender: TObject);
begin
  Label2.Caption:= TSerConnectForm(Form).CurrentDevice^.Model;
  if Label2.Caption = '' then Label2.Caption:= 'Устройство не опознано';
  GetCommands;
end;

procedure TCustomCommandForm.GetCommands;
var
  i: longint;
begin
  cbCommands.Clear;
  with TSerConnectForm(Form)do
  begin
    i:= DeviceIndex;
    cbCommands.Items.AddStrings(SupportedDevices[i].Commands);
  end;
  if cbCommands.Items.Count > 0 then
    while cbCommands.Items[0] = '' do cbCommands.Items.Delete(0);
end;

end.

