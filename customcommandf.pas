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
    Label5: TLabel;
    mCustomCommand: TMemo;
    eInt1: TSpinEdit;
    eTimeout: TSpinEdit;

    procedure btAddClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure btSendClick(Sender: TObject);
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

uses MainF, BaseConF;

{$R *.lfm}

{ TCustomCommandForm }

procedure TCustomCommandForm.btSendClick(Sender: TObject);
var
  i, t: longint;
  s: ansistring;
begin
  with tConnectionForm(Form) do
  begin
    for i:= 0 to mCustomCommand.Lines.Count - 1 do
      Command+= mCustomCommand.Lines[i];

    case cbTerminator.ItemIndex of
      0: Command+= CR;
      1: Command+= LF;
      2: Command+= CRLF;
    end;

    case ConnectionKind of
      //cNone: ;
      cSerial:
        if SerPort.InstanceActive then
        begin
          EnterCriticalSection(CommCS);
            SerPort.RaiseExcept:= false;

            SerPort.SendString(Command);

              if cbAwaitResponse.Checked then
              begin
                s:= SerPort.Recvstring(eTimeOut.Value);
                ShowMessage(s);
              end;

            SerPort.RaiseExcept:= true;
          LeaveCriticalSection(CommCS);
        end;
      cUSB: ;
      cTelNet:
      begin
        EnterCriticalSection(CommCS);
          TelNetClient.Send(Command);

          if cbAwaitResponse.Checked then
          begin
            t:= TelNetClient.Timeout;
            TelNetClient.Timeout:= eTimeOut.Value;
            s:= TelNetClient.RecvTerminated(CurrentDevice^.Terminator);
            ShowMessage(s);
            TelNetClient.Timeout:= t;
          end;
        LeaveCriticalSection(CommCS);
      end;
      cVXI: ;
    end;

    Command:= '';
  end;
end;

procedure TCustomCommandForm.btAddClick(Sender: TObject);
var
  Params, s: string;
begin
  Params:= '';

  if cbQuery.Checked then
    Params:= '?';

  str(eInt1.Value, s);
  if cbInt1.Checked then
    Params+= ' ' + s;    //;!!

  str(eInt2.Value, s);
  if cbInt2.Checked then
    Params+= ' ' + s;

  str(eFloat.Value, s);
  if cbFloat.Checked then
    Params+= ' ' + s;
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
  with tConnectionForm(Form) do
  begin
    Self.Top:= Top;
    Self.Left:= Left;

    Label2.Caption:= CurrentDevice^.Manufacturer + ' ' + CurrentDevice^.Model;
  end;
  if Label2.Caption = '' then
    Label2.Caption:= 'Устройство не опознано';
  GetCommands;
end;

procedure TCustomCommandForm.GetCommands;
begin
  cbCommands.Clear;
  with tConnectionForm(Form).CurrentDevice^ do
  begin
    cbCommands.Items.AddStrings(Commands);
    case Terminator of
      CR:   cbTerminator.ItemIndex:= cbTerminator.Items.IndexOf('CR');
      LF:   cbTerminator.ItemIndex:= cbTerminator.Items.IndexOf('LF');
      CRLF: cbTerminator.ItemIndex:= cbTerminator.Items.IndexOf('CRLF');
    end;
    eTimeOut.Value:= TimeOut;
  end;

  if cbCommands.Items.Count > 0 then
    while cbCommands.Items[0] = '' do
      cbCommands.Items.Delete(0);
end;

end.

