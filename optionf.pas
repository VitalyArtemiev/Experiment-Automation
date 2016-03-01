unit OptionF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Spin;

type

  { TOptionForm }

  TOptionForm = class(TForm)
    btSave: TButton;
    btDefaults: TButton;
    btConfDir: TButton;
    btCancel: TButton;
    btDeviceList: TButton;
    cbHandshake1: TComboBox;
    cbParity1: TComboBox;
    cgFiles: TCheckGroup;
    cbParity: TComboBox;
    cbHandshake: TComboBox;
    cgReadings: TCheckGroup;
    eBaudRate: TSpinEdit;
    eBaudRate1: TSpinEdit;
    eDevice: TComboBox;
    eDevice1: TComboBox;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    eReadingTime: TSpinEdit;
    Label7: TLabel;
    DevicePage: TPageControl;
    Label8: TLabel;
    Label9: TLabel;
    TabControl: TPageControl;
    rgOnConnect: TRadioGroup;
    GeneralTab: TTabSheet;
    ConnectionTab: TTabSheet;
    GenTab: TTabSheet;
    DetTab: TTabSheet;
    procedure btCancelClick(Sender: TObject);
    procedure btConfDirClick(Sender: TObject);
    procedure btDefaultsClick(Sender: TObject);
    procedure btDeviceListClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure eDevice1Change(Sender: TObject);
    procedure eDeviceChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
  private
    Save: boolean;
    { private declarations }
  public
    procedure GetOptions;
    procedure SaveOptions;
    { public declarations }
  end;

var
  OptionForm: TOptionForm;
  NewDefaultParams: ansistring;

implementation

uses MainF, ReadingsF, SerConF, DeviceF;

{$R *.lfm}

{ TOptionForm }
{function NewPar(P: TParity): PParity;
begin
  New(NewPar);
  NewPar^:= P;
end;}

procedure TOptionForm.btSaveClick(Sender: TObject);
begin
  Save:= true;
  OptionForm.Close;
end;

procedure TOptionForm.eDeviceChange(Sender: TObject);
var
  s: string;
begin
  with DeviceForm.sgGenCommands do
  begin
    eBaudRate.Value:= valf(Cells[eDevice.ItemIndex + 1, longint(hBaudRate)]);

    s:= Cells[eDevice.ItemIndex + 1, longint(hParity)];
    cbParity.ItemIndex:= DeviceForm.cbParity.Items.IndexOf(s);

    s:= Cells[eDevice.ItemIndex + 1, longint(hHandShake)];
    cbHandShake.ItemIndex:= DeviceForm.cbHandShake.Items.IndexOf(s);
  end;
end;

procedure TOptionForm.eDevice1Change(Sender: TObject);
var
  s: string;
begin
  with DeviceForm.sgDetCommands do
  begin
    eBaudRate1.Value:= valf(Cells[eDevice1.ItemIndex + 1, longint(hBaudRate)]);

    s:= Cells[eDevice1.ItemIndex + 1, longint(hParity)];
    cbParity1.ItemIndex:= DeviceForm.cbParity.Items.IndexOf(s);

    s:= Cells[eDevice1.ItemIndex + 1, longint(hHandShake)];
    cbHandShake1.ItemIndex:= DeviceForm.cbHandShake.Items.IndexOf(s);
  end;
end;

procedure TOptionForm.btConfDirClick(Sender: TObject);
begin
  MainForm.OpenDialog.FileName:= Config.DefaultParams;
  MainForm.OpenDialog.Title:= 'Выбор файла конфигурации';
  MainForm.OpenDialog.Filter:= 'Все файлы|*.*|Файлы конфигурации|*.cfg';
  MainForm.OpenDialog.Execute;
  NewDefaultParams:= UTF8toANSI(MainForm.OpenDialog.FileName);
  MainForm.OpenDialog.FileName:= '';
end;

procedure TOptionForm.btDefaultsClick(Sender: TObject);
begin
  with MainForm do
  begin
    LoadConfig(DefaultConfig);
    LoadParams(DefaultParams);
  end;
  GetOptions;
end;

procedure TOptionForm.btDeviceListClick(Sender: TObject);
begin
  DeviceForm.ShowModal;
end;

procedure TOptionForm.btCancelClick(Sender: TObject);
begin
  OptionForm.Close;
end;

procedure TOptionForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Save then SaveOptions;
  OptionForm.Close;
end;

procedure TOptionForm.FormShow(Sender: TObject);
begin
  Save:= false;
  GetOptions;
end;

procedure TOptionForm.GetOptions;
var
  i: integer;
  PreviousDevice: string;
begin
  with Config do
  begin
    cgFiles.Checked[0]:= LoadParamsOnStart;
    cgFiles.Checked[1]:= SaveParamsOnExit;
    cgFiles.Checked[2]:= AutoExportParams;
    cgFiles.Checked[3]:= AutoComment;
    cgFiles.Checked[4]:= KeepLog;
    cgReadings.Checked[0]:= AutoReadingConst;
    cgReadings.Checked[1]:= AutoReadingSweep;
    cgReadings.Checked[2]:= AutoReadingStep;
    eReadingTime.Value:= Params.ReadingTime;

    {showmessage(DefaultParams);}                 //!!!!!!!!!!1
    NewDefaultParams:= DefaultParams;
  end;

  with DeviceForm do
  begin
    eDevice.Clear;
    for i:= 1 to sgGenCommands.ColCount - 1 do
      eDevice.AddItem(sgGenCommands.Cells[i, 0], nil);

    eDevice1.Clear;
    for i:= 1 to sgDetCommands.ColCount - 1 do
      eDevice1.AddItem(sgDetCommands.Cells[i, 0], nil);
  end;

  with MainForm, Mainform.ConnectParams, eDevice do
  begin
    if CurrentDevice <> '' then
      ItemIndex:= Items.IndexOf(CurrentDevice)          //!!!???
    else
      ItemIndex:= Items.IndexOf(PresumedDevice);
    if ItemIndex < 0 then ItemIndex:= 0;

    eBaudRate.Value:= BaudRate;
    cbParity.ItemIndex:= Parity;
    if not (SoftFlow or HardFlow) then cbHandShake.ItemIndex:= 0
    else
    if SoftFlow and HardFlow then cbHandShake.ItemIndex:= 3
    else
    if SoftFlow then cbHandShake.ItemIndex:= 1
    else cbHandShake.ItemIndex:= 2;
    //eInitCommand.Text:= InitString;
  end;
  with ReadingsForm, ReadingsForm.ConnectParams, eDevice1 do
  begin
    if CurrentDevice <> '' then
      ItemIndex:= Items.IndexOf(CurrentDevice)          //!!!???
    else
      ItemIndex:= Items.IndexOf(PresumedDevice);
    if ItemIndex < 0 then ItemIndex:= 0;

    eBaudRate1.Value:= BaudRate;
    cbParity1.ItemIndex:= Parity;
    if not (SoftFlow or HardFlow) then cbHandShake1.ItemIndex:= 0
    else
    if SoftFlow and HardFlow then cbHandShake1.ItemIndex:= 3
    else
    if SoftFlow then cbHandShake1.ItemIndex:= 1
    else cbHandShake1.ItemIndex:= 2;
  end;

  eDeviceChange(Self);
  eDevice1Change(Self);
end;

procedure TOptionForm.SaveOptions;
var
  s: string;
begin
  with Config do
  begin
    LoadParamsOnStart:= cgFiles.Checked[0];
    SaveParamsOnExit:=  cgFiles.Checked[1];
    AutoExportParams:=  cgFiles.Checked[2];
    AutoComment:=       cgFiles.Checked[3];
    KeepLog:=           cgFiles.Checked[4];
    AutoReadingConst:= cgReadings.Checked[0];
    AutoReadingSweep:= cgReadings.Checked[1];
    AutoReadingStep:=  cgReadings.Checked[2];
    Params.ReadingTime:= eReadingTime.Value;

    DefaultParams:= NewDefaultParams;
    OnConnect:= ConnectAction(rgOnConnect.ItemIndex);
  end;

  with MainForm, Mainform.ConnectParams, DeviceForm.sgGenCommands do
  begin
    PresumedDevice:= eDevice.Text;
    BaudRate:= eBaudRate.Value;
    Databits:= valf(Cells[eDevice.ItemIndex , longint(hDataBits)]);

    s:= Cells[eDevice.ItemIndex + 1, longint(hStopBits)];
    StopBits:= DeviceForm.cbStopBits.Items.IndexOf(s);

    Parity:= cbParity.ItemIndex;
    Timeout:= seRecvTimeOut.Value;
    case cbHandShake.ItemIndex of
      0: begin
           SoftFlow:= false;
           HardFlow:= false;
         end;
      1: begin
           SoftFlow:= true;
           HardFlow:= false;
         end;
      2: begin
           SoftFlow:= false;
           HardFlow:= true;
         end;
      3: begin
           SoftFlow:= true;
           HardFlow:= true;
         end;
    end;
    //InitString:= eInitCommand.Text;
  end;

  with ReadingsForm, ReadingsForm.ConnectParams, DeviceForm.sgDetCommands do
  begin
    PresumedDevice:= eDevice1.Text;
    BaudRate:= eBaudRate.Value;
    Databits:= valf(Cells[eDevice.ItemIndex + 1, longint(hDataBits)]);

    s:= Cells[eDevice.ItemIndex + 1, longint(hStopBits)];
    StopBits:= DeviceForm.cbStopBits.Items.IndexOf(s);

    Parity:= cbParity1.ItemIndex;
    Timeout:= seRecvTimeOut.Value;
    case cbHandShake1.ItemIndex of
      0: begin
           SoftFlow:= false;
           HardFlow:= false;
         end;
      1: begin
           SoftFlow:= true;
           HardFlow:= false;
         end;
      2: begin
           SoftFlow:= false;
           HardFlow:= true;
         end;
      3: begin
           SoftFlow:= true;
           HardFlow:= true;
         end;
    end;
   // InitString:= eInitCommand1.Text;
  end;
end;

end.

