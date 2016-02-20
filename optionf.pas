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
    cbStopBits1: TComboBox;
    cgFiles: TCheckGroup;
    cbParity: TComboBox;
    cbHandshake: TComboBox;
    cgReadings: TCheckGroup;
    eBaudRate: TSpinEdit;
    eBaudRate1: TSpinEdit;
    eDataBits1: TSpinEdit;
    eDeviceName: TComboBox;
    eDeviceName1: TEdit;
    eInitCommand1: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label15: TLabel;
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
  DeviceForm.Show;
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
  with MainForm, Mainform.ConnectParams do
  begin
    if CurrentDevice <> '' then
      eDeviceName.Text:= CurrentDevice          //!!!???
    else
      eDeviceName.Text:= PresumedDevice;
    eBaudRate.Value:= BaudRate;
    //eDataBits.Value:= Databits;
    //cbStopBits.ItemIndex:= StopBits;
    cbParity.ItemIndex:= Parity;
    if not (SoftFlow or HardFlow) then cbHandShake.ItemIndex:= 0
    else
    if SoftFlow and HardFlow then cbHandShake.ItemIndex:= 3
    else
    if SoftFlow then cbHandShake.ItemIndex:= 1
    else cbHandShake.ItemIndex:= 2;
    //eInitCommand.Text:= InitString;
  end;
  with ReadingsForm, ReadingsForm.ConnectParams do
  begin
    if CurrentDevice <> '' then
      eDeviceName1.Text:= CurrentDevice
    else
      eDeviceName1.Text:= PresumedDevice;
    eBaudRate1.Value:= BaudRate;
    eDataBits1.Value:= Databits;
    cbStopBits1.ItemIndex:= StopBits;
    cbParity1.ItemIndex:= Parity;
    if not (SoftFlow or HardFlow) then cbHandShake1.ItemIndex:= 0
    else
    if SoftFlow and HardFlow then cbHandShake1.ItemIndex:= 3
    else
    if SoftFlow then cbHandShake1.ItemIndex:= 1
    else cbHandShake1.ItemIndex:= 2;
    eInitCommand1.Text:= InitString;
  end;
end;

procedure TOptionForm.SaveOptions;
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

  with MainForm, Mainform.ConnectParams do
  begin
    PresumedDevice:= eDeviceName.Text;
    BaudRate:= eBaudRate.Value;
    //Databits:= eDataBits.Value;
    //StopBits:= cbStopBits.ItemIndex;
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

  with ReadingsForm, ReadingsForm.ConnectParams do
  begin
    PresumedDevice:= eDeviceName1.Text;
    BaudRate:= eBaudRate1.Value;
    Databits:= eDataBits1.Value;
    StopBits:= cbStopBits1.ItemIndex;
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
    InitString:= eInitCommand1.Text;
  end;
end;

end.

