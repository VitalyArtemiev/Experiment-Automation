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
    eIPAdress: TEdit;
    eIPAdress1: TEdit;
    ePort: TEdit;
    ePort1: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
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
    procedure ReloadDeviceList;
    function SaveOptions: integer;
    { public declarations }
  end;

var
  OptionForm: TOptionForm;
  NewDefaultParams: ansistring;

implementation

uses SynaIP, MainF, ReadingsF, SerConF, DeviceF;

{$R *.lfm}

{ TOptionForm }

procedure TOptionForm.btSaveClick(Sender: TObject);
begin
  Save:= true;
  if CloseQuery then
    Close;
end;

procedure TOptionForm.eDeviceChange(Sender: TObject);
var
  s: string;
begin
  with DeviceForm.sgGenCommands do
  begin
    case MainForm.SupportedDevices[eDevice.ItemIndex + 1].Connection of
      //cNone: ;
      cSerial:
        begin
          eBaudrate.Enabled:= true;
          cbParity.Enabled:= true;
          cbHandshake.Enabled:= true;
          eIPAdress.Enabled:= false;
          ePort.Enabled:= false;

          eBaudRate.Value:= valf(Cells[eDevice.ItemIndex + 1, longint(hBaudRate)]);

          s:= Cells[eDevice.ItemIndex + 1, longint(hParity)];
          cbParity.ItemIndex:= DeviceForm.cbParity.Items.IndexOf(s);

          s:= Cells[eDevice.ItemIndex + 1, longint(hHandShake)];
          cbHandShake.ItemIndex:= DeviceForm.cbHandShake.Items.IndexOf(s);
        end;
      cTelNet:
        begin
          eBaudrate.Enabled:= false;
          cbParity.Enabled:= false;
          cbHandshake.Enabled:= false;
          eIPAdress.Enabled:= true;
          ePort.Enabled:= true;
        end;
    end;
  end;
end;

procedure TOptionForm.eDevice1Change(Sender: TObject);
var
  s: string;
begin
  with DeviceForm.sgDetCommands do
  begin
    case ReadingsForm.SupportedDevices[eDevice1.ItemIndex + 1].Connection of
      //cNone: ;
      cSerial:
        begin
          eBaudrate1.Enabled:= true;
          cbParity1.Enabled:= true;
          cbHandshake1.Enabled:= true;
          eIPAdress1.Enabled:= false;
          ePort1.Enabled:= false;

          eBaudRate1.Value:= valf(Cells[eDevice1.ItemIndex + 1, longint(hBaudRate)]);

          s:= Cells[eDevice1.ItemIndex + 1, longint(hParity)];
          cbParity1.ItemIndex:= DeviceForm.cbParity.Items.IndexOf(s);

          s:= Cells[eDevice1.ItemIndex + 1, longint(hHandShake)];
          cbHandShake1.ItemIndex:= DeviceForm.cbHandShake.Items.IndexOf(s);
        end;
      cTelNet:
        begin
          eBaudrate1.Enabled:= false;
          cbParity1.Enabled:= false;
          cbHandshake1.Enabled:= false;
          eIPAdress1.Enabled:= true;
          ePort1.Enabled:= true;
        end;
    end;


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
  ShowMessage('Загружено:' + LineEnding + DefaultConfig
              + LineEnding + DefaultParams);
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
var
  o: integer;
begin
  if Save then
  begin
    o:= SaveOptions;
    if o = 0 then
      CanClose:= true
    else
    begin
      CanClose:= false;
    end;
  end;
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

    NewDefaultParams:= DefaultParams;
  end;

  ReloadDeviceList;

  with MainForm, MainForm.CurrentDevice^, eDevice do
  begin
    if CurrentDevice^.Model <> '' then
      ItemIndex:= Items.IndexOf(CurrentDevice^.Model)
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
  end;

  with ReadingsForm, ReadingsForm.CurrentDevice^, eDevice1 do
  begin
    if CurrentDevice^.Model <> '' then
      ItemIndex:= Items.IndexOf(CurrentDevice^.Model)
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

procedure TOptionForm.ReloadDeviceList;
var
  i: integer;
begin
  with DeviceForm do
  begin
    eDevice.Clear;
    for i:= 1 to sgGenCommands.ColCount - 1 do
      eDevice.AddItem(sgGenCommands.Cells[i, 0], nil);

    eDevice1.Clear;
    for i:= 1 to sgDetCommands.ColCount - 1 do
      eDevice1.AddItem(sgDetCommands.Cells[i, 0], nil);
  end;

  if MainForm.ConnectionKind <> cNone then
    eDevice.ItemIndex:= eDevice.Items.IndexOf(MainForm.CurrentDevice^.Model);

  if ReadingsForm.ConnectionKind <> cNone then
    eDevice1.ItemIndex:= eDevice1.Items.IndexOf(ReadingsForm.CurrentDevice^.Model);

  eDeviceChange(Self);
  eDevice1Change(Self);
end;

function TOptionForm.SaveOptions: integer;
var
  s: string;
begin
  Result:= 0;

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

  with MainForm, Mainform.SupportedDevices[eDevice.ItemIndex + 1], DeviceForm.sgGenCommands do
  begin
    PresumedDevice:= eDevice.Text;
    Timeout:= seRecvTimeOut.Value;
    case Connection of
      cSerial:
        begin
          BaudRate:= eBaudRate.Value;
          Databits:= valf(Cells[eDevice.ItemIndex + 1, longint(hDataBits)]);

          s:= Cells[eDevice.ItemIndex + 1, longint(hStopBits)];
          StopBits:= DeviceForm.cbStopBits.Items.IndexOf(s);

          Parity:= cbParity.ItemIndex;

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
        end;
      cTelNet:
        begin
          if IsIP(eIPAdress.Text) then
            Host:= eIPAdress.Text
          else
          begin
            ShowMessage('Ошибка в поле "IP-адрес"');
            DevicePage.TabIndex:= 0;
            exit(-1);
          end;

          if valf(ePort.Text) > 0 then
            Port:= ePort.Text
          else
          begin
            ShowMessage('Ошибка в поле "Порт"');
            DevicePage.TabIndex:= 0;
            exit(-2);
          end
        end;
    end;
  end;

  with ReadingsForm, ReadingsForm.SupportedDevices[eDevice1.ItemIndex + 1], DeviceForm.sgDetCommands do
  begin
    PresumedDevice:= eDevice1.Text;
    Timeout:= seRecvTimeOut.Value;
    case Connection of
      cSerial:
        begin

          BaudRate:= eBaudRate.Value;
          Databits:= valf(Cells[eDevice1.ItemIndex + 1, longint(hDataBits)]);

          s:= Cells[eDevice1.ItemIndex + 1, longint(hStopBits)];
          StopBits:= DeviceForm.cbStopBits.Items.IndexOf(s);

          Parity:= cbParity1.ItemIndex;

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
        end;
      cTelNet:
        begin
          if IsIP(eIPAdress1.Text) then
            Host:= eIPAdress1.Text
          else
          begin
            ShowMessage('Ошибка в поле "IP-адрес"');
            DevicePage.TabIndex:= 1;
            exit(-1);
          end;

          if valf(ePort1.Text) > 0 then
            Port:= ePort1.Text
          else
          begin
            ShowMessage('Ошибка в поле "Порт"');
            DevicePage.TabIndex:= 1;
            exit(-2);
          end
        end;
    end;
  end;
end;

end.

