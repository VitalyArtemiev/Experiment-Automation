unit serconf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, ComCtrls, DbCtrls, Spin, ExtCtrls, Buttons,
  Dialogs, synaser, StatusF, CustomCommandF;

type
  ConnectAction = (ANo, AQuery, AReset);

  tSSA = array of ansistring;
  pSSA = ^tSSA;

  RConfig = record
    DefaultParams: ansistring;
    LoadParamsOnStart, SaveParamsOnExit, AutoExportParams, AutoComment,
    AutoReadingConst, AutoReadingSweep, AutoReadingStep, KeepLog: boolean;
    OnConnect: ConnectAction;
  end;

  RConnectParams = record
    Device, Port, InitString: shortstring;
    Parity, StopBits: byte;
    SoftFlow, HardFlow: boolean;
    BaudRate, DataBits, Timeout: longword;
  end;

  RParams = record
    Amplitude, Offset, Frequency, SweepStartF, SweepStopF, SweepRate, StepStartF,
      StepStopF, StepF, StepStartA, StepStopA, StepA, AxisLimit: double;
    Impedance, AmpUnit, SweepType, SweepDir, SampleRate, TimeConstant, Sensitivity,
      XAxis, ReadingsMode, Display1, Display2, Ratio1, Ratio2, Show1, Show2: byte;
    ACOn, AutoSweep, GenFreq, OnePoint: boolean;
    TransferPars: array [1..11] of boolean;
    CurrFunc, UpdateInterval, ReadingTime, TimeStep, Delay: longint;
    GeneratorCP, DetectorCP: RConnectParams;
  end;

  tDevice = record   //device to each tserconform so that get rid of commcs
    Manufacturer, Model: shortstring;
    Commands: tSSA;
    ParSeparator, CommSeparator, Terminator: string;
  end;

  tUnits =       (
                  NOUNIT, VP,   VR
                  );

  tCommonCommand = (
                    RST, IDN, RCL, SAV, TST, CAL, CLS, STB, SRE, ESR, ESE, PSC , TRG
                    );

  { TSerConnectForm }

  TSerConnectForm = class(TForm)
    btApply: TButton;
    btnConnect: TButton;
    btnDisconnect: TButton;
    btnTest: TButton;
    btQuery: TSpeedButton;
    btReset: TSpeedButton;
    btStatus: TSpeedButton;
    btStop: TButton;
    btCustomCommand: TSpeedButton;
    cbPortSelect: TComboBox;
    seRecvTimeout: TSpinEdit;

    Label3: TLabel;
    Label8: TLabel;
    StatusBar1: TStatusBar;

    procedure btnConnectClick(Sender: TObject); virtual;
    procedure btCustomCommandClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure btQueryClick(Sender: TObject); virtual; abstract;
    procedure btResetClick(Sender: TObject);
    procedure btStatusClick(Sender: TObject);
  private
    fTimeOuts: longword;
    procedure SetTOE(AValue: longword);
  public
    CommCS: TRTLCriticalSection;
    SerPort: TBlockSerial;
    ConnectParams: RConnectParams;
    CommandString, PresumedDevice, CurrentDevice: shortstring;
    DeviceIndex: longint;

    property TimeOutErrors: longword read fTimeOuts write SetTOE;

    procedure InitDevice;
    function Connect: longint; virtual;
    procedure EnableControls(Enable: boolean); virtual; abstract;
    procedure AddCommand(c: variant{tCommand}; Query: boolean = false);
    procedure AddCommand(c: variant{tCommand}; Query: boolean; i: longint);
    procedure AddCommand(c: variant{tCommand}; Query: boolean; var a: array of longint);
    procedure AddCommand(c: variant{tCommand}; Query: boolean; x: real; Units: tUnits);
    procedure PassCommands;
    function RecvString: string;
  end;

  procedure WriteProgramLog(Log: string);
  function strf(x: double): string;
  function strf(x: longint): string;

const
  sUnits: array[0..2] of shortstring =   (
                  '', 'VP', 'VR'
                  );

  CommonCommand: array [RST..TRG] of ansistring = (
                    '*RST', '*IDN', '*RCL', '*SAV', '*TST', '*CAL', '*CLS', '*STB', '*SRE', '*ESR', '*ESE', '*PSC' , '*TRG'
                    );

var
  ProgramLog: TFileStream;
  LogCS: TRTLCriticalSection;

implementation

uses
  GenConst, MainF, OptionF;

function strf(x: double): string;
begin
  str(x, Result);
end;

function strf(x: longint): string;
begin
  str(x, Result);
end;

procedure WriteProgramLog(Log: string);
begin
  if Config.KeepLog then
  try
    EnterCriticalSection(LogCS);
      Log+= LineEnding;
      ProgramLog.Write(Log[1], length(Log));
  finally
    LeaveCriticalSection(LogCS);
  end;
end;

procedure TSerConnectForm.btnConnectClick(Sender: TObject);
begin
  btnDisconnectClick(Self);
  Connect;
end;

procedure TSerConnectForm.btnDisconnectClick(Sender: TObject);
begin
  if SerPort.InstanceActive then
  begin
    SerPort.Purge;
    SerPort.CloseSocket;
    StatusBar1.Caption:= 'Нет подключения';
    EnableControls(false);
    DeviceIndex:= iDefaultdevice;
  end;
end;

procedure TSerConnectForm.btnTestClick(Sender: TObject);//проверку на правильность настройки
begin
  if SerPort.InstanceActive then
  begin
    AddCommand(IDN, true);
    PassCommands;

    //DataVolume:= SerPort.WaitingData;
    //str(DataVolume, s);
    //DebugBox.Items.Strings[11]:= s;
    //setlength(s, DataVolume);
    SerPort.RaiseExcept:= false;  //sleep
    //if SerPort.CanRead(seRecvTimeout.Value) then
    CurrentDevice:= RecvString;
    if CurrentDevice <> '' then ShowMessage('Подключено к ' + CurrentDevice);
    SerPort.RaiseExcept:= true;
  end;
end;

procedure TSerConnectForm.btStatusClick(Sender: TObject);
begin
  if StatusForm.Visible then StatusForm.Hide;
  StatusForm.Form:= pointer(Self);
  StatusForm.Show;
end;

procedure TSerConnectForm.btResetClick(Sender: TObject);
begin
  AddCommand(RST);
  PassCommands;
end;

procedure TSerConnectForm.btCustomCommandClick(Sender: TObject);
begin
  if CustomCommandForm.Visible then CustomCommandForm.Hide;
  CustomCommandForm.Form:= pointer(Self);
  CustomCommandForm.Show;
end;

procedure TSerConnectForm.SetTOE(AValue: longword);
begin
  StatusBar1.SimpleText:= 'Таймаутов:' + strf(AValue);
  WriteProgramLog('Timeout receiving string');
  fTimeOuts:= AValue;
end;

procedure TSerConnectForm.InitDevice;
var
  s, t: string;
  i: word;
begin
  s:= ConnectParams.InitString;
  if s = '' then exit;

  t:= SupportedDevices[iDefaultDevice].Terminator;
  for i:= 1 to high(SupportedDevices) do
    if pos(PresumedDevice, SupportedDevices[i].Model) <> 0 then
      t:= SupportedDevices[i].Terminator;

  s+= t;

  WriteProgramLog('Строка на устройство ' + CurrentDevice);
  WriteProgramLog(s);
  WriteProgramLog('');
  SerPort.SendString(s);        //cts????
end;

function TSerConnectForm.Connect: longint;
var
  P: char;
  i: longint;
begin
  if cbPortSelect.ItemIndex >= 0 then
  begin
    WriteProgramLog('Подключение...');
    SerPort.TestDsr:= true;
    SerPort.RaiseExcept:= false;
    SerPort.Connect(cbPortSelect.Text);      //After successfull connection the DTR signal is set
    //DebugBox.Items.Strings[4]:= SerPort.Device;
    {if serport.cts then DebugBox.Items.Strings[5]:= 'cts 1';
    if serport.testdsr then DebugBox.Items.Strings[6]:= 'dsr 1';
    if SerPort.CanRead(TestTimeOut) then DebugBox.Items.Strings[7]:= 'canread';
    if SerPort.CanWrite(TestTimeOut) then DebugBox.Items.Strings[8]:= 'canwrite';   }

    if SerPort.LastError = 0 then   //TestDSR?
    with ConnectParams do
    begin
      case Parity of
        0: P:= 'N';   //none, odd, even, mark, space
        1: P:= 'O';
        2: P:= 'E';
        3: P:= 'M';
        4: P:= 'S';
      end;
      SerPort.Config(BaudRate, DataBits, P, StopBits, SoftFlow, HardFlow);
      if SerPort.LastError <> 0 then
      begin
        ShowMessage('Ошибка' + SerPort.LastErrorDesc);
        SerPort.CloseSocket;
        SerPort.RaiseExcept:= true;
        exit
      end;

        InitDevice;
        btnTestClick(Self);
        if CurrentDevice <> '' then
        begin
        for i:= 1 to high(SupportedDevices) do
          with SupportedDevices[i] do
          begin
            if (pos(Manufacturer, CurrentDevice) > 0) and (pos(Model, CurrentDevice) > 0) then
            begin
              DeviceIndex:= i;
              break;
            end;
          end;
        if DeviceIndex = iDefaultDevice then ShowMessage('Устройство не опознано');
        end
        else
        begin
          ShowMessage('Устройство не оветило на запрос модели или неверны параметры подключения.'#13#10'Исправьте параметры или введите модель подключаемого прибора.');
          OptionForm.ShowModal;
          //OptionForm.ModalResult:=;
          for i:= 1 to high(SupportedDevices) do
          with SupportedDevices[i] do
          begin
            if (pos(PresumedDevice, Manufacturer) > 0) or (pos(PresumedDevice, Model) > 0) then
            begin
              DeviceIndex:= i;
              break;
            end;
          end;
        if DeviceIndex = iDefaultDevice then ShowMessage('Устройство не опознано');
        end;

        if (PresumedDevice <> '') and (CurrentDevice <> '') and
          (PresumedDevice <> CurrentDevice) then
          ShowMessage('Внимание! Текущая конфигурация создавалась для другого прибора.');

      StatusBar1.Caption:= 'Готовность';
    end
    else
    begin
      CurrentDevice:= '';
      StatusBar1.Caption:= 'Порт не отвечает';
      SerPort.CloseSocket;
      //showmessage('');
    end
  end;
  if SerPort.InstanceActive then
  begin
    StatusBar1.Caption:= 'Есть подключение';
    Connect:= 0;
    TimeOutErrors:= 0;
    StatusBar1.SimpleText:= '';
    EnableControls(true);

    case Config.OnConnect of
      AQuery: btQueryClick(Self);
      AReset: btResetClick(Self);
    end;

    ConnectParams.Port:= cbPortSelect.Text;
    ConnectParams.TimeOut:= seRecvTimeOut.Value;
    ConnectParams.Device:= CurrentDevice;
  end
  else StatusBar1.Caption:= 'Нет подключения';
  SerPort.RaiseExcept:= true;
end;

procedure TSerConnectForm.AddCommand(c: variant{tCommand}; Query: boolean = false);
begin

  if (c > high(SupportedDevices[DeviceIndex].Commands)) or
    (SupportedDevices[DeviceIndex].Commands[c] = '') then    //commcs just bc suppdev???
  begin
    WriteProgramLog('Error: command unsopported by this device');
    exit
  end;

  if CommandString <> '' then CommandString+= SupportedDevices[DeviceIndex].CommSeparator;

  CommandString+= SupportedDevices[DeviceIndex].Commands[c];

  if Query then CommandString+= '?';
end;

procedure TSerConnectForm.AddCommand(c: variant{tCommand}; Query: boolean; i: longint);
begin
  if (c > high(SupportedDevices[DeviceIndex].Commands)) or
    (SupportedDevices[DeviceIndex].Commands[c] = '') then
  begin
    WriteProgramLog('Error: command unsopported by this device');
    exit
  end;

  if CommandString <> '' then CommandString+= SupportedDevices[DeviceIndex].CommSeparator;

  CommandString+= SupportedDevices[DeviceIndex].Commands[c];

  if Query then CommandString+= '?';

  CommandString+= strf(i);
end;

procedure TSerConnectForm.AddCommand(c: variant{tCommand}; Query: boolean; var a: array of longint);
var
  i: longint;
begin
  if (c > high(SupportedDevices[DeviceIndex].Commands)) or
    (SupportedDevices[DeviceIndex].Commands[c] = '') then
  begin
    WriteProgramLog('Error: command unsopported by this device');
    exit
  end;

  if CommandString <> '' then CommandString+= SupportedDevices[DeviceIndex].CommSeparator;

  CommandString+= SupportedDevices[DeviceIndex].Commands[c];

  if Query then CommandString+= '?';

  for i:= 0 to high(a) do
  begin
    if i <> 0 then CommandString+= SupportedDevices[DeviceIndex].ParSeparator;
    CommandString+= strf(a[i]);
  end;
end;

procedure TSerConnectForm.AddCommand(c: variant{tCommand}; Query: boolean; x: real; Units: tUnits);
begin
  if (c > high(SupportedDevices[DeviceIndex].Commands)) or
    (SupportedDevices[DeviceIndex].Commands[c] = '') then
  begin
    WriteProgramLog('Error: command unsopported by this device');
    exit
  end;

  if CommandString <> '' then CommandString+= SupportedDevices[DeviceIndex].CommSeparator;

  CommandString+= SupportedDevices[DeviceIndex].Commands[c];

  if Query then CommandString+= '?';

  CommandString+= strf(x) + sUnits[integer(Units)]; //does this work ???
end;

procedure TSerConnectForm.PassCommands;
begin
  CommandString+= SupportedDevices[DeviceIndex].Terminator;
  if (serport.instanceactive) and (serport.CTS) then
  begin
    WriteProgramLog('Строка на устройство ' + CurrentDevice);
    WriteProgramLog(CommandString);
    WriteProgramLog('');
    SerPort.SendString(CommandString);        //cts????
  end;
  CommandString:= '';
end;

function TSerConnectForm.RecvString: string;
begin
  if (serport.instanceactive) and (serport.CTS) then
  begin
    SerPort.RaiseExcept:= false;
    Result:= SerPort.RecvString(ConnectParams.Timeout);
    SerPort.RaiseExcept:= false;

    writeprogramlog('Получена строка ' + Result);

    if SerPort.LastError = ErrTimeOut then TimeOutErrors:= TimeOutErrors + 1;
  end;
end;

end.
