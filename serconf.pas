unit serconf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, ComCtrls, DbCtrls, Spin, ExtCtrls, Buttons,
  Dialogs, Grids, synaser, tlntsend, StatusF, CustomCommandF;

type
  tIntegerArray = array of integer;

  ConnectAction = (ANo, AQuery, AReset);

  eConnectionKind = (cNone, cSerial, cTelNet);

  eDeviceKind = (dGenerator, dDetector);

  tSSA = array of ansistring;
  pSSA = ^tSSA;

  rConfig = record
    DefaultParams, WorkConfig, DefaultGens, DefaultDets: shortstring;
    LoadParamsOnStart, SaveParamsOnExit, AutoExportParams, AutoComment,
    AutoReadingConst, AutoReadingSweep, AutoReadingStep, KeepLog: boolean;
    OnConnect: ConnectAction;
  end;

  rParams = record
    Amplitude, Offset, Frequency, SweepStartF, SweepStopF, SweepRate, StepStartF,
      StepStopF, StepF, StepStartA, StepStopA, StepA, AxisLimit: double;
    Impedance, CurrFunc, AmpUnit, SweepType, SweepDir, SampleRate, TimeConstant, Sensitivity,
      XAxis, ReadingsMode, Display1, Display2, Ratio1, Ratio2, Show1, Show2: shortint;
    ACOn, AutoSweep, GenFreq, OnePoint: boolean;
    TransferPars: array [0..19] of boolean;
    UpdateInterval, ReadingTime, TimeStep, Delay: longint;
    GeneratorPort, DetectorPort, LastGenerator, LastDetector: shortstring;
  end;

  pDevice = ^tDevice;

  tDevice = record
    Manufacturer, Model: string;
    Commands: tSSA;
    ParSeparator, CommSeparator, Terminator, InitString: string;
    //Connection: eConnectionKind;
    Timeout: longword;
    case Connection: eConnectionKind of
     // cNone:   ();
      cSerial: (Parity, StopBits: byte;
                SoftFlow, HardFlow: boolean;
                BaudRate, DataBits: longword);
      cTelNet: (Host, Port: string[15]);
  end;

  eUnits = (
    NOUNIT, VP,   VR
            );

  { TSerConnectForm }

  tSerConnectForm = class(tForm)
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
    TestResult: string;
    fTimeOuts: longword;

    function GetCurDev: pDevice;
    procedure SetTOE(AValue: longword);

  public
    ConnectionKind: eConnectionKind;
    CommCS: TRTLCriticalSection;
    SerPort: TBlockSerial;
    TelNetClient: tTelNetSend;
    CommandString, PresumedDevice: shortstring;
    DeviceIndex: longint;
    DeviceKind: eDeviceKind;
    SupportedDevices: array of tDevice;

    property TimeOutErrors: longword read fTimeOuts write SetTOE;
    property CurrentDevice: pDevice read GetCurDev;

    procedure GetSupportedDevices(Kind: eDeviceKind);
    procedure InitDevice;
    function ConnectSerial: longint; virtual;
    function ConnectTelNet: longint; virtual;
    procedure EnableControls(Enable: boolean); virtual; abstract;
    function GetCommandName(c: variant): string;
    procedure AddCommand(c: variant{tCommand}; Query: boolean = false);
    procedure AddCommand(c: variant{tCommand}; Query: boolean; i: longint);
    procedure AddCommand(c: variant{tCommand}; Query: boolean; var a: tIntegerArray);
    procedure AddCommand(c: variant{tCommand}; Query: boolean; x: real; Units: eUnits);
    procedure PassCommands;
    function RecvString: string;
  end;

  procedure WriteProgramLog(Log: string);
  function strf(x: double): string;
  function strf(x: longint): string;
  function valf(s: string): integer;

const
  sUnits: array[0..2] of shortstring =   (
                  '', 'VP', 'VR'
                  );

var
  ProgramLog: TFileStream;
  LogCS: TRTLCriticalSection;

implementation

uses
  DeviceF, MainF, OptionF;

function strf(x: double): string;
begin
  str(x, Result);
end;

function strf(x: longint): string;
begin
  str(x, Result);
end;

function valf(s: string): integer;
begin
  val(s, Result);
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

procedure tSerConnectForm.btnConnectClick(Sender: TObject);
begin
  btnDisconnectClick(Sender);
  if cbPortSelect.ItemIndex >= 0 then
  begin
    if cbPortSelect.Text = 'Ethernet' then
      ConnectTelNet
    else
      ConnectSerial;
  end;
end;

procedure tSerConnectForm.btnDisconnectClick(Sender: TObject);
begin
  if assigned(SerPort) then
  begin
    if SerPort.InstanceActive then
    begin
      SerPort.Purge;
      SerPort.CloseSocket;

      StatusBar1.Caption:= 'Нет подключения'; { TODO 2 -cImprovement : Sort out statusbars }
      EnableControls(false);
      ConnectionKind:= cNone;
      DeviceIndex:= iDefaultdevice;
    end;
    freeandnil(SerPort);
  end;

  if assigned(TelNetClient) then
  begin
    StatusBar1.Caption:= 'Нет подключения';
    EnableControls(false);
    ConnectionKind:= cNone;
    DeviceIndex:= iDefaultdevice;

    freeandnil(TelNetClient);
  end;
end;

procedure tSerConnectForm.btnTestClick(Sender: TObject);//проверку на правильность настройки
begin
  case ConnectionKind of
    cNone: ShowMessage('Подключение отсутствует');
    cSerial:
      begin
        if SerPort.InstanceActive then
        begin
          AddCommand(cIdentify, true);
          PassCommands;

          //DataVolume:= SerPort.WaitingData;
          //str(DataVolume, s);
          //DebugBox.Items.Strings[11]:= s;
          //setlength(s, DataVolume);
            //sleep
          //if SerPort.CanRead(seRecvTimeout.Value) then
          TestResult:= RecvString;
          if TestResult <> '' then ShowMessage('Подключено к ' + TestResult);
        end;
      end;
    cTelNet:
      begin
        AddCommand(cIdentify, true);
        PassCommands;
        TestResult:= RecvString;
        if TestResult <> '' then ShowMessage('Подключено к ' + TestResult);
      end;
  end;
end;

procedure tSerConnectForm.btStatusClick(Sender: TObject);
begin
  if StatusForm.Visible then StatusForm.Hide;
  StatusForm.Form:= pointer(Self);
  StatusForm.Show;
end;

procedure tSerConnectForm.btResetClick(Sender: TObject);
begin
  AddCommand(cReset);
  PassCommands;
end;

procedure tSerConnectForm.btCustomCommandClick(Sender: TObject);
begin
  if CustomCommandForm.Visible then CustomCommandForm.Hide;
  CustomCommandForm.Form:= pointer(Self);
  CustomCommandForm.Show;
end;

procedure tSerConnectForm.SetTOE(AValue: longword);
begin
  StatusBar1.SimpleText:= 'Таймаутов:' + strf(AValue);
  WriteProgramLog('Timeout receiving string');
  fTimeOuts:= AValue;
end;

function tSerConnectForm.GetCurDev: pDevice;
begin
  Result:= @SupportedDevices[DeviceIndex];
end;

procedure tSerConnectForm.GetSupportedDevices(Kind: eDeviceKind);
var
  i, j: integer;
  sg: tStringGrid;
begin
  case Kind of
    dGenerator: sg:= DeviceForm.sgGenCommands;
    dDetector:  sg:= DeviceForm.sgDetCommands;
  end;

  with sg do
  begin
    setlength(SupportedDevices, ColCount); //0th is defdevice
    //idefdev
    for i:= 1 to high(SupportedDevices) do
    begin
      with SupportedDevices[i] do
      begin
        Manufacturer:=  Cells[i, longint(hManufacturer)];
        Model:=         Cells[i, longint(hModel)];
        CommSeparator:= Cells[i, longint(hCommSeparator)];
        ParSeparator:=  Cells[i, longint(hParSeparator)];
        case Cells[i, longint(hTerminator)] of
          'CR':   Terminator:= CR;
          'LF':   Terminator:= LF;
          'CRLF': Terminator:= CRLF;
        end;

        TimeOut:=       valf(Cells[i, longint(hTimeout)]);

        case Cells[i, longint(hInterface)] of
          'Ethernet':
          begin
            Connection:= cTelNet;
            Host:= Cells[i, longint(hIPAdress)];
            Port:= Cells[i, longint(hPort)];
          end;
          else
          begin
            Connection:= cSerial;
            BaudRate:= valf(Cells[i, longint(hBaudRate)]);
            DataBits:= valf(Cells[i, longint(hDataBits)]);
            StopBits:= DeviceForm.cbStopBits.Items.IndexOf(Cells[i, integer(hStopBits)]);
            Parity:= DeviceForm.cbParity.Items.IndexOf(Cells[i, integer(hParity)]);
            case DeviceForm.cbHandshake.Items.IndexOf(Cells[i, integer(hHandshake)]) of
              0:
              begin
                SoftFlow:= false;
                HardFlow:= false;
              end;
              1:
              begin
                SoftFlow:= true;
                HardFlow:= false;
              end;
              2:
              begin
                SoftFlow:= false;
                HardFlow:= true;
              end;
              3:
              begin
                SoftFlow:= true;
                HardFlow:= true;
              end;
            end;
          end;
        end;
        //writeprogramlog(strf(baudrate)+''+ strf(databits)+''+ strf(stopbits));

        setlength(Commands, RowCount - SGHeaderLength);
        for j:= 0 to RowCount - SGHeaderLength  - 1 do
          Commands[j]:= Cells[i, j + SGHeaderLength];

        {for j:= 0 to high(Commands) do
          writeProgramLog(getcommandname(j) + '  ' + Commands[j]);  }
      end;


    end;
  end;

end;

procedure tSerConnectForm.InitDevice;
var
  s: string;
begin
  s:= CurrentDevice^.InitString;
  if s = '' then exit;
  s+= CurrentDevice^.Terminator;

  WriteProgramLog('Строка на устройство ' + TestResult);
  WriteProgramLog(s);
  WriteProgramLog('');
  SerPort.SendString(s);        //cts????
end;

function tSerConnectForm.ConnectSerial: longint;
var
  P: char;
  i: integer;
  s: string;
begin
  WriteProgramLog('Подключение...');

  DeviceIndex:= iDefaultDevice;
  ConnectionKind:= cSerial;
  SerPort:= TBlockSerial.Create;
  SerPort.RaiseExcept:= true;
  SerPort.ConvertLineEnd:= true;
  SerPort.DeadLockTimeOut:= 10000;

  SerPort.TestDsr:= true;
  SerPort.RaiseExcept:= false;
  SerPort.Connect(cbPortSelect.Text);
  if SerPort.LastError = 0 then
  begin
    for i:= 1 to high(SupportedDevices) do
    begin
      DeviceIndex:= i;
      with CurrentDevice^ do
        if Connection = cSerial then
        begin
          WriteProgramLog('Попытка подключения к ' + Manufacturer + ' ' + Model);
          case Parity of
            0: P:= 'N';   //none, odd, even, mark, space
            1: P:= 'O';
            2: P:= 'E';
            3: P:= 'M';
            4: P:= 'S';
          end;

          SerPort.Config(BaudRate, DataBits, P, StopBits, SoftFlow, HardFlow);

          InitDevice;
          btnTestClick(Self);
          WriteProgramLog('Ответ устройства: ' + TestResult);

          if TestResult <> '' then
          begin
            if (pos(Manufacturer, TestResult) > 0) and (pos(Model, TestResult) > 0) then
            begin
              WriteProgramLog('Успешно');
              break;
            end
            else
              DeviceIndex:= iDefaultDevice;
          end
          else
            DeviceIndex:= iDefaultDevice;
        end;
    end;
    if DeviceIndex = iDefaultDevice then ShowMessage('Устройство не опознано');
  end;

  if SerPort.LastError <> 0 then
  begin
    s:= 'Ошибка подключения: ' + SerPort.LastErrorDesc;
    WriteProgramLog(s);
    ShowMessage(s);
    SerPort.CloseSocket;
    SerPort.RaiseExcept:= true;
    ConnectionKind:= cNone;
    freeandnil(SerPort);
    exit(-1)
  end;
        {end
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

      if (PresumedDevice <> '') and (TestResult <> '') and
        (PresumedDevice <> TestResult) then
        ShowMessage('Внимание! Текущая конфигурация создавалась для другого прибора.');

    StatusBar1.Caption:= 'Готовность';
      end;}
        //After successfull connection the DTR signal is set
  //DebugBox.Items.Strings[4]:= SerPort.Device;
  {if serport.cts then DebugBox.Items.Strings[5]:= 'cts 1';
  if serport.testdsr then DebugBox.Items.Strings[6]:= 'dsr 1';
  if SerPort.CanRead(TestTimeOut) then DebugBox.Items.Strings[7]:= 'canread';
  if SerPort.CanWrite(TestTimeOut) then DebugBox.Items.Strings[8]:= 'canwrite';   }

     //TestDSR?

  if SerPort.InstanceActive then
  begin
    StatusBar1.Caption:= 'Есть подключение';
    Result:= 0;
    TimeOutErrors:= 0;
    StatusBar1.SimpleText:= '';
    EnableControls(true);

    case Config.OnConnect of
      AQuery: btQueryClick(Self);
      AReset: btResetClick(Self);
    end;

    CurrentDevice^.Port:= cbPortSelect.Text;
    CurrentDevice^.TimeOut:= seRecvTimeOut.Value;
  end
  else
    begin
      StatusBar1.Caption:= 'Нет подключения';
      SerPort.CloseSocket;
      ConnectionKind:= cNone;
      freeandnil(SerPort);
      Result:= -2;
    end;
  SerPort.RaiseExcept:= true;
end;

function tSerConnectForm.ConnectTelNet: longint;
var
  i: integer;
begin
  DeviceIndex:= iDefaultDevice;
  ConnectionKind:= cTelNet;
  TelNetClient:= tTelNetSend.Create;
  for i:= 1 to high(SupportedDevices) do
    with SupportedDevices[i] do
      if Connection = cTelNet then
      begin
        WriteProgramLog('Попытка подключения к ' + Manufacturer + ' ' + Model);
        TelNetClient.TargetHost:= Host;
        TelNetClient.TargetPort:= Port;
        TelNetClient.Timeout:= TimeOut;

        TelNetClient.Login;

        InitDevice;
        btnTestClick(Self);
        WriteProgramLog('Ответ устройства: ' + TestResult);

        if TestResult <> '' then
          if (pos(Manufacturer, TestResult) > 0) and (pos(Model, TestResult) > 0) then
          begin
            DeviceIndex:= i;
            break;
          end;
      end;
  if DeviceIndex <> iDefaultDevice then
  begin
    Result:= 0;
    case Config.OnConnect of
      AQuery: btQueryClick(Self);
      AReset: btResetClick(Self);
    end;
    CurrentDevice^.TimeOut:= seRecvTimeOut.Value;
  end
  else
  begin
    ShowMessage('Устройство не опознано');
    ConnectionKind:= cNone;
    freeandnil(TelNetClient);
    Result:= -1;
  end;
end;

function tSerConnectForm.GetCommandName(c: variant): string;
begin
  case c of
    0..integer(cTrigger):
      str(eCommonCommand(c),Result);
    else
      case DeviceKind of
        dGenerator:
        begin
          if c > integer(high(eGenCommand)) then
            Result:= 'Invalid command'
          else
            str(eGenCommand(c),Result);
        end;
        dDetector:
        begin
          if c > integer(high(eDetCommand)) then
            Result:= 'Invalid command'
          else
            str(eDetCommand(c),Result);
        end;
      end;
  end;
end;

procedure tSerConnectForm.AddCommand(c: variant; Query: boolean);
begin
  if (c > high(CurrentDevice^.Commands)) or
    (CurrentDevice^.Commands[c] = '') then    //commcs just bc suppdev???
  begin
    WriteProgramLog('Error: command "'+ GetCommandName(c) +'" unsopported by this device');
    exit
  end;

  if CommandString <> '' then CommandString+= CurrentDevice^.CommSeparator;

  CommandString+= CurrentDevice^.Commands[c];

  if Query then CommandString+= '?';
end;

procedure tSerConnectForm.AddCommand(c: variant; Query: boolean; i: longint);
begin
  if (c > high(CurrentDevice^.Commands)) or
    (CurrentDevice^.Commands[c] = '') then
  begin
    WriteProgramLog('Error: command "'+ GetCommandName(c) +'" unsopported by this device');
    exit
  end;

  if CommandString <> '' then CommandString+= CurrentDevice^.CommSeparator;

  CommandString+= CurrentDevice^.Commands[c];

  if Query then CommandString+= '?';

  CommandString+= strf(i);
end;

procedure tSerConnectForm.AddCommand(c: variant; Query: boolean;
  var a: tIntegerArray);
var
  i: longint;
begin
  if (c > high(CurrentDevice^.Commands)) or
    (CurrentDevice^.Commands[c] = '') then
  begin
    WriteProgramLog('Error: command "'+ GetCommandName(c) +'" unsopported by this device');
    exit
  end;

  if CommandString <> '' then CommandString+= CurrentDevice^.CommSeparator;

  CommandString+= CurrentDevice^.Commands[c];

  if Query then CommandString+= '?';

  for i:= 0 to high(a) do
  begin
    if i <> 0 then CommandString+= CurrentDevice^.ParSeparator;
    CommandString+= strf(a[i]);
  end;
end;

procedure tSerConnectForm.AddCommand(c: variant; Query: boolean; x: real;
  Units: eUnits);
begin
  if (c > high(CurrentDevice^.Commands)) or
    (CurrentDevice^.Commands[c] = '') then
  begin

    WriteProgramLog('Error: command "'+ GetCommandName(c) +'" unsopported by ' + Currentdevice^.Model);
    exit
  end;

  if CommandString <> '' then CommandString+= CurrentDevice^.CommSeparator;

  CommandString+= CurrentDevice^.Commands[c];

  if Query then CommandString+= '?';

  CommandString+= strf(x) + sUnits[integer(Units)]; //does this work ???
end;

procedure tSerConnectForm.PassCommands;
begin
  CommandString+= CurrentDevice^.Terminator;
  case ConnectionKind of
    //cNone: ;
    cSerial:
      begin
        if (serport.instanceactive) and (serport.CTS) then
        begin
          WriteProgramLog('Строка на устройство ' + TestResult);
          WriteProgramLog(CommandString);
          WriteProgramLog('');
          SerPort.SendString(CommandString);        //cts????
        end;
      end;
    cTelNet:
      begin
        WriteProgramLog('Строка на устройство ' + TestResult);
        WriteProgramLog(CommandString);
        WriteProgramLog('');
        TelNetClient.Send(CommandString);
      end;
  end;
  CommandString:= '';
end;

function tSerConnectForm.RecvString: string;
begin
  case ConnectionKind of
    //cNone: ;
    cSerial:
      begin
        if (serport.instanceactive) and (serport.CTS) then
        begin
          SerPort.RaiseExcept:= false;
          Result:= SerPort.RecvString(CurrentDevice^.Timeout);
          SerPort.RaiseExcept:= true;

          writeprogramlog('Получена строка ' + Result);

          if SerPort.LastError = ErrTimeOut then TimeOutErrors:= TimeOutErrors + 1;
        end;
      end;
    cTelNet:
      begin;
        Result:= TelNetClient.RecvTerminated(CurrentDevice^.Terminator);

        writeprogramlog('Получена строка ' + Result);
      end;
  end;
end;

end.

