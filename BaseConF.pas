unit BaseConF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, variants, Controls, Forms, StdCtrls, ComCtrls, DbCtrls, Spin,
  ExtCtrls, Buttons, Dialogs, Grids, synaser, tlntsend, blcksock,
  {libusboop, libusb,} StatusF, CustomCommandF{, MenuH};

type
  tIntegerArray = array of integer;
  tStringArray  = array of string;
  tVariantArray = array of variant;

  ConnectAction = (ANo, AQuery, AReset);

  eConnectionKind = (cNone = -1, cSerial, cUSB, cTelNet, cVXI);

  eDeviceKind = (dGenerator, dDetector, dTempController);

  eReadMode = (rBuffer, rSimultaneous, rRealTime);

  Buffer = array of double;
  pBuffer = ^Buffer;

  tSSA = array of ansistring;
  pSSA = ^tSSA;

  rConfig = record
    CfgFolder, WorkConfig, ParamFile, DefaultGens, DefaultDets, DefaultTemps: shortstring;
    LoadParamsOnStart, SaveParamsOnExit, AutoExportParams, AutoComment,
      AutoReadingConst, AutoReadingSweep, AutoReadingStep, KeepLog: boolean;
    OnConnect: ConnectAction;
    ReadingTime: longint;
  end;

  rParams = record
    Amplitude, Offset, Frequency, SweepStartF, SweepStopF, SweepRate, StepStartF,
      StepStopF, StepF, StepStartA, StepStopA, StepA, AxisLimit: double;
    Impedance, CurrFunc, AmplUnit, SweepType, SweepDir, Modulation, SampleRate, TimeConstant,
      Sensitivity, CloseReserve, WideReserve, InputRange,
      XAxis, ReadingsMode, Display1, Display2, Ratio1, Ratio2, Show1, Show2: shortint;
    ACOn, AutoSweep, GenFreq: boolean;
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
      cVXI:    (Address: string[32]);
  end;

  eUnits = (
    uNone = -1, uVPeak, uVRms, udBm
            );

  { tConnectionForm }

  tConnectionForm = class(tForm)
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
    StatusBar: TStatusBar;//THintRedirStatusBar;

    procedure btnConnectClick(Sender: TObject); virtual;
    procedure btCustomCommandClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure btQueryClick(Sender: TObject); virtual; abstract;
    procedure btResetClick(Sender: TObject);
    procedure btStatusClick(Sender: TObject);
  protected
    DisplayMessages: boolean; //only for connect procedures
    procedure CreateSocket(Sender: TObject);
  private
    TestResult: string;
    fTimeOuts: longword;

    function GetCurDev: pDevice;
    procedure SetTOE(AValue: longword);
  public
    ConnectionKind: eConnectionKind;
    CommCS: tRTLCriticalSection;

    SerPort: tBlockSerial;
    TelNetClient: tTelNetSend;
    //Session: tVisaSession;
    //UsbContext : tLibUsbContext;

    ParamsApplied: boolean;
    CommandString: shortstring;
    DeviceIndex: longint;
    DeviceKind: eDeviceKind;
    SupportedDevices: array of tDevice;
    AmplitudeUnits: array [-1..2] of string;
    MinDelay, RealTimeWaitPoints: longword;

    property TimeOutErrors: longword read fTimeOuts write SetTOE;
    property CurrentDevice: pDevice read GetCurDev;

    procedure SetCursorAll(Cur: tCursor);

    procedure GetSupportedDevices(Kind: eDeviceKind);
    procedure GetDeviceParams; virtual; abstract;
    procedure InitDevice;
    function RequestIdentity(TimeOut: longword): string;

    function ConnectSerial: longint; virtual;
    function ConnectTelNet: longint; virtual;
    function ConnectVXI: longint; virtual;
    function ConnectUSB: longint; virtual;
    function AutoConnect: longint; virtual;

    procedure EnableControls(Enable: boolean); virtual; abstract;
    function GetCommandName(c: variant): string;
    function CommandSupported(c: variant): boolean;
    procedure AddCommand(c: variant{tCommand}; Query: boolean = false);
    procedure AddCommand(c: variant{tCommand}; Query: boolean; s: string);
    procedure AddCommand(c: variant{tCommand}; Query: boolean; i: longint);
    procedure AddCommand(c: variant{tCommand}; Query: boolean; var a: tIntegerArray);
    procedure AddCommand(c: variant{tCommand}; Query: boolean; var a: tStringArray);
    procedure AddCommand(c: variant{tCommand}; Query: boolean; var a: tVariantArray);  //supports ordinal, float, string
    procedure AddCommand(c: variant{tCommand}; Query: boolean; x: double; Units: eUnits = uNone);
    procedure PassCommands;                                                     //Sometimes there's a framing error when communicating with devices. (In call ClearCommError) I added a crutch to synaser to ignore it as it seems not to affect communications
    procedure Purge;
    function RecvString: string;
    function RecvString(TimeOut: longword): string;

    procedure SaveState(FileStream: tFileStream);
    function RestoreState(FileStream: tFileStream): integer;
    function RestoreState(FileName: string): integer;
    procedure EnsureValidItemIndices;
  end;

  procedure WriteProgramLog(Log: string; Force: boolean = false);
  procedure WriteProgramLog(i: longint; Force: boolean = false);
  procedure WriteProgramLog(i: int64; Force: boolean = false);
  procedure WriteProgramLog(d: double; Force: boolean = false);
  procedure SeparateIndices(Source: string; sa: TStrings; var ia: tStringArray);

  function strf(x: double): string;
  function strf(x: longint): string;
  function strf(x: int64): string;
  function valf(s: string): integer;
  function vald(s: string): double;

  function CopyFromTo(Origin: string; Start, Stop: string): string;
  function CopyFromTo(Origin: string; SearchStart: integer; Start, Stop: string): string;

  function CopyDelFromTo(var Origin: string; Start, Stop: string): string;
 // function SubstrCount(const aString, aSubstring: string): Integer;

const
  iDefaultDevice = 0;

  spConnection = 0;
  spDevice = 1;
  spTimeOuts = 2;
  spStatus = 3;

  GenCaption = 'Управление генератором';
  DetCaption = 'Управление детектором';
  TempCaption = 'Управление термоконтроллером';

  StartCaption = 'Начать снятие';
  PauseCaption = 'Приостановить';
  ContinueCaption = 'Продолжить';

  IndexDelim = ' - ';

  CfgExt = '.cfg';
  PrExt = '.pr';
  LogExtensions: array[0..2] of string = ('.genlog', '.detlog', '.templog');
  ChartZoomFactor = 2;

var
  ProgramLog: TFileStream;
  LogCS: TRTLCriticalSection;

implementation

uses
  sockets, LConvEncoding, Math, EditBtn, StrUtils, DeviceF, MainF;

function strf(x: double): string; inline;
begin
  str(x, Result);
end;

function strf(x: longint): string; inline;
begin
  str(x, Result);
end;

function strf(x: int64): string;
begin
  str(x, Result);
end;

function valf(s: string): integer; inline;
begin
  val(s, Result);
end;

function vald(s: string): double; inline;
begin
  val(s, Result);
end;

function CopyFromTo(Origin: string; Start, Stop: string): string;
var
  p1, p2: integer;
begin
  p1:= Pos(Start, Origin) + 1;
  p2:= Pos(Stop, Origin);
  Result:= Copy(Origin, p1, p2 - p1);
end;

function CopyFromTo(Origin: string; SearchStart: integer; Start, Stop: string
  ): string;
var
  p1, p2: integer;
begin
  Delete(Origin, 1, SearchStart - 1);
  p1:= Pos(Start, Origin) + 1;
  p2:= Pos(Stop, Origin);
  Result:= Copy(Origin, p1, p2 - p1);
end;

function CopyDelFromTo(var Origin: string; Start, Stop: string): string;
var
  p1, p2: integer;
begin
  p1:= Pos(Start, Origin) + 1;
  p2:= Pos(Stop, Origin);
  Result:= Copy(Origin, p1, p2 - p1);
  Delete(Origin, 1, p2 + length(Stop) - 1);
end;

{function SubstrCount(const aString, aSubstring: string): Integer;
var
  lPosition: Integer;
begin
  Result := 0;
  lPosition := PosEx(aSubstring, aString);
  while lPosition <> 0 do
  begin
    Inc(Result);
    lPosition := PosEx(aSubstring, aString, lPosition + Length(aSubstring));
  end;
end; }

procedure WriteProgramLog(Log: string; Force: boolean = false); inline;
begin
  if Config.KeepLog or Force then
  try
    EnterCriticalSection(LogCS);
      Log+= LineEnding;
      ProgramLog.Write(Log[1], length(Log));
  finally
    LeaveCriticalSection(LogCS);
  end;
end;

procedure WriteProgramLog(i: longint; Force: boolean = false); inline;
begin
  WriteProgramLog(strf(i), Force);
end;

procedure WriteProgramLog(i: int64; Force: boolean = false); inline;
begin
  WriteProgramLog(strf(i), Force);
end;

procedure WriteProgramLog(d: double; Force: boolean = false); inline;
begin
  WriteProgramLog(strf(d), Force);
end;

procedure SeparateIndices(Source: string; sa: TStrings; var ia: tStringArray);
var
  s: string;
  i: integer;
begin
  sa.Clear;
  sa.AddText(Source);
  setlength(ia, sa.Count);
  for i:= 0 to sa.Count - 1 do
  begin
    s:= sa[i];
    if pos(IndexDelim, s) <> 0 then
    begin
      ia[i]:= CopyDelFromTo(s, '', IndexDelim);
      sa[i]:= s;
    end
    else
    begin
      ia[i]:= strf(i);
    end;
  end;
end;

procedure tConnectionForm.btnConnectClick(Sender: TObject);
var
  Result: integer;
  Crutch: integer absolute Result;
begin
  SetCursorAll(crHourGlass);

  btnDisconnectClick(Sender);
  if cbPortSelect.ItemIndex >= 0 then
  begin
    if pos('Ethernet', cbPortSelect.Text) <> 0 then
      Result:= ConnectTelNet
    else
      Result:= ConnectSerial;
  end;
  if ConnectionKind <> cNone then
  begin
    StatusBar.Panels[spDevice].Text:= CurrentDevice^.Model;
    StatusBar.Panels[spStatus].Text:= '';
    case DeviceKind of
      dGenerator:      Caption:= GenCaption + ' ' + CurrentDevice^.Model;
      dDetector:       Caption:= DetCaption + ' ' + CurrentDevice^.Model;
      dTempController: Caption:= TempCaption + ' ' + CurrentDevice^.Model;
    end;
  end;
  if (Result = 0) or debug then
  begin
    Crutch:= cbPortSelect.ItemIndex;  //because it loads port too
    GetDeviceParams;
    try
      if FileExists(MainForm.FullCfgDir + Config.ParamFile) then
        RestoreState(MainForm.FullCfgDir + Config.ParamFile)
      else
        writeprogramlog('File ' + MainForm.FullCfgDir + Config.ParamFile + ' not found');
    except
      on E: Exception do
        ShowMessage(E.Message);
    end;
    EnsureValidItemIndices;

    cbPortSelect.ItemIndex:= Crutch;
  end
  else
    WriteProgramLog('Подключение - результат: ' + strf(Result));
  SetCursorAll(crDefault);
end;

procedure tConnectionForm.btnDisconnectClick(Sender: TObject);
begin
  ParamsApplied:= false;
  if assigned(SerPort) then
  begin
    if SerPort.InstanceActive then
    begin
      SerPort.Purge;
      SerPort.CloseSocket;
    end;
    freeandnil(SerPort);
  end;

  if assigned(TelNetClient) then
  begin
    freeandnil(TelNetClient);
  end;

  EnableControls(false);
  ConnectionKind:= cNone;
  DeviceIndex:= iDefaultDevice;
  StatusBar.Panels[spDevice].Text:= CurrentDevice^.Model; //why? iunno. whats yo case anyway, m8? why do you care? i dont. look at me, im nick wilde and i dont care. just look at how much i dont care. its astonishing. one could even say its un-'care'-cteristic of me to care so little. get it? actually, i dont care if you get it. buzz off. get off ma case, pal! or else!
  StatusBar.Panels[spConnection].Text:= 'Нет подключения';

  case DeviceKind of
    dGenerator:      Caption:= GenCaption;
    dDetector:       Caption:= DetCaption;
    dTempController: Caption:= TempCaption;
  end;
end;

procedure tConnectionForm.btnTestClick(Sender: TObject);//проверку на правильность настройки
begin
  if ConnectionKind = cNone then
  begin
    ShowMessage('Подключение отсутствует');
    exit;
  end;

  TestResult:= RequestIdentity(seRecvTimeout.Value);
  if not IsEmptyStr(TestResult, [' ']) then
    ShowMessage('Подключено к ' + TestResult);
end;

procedure tConnectionForm.btStatusClick(Sender: TObject);
begin
  if StatusForm.Visible then
    StatusForm.Hide;
  StatusForm.Form:= pointer(Self);
  StatusForm.Show;
end;

procedure tConnectionForm.CreateSocket(Sender: TObject);    //this was supposed to solve winxp telnet delay problem
var
  Socket: TSocket;
  f: longint;
begin
  Socket:= TelNetClient.Sock.Socket;

   writeprogramlog(Socket);
    f:= 1;
    f:= fpsetsockopt(Socket, IPPROTO_TCP, TCP_NODELAY, @f, Sizeof(f));
    WriteprogramLog(f);

    f:= socketerror;

    case f of
      ESockEBADF: WriteprogramLog('badf');
      ESockENOTSOCK: WriteprogramLog('notsock');
      ESockEFAULT: WriteprogramLog('fault');
      else
        WriteprogramLog('err ' + strf(f));
    end;
end;

procedure tConnectionForm.btResetClick(Sender: TObject);
begin
  Purge;
  EnterCriticalSection(CommCS);
    AddCommand(cReset);
    PassCommands;
  LeaveCritiCalSection(CommCS);
end;

procedure tConnectionForm.btCustomCommandClick(Sender: TObject);
begin
  if CustomCommandForm.Visible then
    CustomCommandForm.Hide;
  CustomCommandForm.Form:= pointer(Self);
  CustomCommandForm.Show;
end;

procedure tConnectionForm.SetTOE(AValue: longword);
begin
  StatusBar.Panels[spTimeouts].Text:= 'Таймаутов: ' + strf(AValue);
  WriteProgramLog('Timeout receiving string');
  fTimeOuts:= AValue;
end;

procedure tConnectionForm.SetCursorAll(Cur: tCursor);
var
   i, j: Integer;
begin
  Cursor := Cur;
  for i := 0 to ControlCount - 1 do
  begin
    if (Controls[i].ClassType <> TBitBtn) then
      Controls[i].Cursor := Cur;

    if Controls[i] is tPanel then
    with Controls[i] as tPanel do
    begin
      for j := 0 to ControlCount - 1 do
      begin
        if (Controls[j].ClassType <> TBitBtn) then
           Controls[j].Cursor := Cur;
       // showmessage(Controls[j].name);
      end;
    end;
  end;
end;

function tConnectionForm.GetCurDev: pDevice;
begin
  Result:= @SupportedDevices[DeviceIndex];
end;

procedure tConnectionForm.GetSupportedDevices(Kind: eDeviceKind);
var
  i, j: integer;
  sg: tStringGrid;
begin
  case Kind of
    dGenerator:      sg:= DeviceForm.sgGenCommands;
    dDetector:       sg:= DeviceForm.sgDetCommands;
    dTempController: sg:= DeviceForm.sgTempCommands;
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
        InitString:=    Cells[i, longint(hInitString)];
        case Cells[i, longint(hTerminator)] of
          'CR':   Terminator:= CR;
          'LF':   Terminator:= LF;
          'CRLF': Terminator:= CRLF;
        end;
        TimeOut:= valf(Cells[i, longint(hTimeout)]);

        case Cells[i, longint(hInterface)] of
          TelnetString:
          begin
            Connection:= cTelNet;
            Host:= Cells[i, longint(hIPAdress)];
            Port:= Cells[i, longint(hPort)];
          end;
          VXIString:
          begin
            Connection:= cVXI;
            Host:= Cells[i, longint(hIPAdress)];
            Port:= Cells[i, longint(hPort)];
          end;
          USBString:
          begin
            Connection:= cUSB;
          end;
          SerialString:
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

        {writeprogramlog(model, true);   //this checks if commands are matched correctly
        for j:= 0 to high(Commands) do
          writeProgramLog(getcommandname(j) + '  ' + Commands[j], true);}
      end;
    end;
  end;
end;

procedure tConnectionForm.InitDevice;
var
  s: string;
begin
  s:= CurrentDevice^.InitString;
  if IsEmptyStr(s, [' ']) then
    exit;
  s+= CurrentDevice^.Terminator;

  WriteProgramLog('Строка на устройство ' + TestResult);
  WriteProgramLog(s);
  WriteProgramLog('');

  case ConnectionKind of
    //cNone: ;
    cSerial:
      begin
        SerPort.SendString(s);  //cts????
      end;
    cUSB: ;
    cTelNet:
      begin

      end;
    cVXI: ;
  end;
end;

function tConnectionForm.RequestIdentity(TimeOut: longword): string;
begin
  Purge;
  case ConnectionKind of
    cNone: Result:= '';
    cSerial:
      begin
        if SerPort.InstanceActive then { TODO 3 -cImprovement : unify? }
        begin
          EnterCriticalSection(CommCS);
            AddCommand(cIdentify, true);
            PassCommands;
            Result:= RecvString(TimeOut);
            if Result = '' then
              Result:= RecvString(TimeOut);
          LeaveCriticalSection(CommCS);
          //if SerPort.CanRead(seRecvTimeout.Value) then
        end;
      end;
    cTelNet:
      begin
        EnterCriticalSection(CommCS);
          AddCommand(cIdentify, true);
          PassCommands;
          Result:= RecvString(TimeOut);
        LeaveCriticalSection(CommCS);
      end;

    cUSB:
      begin

      end;
    cVXI:
      begin
        EnterCriticalSection(CommCS);
          AddCommand(cIdentify, true);
          PassCommands;
          Result:= RecvString(TimeOut);
        LeaveCriticalSection(CommCS);
      end;
  end;
end;

function tConnectionForm.ConnectSerial: longint;
var
  P: char;
  i: integer;
  s: string;
  enc: boolean;
begin
  WriteProgramLog('Подключение...');

  DeviceIndex:= iDefaultDevice;
  TestResult:= '';
  ConnectionKind:= cSerial;
  SerPort:= TBlockSerial.Create;
  SerPort.ConvertLineEnd:= true;
  SerPort.DeadLockTimeOut:= 2000;

  SerPort.TestDsr:= true;
  try
    SerPort.Connect(cbPortSelect.Text);
    if SerPort.LastError = 0 then
    begin
      for i:= 1 to high(SupportedDevices) do
      begin
        DeviceIndex:= i;
        SerPort.RaiseExcept:= false;   //because this resets in recvstring
        with CurrentDevice^ do
          if Connection = cSerial then
          begin
            s:= 'Попытка подключения к ' + Model;
            StatusBar.Panels[spStatus].Text:= s;
            StatusBar.Update;
            WriteProgramLog(s);
            case Parity of
              0: P:= 'N';   //none, odd, even, mark, space
              1: P:= 'O';
              2: P:= 'E';
              3: P:= 'M';
              4: P:= 'S';
            end;
            SerPort.Config(BaudRate, DataBits, P, StopBits, SoftFlow, HardFlow);

            if Serport.raiseexcept then showmessage('HOWWW');
            InitDevice;
            TestResult:= RequestIdentity(seRecvTimeout.Value);
            WriteProgramLog('Ответ устройства: ' + TestResult);

            if not IsEmptyStr(TestResult, [' ']) then
            begin
              if (pos(Manufacturer, TestResult) > 0) and (pos(Model, TestResult) > 0) then
              begin
                Result:= 0;
                WriteProgramLog('Успешно');
                break;
              end
              else
              begin
                Result:= -2; //not found
                DeviceIndex:= iDefaultDevice;
              end;
            end
            else
            begin
              Result:= -1; //no answer
              DeviceIndex:= iDefaultDevice;
            end;
          end
          else
          begin
            Result:= -3; //none with this interface
            DeviceIndex:= iDefaultDevice;
          end;
      end;
    end
    else
    begin
      Result:= -4   //synaser error
    end;

    if SerPort.InstanceActive and (Result = 0) then
    begin
      StatusBar.Panels[spConnection].Text:= 'Подключено к ' + cbPortSelect.Text;
      Result:= 0;
      TimeOutErrors:= 0;
      EnableControls(true);

      SerPort.RaiseExcept:= true;

      case Config.OnConnect of
        AQuery: btQueryClick(Self);
        AReset: btResetClick(Self);
      end;

      CurrentDevice^.Port:= cbPortSelect.Text;
      CurrentDevice^.TimeOut:= seRecvTimeOut.Value;
    end
    else
    begin
      StatusBar.Panels[spConnection].Text:= 'Нет подключения';
      case Result of
        -1: StatusBar.Panels[spStatus].Text:= 'Устройство не ответило';
        -2: StatusBar.Panels[spStatus].Text:= 'Устройство ' + TestResult + ' не опознано';
        -3: StatusBar.Panels[spStatus].Text:= 'Нет совместимых устройств - COM';
        -4:  //problems with encoding from win drivers
          begin
            s:= SerPort.LastErrorDesc;
            s:= ConvertEncodingToUTF8(s, GuessEncoding(s), enc);
            StatusBar.Panels[spStatus].Text:= 'Ошибка подключения: ' + s;
          end;
      end;
      if DisplayMessages then
        ShowMessage(StatusBar.Panels[spStatus].Text);
      WriteProgramLog(StatusBar.Panels[spStatus].Text);

      SerPort.CloseSocket;
      ConnectionKind:= cNone;
      freeandnil(SerPort);
      if Result = 0 then
        Result:= -5; //this should never happen; instance not active
      exit;
    end;

  except
    on E:Exception do
    begin
      WriteProgramLog(E.Message, true);
      WriteProgramLog(SerPort.LastError, true);
      WriteProgramLog('LB' + SerPort.LineBuffer, true);
      ShowMessage(E.Message);
      SerPort.CloseSocket;
      ConnectionKind:= cNone;
      freeandnil(SerPort);
      Result:= -6; //driver error: receive framing error
    end;
  end;
end;

function tConnectionForm.ConnectTelNet: longint;
var
  i: integer;
  s: string;
begin
  DeviceIndex:= iDefaultDevice;
  TestResult:= '';
  ConnectionKind:= cTelNet;
  TelNetClient:= tTelNetSend.Create;
  TelNetClient.Sock.RaiseExcept:= false;

  try
    for i:= 1 to high(SupportedDevices) do
    begin
      DeviceIndex:= i;
      with CurrentDevice^ do
        if Connection = cTelNet then
        begin
          s:= 'Попытка подключения к ' + Model;
          StatusBar.Panels[spStatus].Text:= s;
          StatusBar.Update;
          WriteProgramLog(s);
          WriteProgramLog(Host + ':' + Port);

          TelNetClient.TermType:= '';
          TelNetClient.TargetHost:= Host;
          TelNetClient.TargetPort:= Port;
          TelNetClient.Timeout:= TimeOut;

          //TelNetClient.Sock.OnCreateSocket:= @CreateSocket;  no noticable effect

          TelNetClient.Login;

          InitDevice;
          TestResult:= RequestIdentity(seRecvTimeout.Value);
          WriteProgramLog('Ответ устройства: ' + TestResult);

          if not IsEmptyStr(TestResult, [' ']) then
          begin
            if (pos(Manufacturer, TestResult) > 0) and (pos(Model, TestResult) > 0) then
            begin
              Result:= 0;
              DeviceIndex:= i;
              break;
            end
            else
            begin
              Result:= -2; //not found
              DeviceIndex:= iDefaultDevice;
            end;
          end
          else
          begin
            Result:= -1; //no answer
            DeviceIndex:= iDefaultDevice;
          end;
        end
        else
        begin
          Result:= -3;  //no compatible devices
          DeviceIndex:= iDefaultDevice;
        end;
    end;

    if TelNetClient.Sock.LastError <> 0 then
    begin
      TelNetClient.Sock.RaiseExcept:= true;
      Result:= -4 //synaser error
    end;

    if Result = 0 then
    begin
      StatusBar.Panels[spConnection].Text:= CurrentDevice^.Host;
      TimeOutErrors:= 0;
      EnableControls(true);

      TelNetClient.Sock.RaiseExcept:= true;

      case Config.OnConnect of
        AQuery: btQueryClick(Self);
        AReset: btResetClick(Self);
      end;

      CurrentDevice^.TimeOut:= seRecvTimeOut.Value;
    end
    else
    begin
      StatusBar.Panels[spConnection].Text:= 'Нет подключения';
      case Result of
        -1: StatusBar.Panels[spStatus].Text:= 'Устройство не ответило';
        -2: StatusBar.Panels[spStatus].Text:= 'Устройство ' + TestResult + ' не опознано';
        -3: StatusBar.Panels[spStatus].Text:= 'Нет совместимых устройств - TelNet';
        -4: StatusBar.Panels[spStatus].Text:= 'Ошибка подключения: ' + TelNetClient.Sock.LastErrorDesc;
      end;
      if DisplayMessages then
        ShowMessage(StatusBar.Panels[spStatus].Text);
      WriteProgramLog(StatusBar.Panels[spStatus].Text);

      ConnectionKind:= cNone;
      freeandnil(TelNetClient);
    end;
  except
    on E:Exception do
    begin
      WriteProgramLog(E.Message, true);
      WriteProgramLog(TelNetClient.Sock.LastError, true);
      WriteProgramLog(TelNetClient.Sock.LineBuffer, true);
      ShowMessage(E.Message);
      TelNetClient.Sock.CloseSocket;
      ConnectionKind:= cNone;
      freeandnil(TelNetClient);
      Result:= -6; //driver error
    end;
  end;
end;

function tConnectionForm.ConnectVXI: longint;
var
  i: integer;
begin
 { DeviceIndex:= iDefaultDevice;
  ConnectionKind:= cVXI;
  Session:= tVisaSession.Create(nil);
  for i:= 1 to high(SupportedDevices) do
    with SupportedDevices[i] do
      if Connection = cVXI then
      begin
        WriteProgramLog('Попытка подключения к ' + Manufacturer + ' ' + Model);
        Session.Address:= Address;
        Session.ConnectTimeout:= Timeout;
        Session.Active:= true;

        InitDevice;
        TestResult:= RequestIdentity;
        WriteProgramLog('Ответ устройства: ' + TestResult);

        if not IsEmptyStr(TestResult, [' ']) then
          if (pos(Manufacturer, TestResult) > 0) and (pos(Model, TestResult) > 0) then
          begin
            DeviceIndex:= i;
            break;
          end;
      end;
  if DeviceIndex <> iDefaultDevice then
  begin
    StatusBar.Panels[spConnection].Text:= CurrentDevice^.Host;
    Result:= 0;
    case Config.OnConnect of
      AQuery: btQueryClick(Self);
      AReset: btResetClick(Self);
    end;
    CurrentDevice^.TimeOut:= seRecvTimeOut.Value;
  end
  else
  begin
    StatusBar.Panels[spStatus].Text:= 'Устройство не опознано';
    ShowMessage('Устройство не опознано');
    ConnectionKind:= cNone;
    freeandnil(Session);
    Result:= -1;
  end; }
end;

function tConnectionForm.ConnectUSB: longint;
var
  i, USBDeviceCount: integer;
 // USBDevices: PPlibusb_device ;
  //portpath: TDynByteArray;
  Address, Bus, Port: byte;
 // Descriptor: libusb_device_descriptor;
begin
  {DeviceIndex:= iDefaultDevice;
  ConnectionKind:= cUSB;
  USBContext.Create;
  USBDeviceCount:= eLibUSB.Check(USBCOntext.GetDeviceList(USBDevices), 'Список устройств');

    For i:= 0 to USBDeviceCount - 1 do
      Begin
        Address  := UsbContext.GetDeviceAddress   (USBDevices[I]);
        Bus      := UsbContext.GetBusNumber       (USBDevices[I]);
        Port     := UsbContext.GetPortNumber      (USBDevices[I]);
        PortPath := USBContext.GetPortPath        (USBDevices[I]);
        //Speed    := TLibUsbContext.GetDeviceSpeed     (USBDevices[I]);
        Descriptor  := UsbContext.GetDeviceDescriptor(USBDevices[I]);
        writeprogramlog('Bus' + strf(Bus));
        writeprogramlog('Device'+strf(Address)+': ID ' +IntToHex(Descriptor.idVendor,4) +':' +IntToHex(Descriptor.idProduct,4));
        Write(',  port: ',Port:3);
      end;

  USBContext.FreeDeviceList(USBDevices);
  for i:= 1 to high(SupportedDevices) do
    with SupportedDevices[i] do
      if Connection = cUSB then
      begin
        WriteProgramLog('Попытка подключения к ' + Manufacturer + ' ' + Model);
        TelNetClient.TargetHost:= Host;
        TelNetClient.TargetPort:= Port;
        TelNetClient.Timeout:= TimeOut;

        TelNetClient.Login;

        InitDevice;
        TestResult:= RequestIdentity;
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
    StatusBar.Panels[spConnection].Text:= CurrentDevice^.Host;
    Result:= 0;
    case Config.OnConnect of
      AQuery: btQueryClick(Self);
      AReset: btResetClick(Self);
    end;
    CurrentDevice^.TimeOut:= seRecvTimeOut.Value;
  end
  else
  begin
    StatusBar.Panels[spStatus].Text:= 'Устройство не опознано';
    ShowMessage('Устройство не опознано');
    ConnectionKind:= cNone;
    freeandnil(USBContext);
    Result:= -1;
  end; }
end;

function tConnectionForm.AutoConnect: longint;
var
  i, Crutch: integer;
begin
  btnDisconnectClick(Self);
  DisplayMessages:= false;

  if pos('Ethernet', cbPortSelect.Text) <> 0 then                               //first connect to last saved
      Result:= ConnectTelNet
    else
      Result:= ConnectSerial;

  if Result <> 0 then
    for i:= 0 to cbPortSelect.Items.Count - 1 do
    begin
      cbPortSelect.ItemIndex:= i;
      Update;
      if pos('Ethernet', cbPortSelect.Text) <> 0 then
        Result:= ConnectTelNet
      else
        Result:= ConnectSerial;
      if Result = 0 then
        break;
    end;

  DisplayMessages:= true;

  if ConnectionKind <> cNone then
  begin
    StatusBar.Panels[spDevice].Text:= CurrentDevice^.Model;
    StatusBar.Panels[spStatus].Text:= '';
  end;

  if (Result = 0) or debug then
  begin
    Crutch:= cbPortSelect.ItemIndex;  //because it loads port too
    GetDeviceParams;
    try
      if FileExists(MainForm.FullCfgDir + Config.ParamFile) then
        RestoreState(MainForm.FullCfgDir + Config.ParamFile)
      else
        writeprogramlog('File ' + MainForm.FullCfgDir + Config.ParamFile + ' not found');
    except
      on E: Exception do
        ShowMessage(E.Message);
    end;
    cbPortSelect.ItemIndex:= Crutch;
  end
  else
    WriteProgramLog('Подключение - результат: ' + strf(Result));
  Update;
end;

function tConnectionForm.GetCommandName(c: variant): string;
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
        dTempController:
        begin
          if c > integer(high(eTempCommand)) then
            Result:= 'Invalid command'
          else
            str(eDetCommand(c),Result);
        end;
      end;
  end;
end;

function tConnectionForm.CommandSupported(c: variant): boolean; { TODO 1 -cImprovement : change some visibility stuff to this? }
begin                                                              //currently only used in offsetf
  if (c > high(CurrentDevice^.Commands)) or
     (CurrentDevice^.Commands[c] = '') then
    Result:= false
  else
    Result:= true;
end;

procedure tConnectionForm.AddCommand(c: variant; Query: boolean);
begin
  with CurrentDevice^ do
  begin
    if (c > high(Commands)) or
      (Commands[c] = '') then  //time critical so no complicated checks???
    begin
      WriteProgramLog('Error: command "'+ GetCommandName(c) +'" unsopported by ' + strf(deviceindex){Model});
      exit
    end;

    if CommandString <> '' then
      CommandString+= CommSeparator;

    CommandString+= Commands[c];

    if Query then
      CommandString+= '?';
  end;
end;

procedure tConnectionForm.AddCommand(c: variant; Query: boolean; s: string);
begin
  with CurrentDevice^ do
  begin
    if (c > high(Commands)) or
      (Commands[c] = '') then  //time critical so no complicated checks???
    begin
      WriteProgramLog('Error: command "'+ GetCommandName(c) +'" unsopported by ' + Model);
      exit
    end;

    if CommandString <> '' then
      CommandString+= CommSeparator;

    CommandString+= Commands[c];

    if Query then
      CommandString+= '?';

    CommandString+= s;
  end;
end;

procedure tConnectionForm.AddCommand(c: variant; Query: boolean; i: longint);
begin
  with CurrentDevice^ do
  begin
    if (c > high(Commands)) or
      (Commands[c] = '') then
    begin
      WriteProgramLog('Error: command "'+ GetCommandName(c) +'" unsopported by ' + Model);
      exit
    end;

    if CommandString <> '' then
      CommandString+= CommSeparator;

    CommandString+= Commands[c];

    if Query then
      CommandString+= '?';

    CommandString+= strf(i);
  end;
end;

procedure tConnectionForm.AddCommand(c: variant; Query: boolean;
  var a: tIntegerArray);
var
  i: longint;
begin
  with CurrentDevice^ do
  begin
    if (c > high(Commands)) or
      (Commands[c] = '') then
    begin
      WriteProgramLog('Error: command "'+ GetCommandName(c) +'" unsopported by ' + Model);
      exit
    end;

    if CommandString <> '' then
      CommandString+= CommSeparator;

    CommandString+= Commands[c];

    if Query then
      CommandString+= '?';

    for i:= 0 to high(a) do
    begin
      if i <> 0 then
        CommandString+= ParSeparator;
      CommandString+= strf(a[i]);
    end;
  end;
end;

procedure tConnectionForm.AddCommand(c: variant; Query: boolean;
  var a: tStringArray);
var
  i: longint;
begin
  with CurrentDevice^ do
  begin
    if (c > high(Commands)) or
      (Commands[c] = '') then
    begin
      WriteProgramLog('Error: command "'+ GetCommandName(c) +'" unsopported by ' + Model);
      exit
    end;

    if CommandString <> '' then
      CommandString+= CommSeparator;

    CommandString+= Commands[c];

    if Query then
      CommandString+= '?';

    for i:= 0 to high(a) do
    begin
      if i <> 0 then
        CommandString+= ParSeparator;
      CommandString+= a[i];
    end;
  end;
end;

procedure tConnectionForm.AddCommand(c: variant; Query: boolean;
  var a: tVariantArray);
var
  i: longint;
begin
  with CurrentDevice^ do
  begin
    if (c > high(Commands)) or
      (Commands[c] = '') then
    begin
      WriteProgramLog('Error: command "'+ GetCommandName(c) +'" unsopported by ' + Model);
      exit
    end;

    if CommandString <> '' then
      CommandString+= CommSeparator;

    CommandString+= Commands[c];

    if Query then
      CommandString+= '?';

    for i:= 0 to high(a) do
    begin
      if i <> 0 then
        CommandString+= ParSeparator;

      if VarIsOrdinal(a[i]) then
        CommandString+= strf(integer(a[i]))
      else
      if VarIsFloat(a[i]) then
        CommandString+= strf(double(a[i]))
      else
      if VarIsStr(a[i]) then
        CommandString+= a[i];
    end
  end;
end;

procedure tConnectionForm.AddCommand(c: variant; Query: boolean; x: double;
  Units: eUnits = uNone);
begin
  with CurrentDevice^ do
  begin
    if (c > high(Commands)) or
      (Commands[c] = '') then
    begin
      WriteProgramLog('Error: command "'+ GetCommandName(c) +'" unsopported by ' + Model);
      exit
    end;

    if CommandString <> '' then
      CommandString+= CommSeparator;

    CommandString+= Commands[c];

    if Query then
      CommandString+= '?';

    CommandString+= strf(x) + AmplitudeUnits[integer(Units)]; //does this work ???
  end;
end;

procedure tConnectionForm.PassCommands;
begin
  CommandString+= CurrentDevice^.Terminator;
  try
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
  except
    on E: Exception do
      WriteProgramLog('PassCommands exception: ' + E.Message);
  end;
  CommandString:= '';
end;

procedure tConnectionForm.Purge;
begin
  case ConnectionKind of
    //cNone: ;
    cSerial:
      begin
        if (serport.instanceactive) and (serport.CTS) then
        begin
          SerPort.Purge;

        end;
      end;
    cTelNet:
      begin
        TelNetClient.Sock.Purge; //?????? { TODO 1 -cBug : check }
      end;

    cVXI:
      begin

      end
  end
end;

function tConnectionForm.RecvString: string;
begin
  try
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

          case SerPort.LastError of
            0: ;
            ErrTimeOut: TimeOutErrors:= TimeOutErrors + 1;
            else
              WriteProgramLog('RecvString Error' + SerPort.LastErrorDesc);
          end;
        end;
      end;
    cTelNet:
      begin
        TelNetClient.Sock.RaiseExcept:= false;
        Result:= TelNetClient.RecvTerminated(CurrentDevice^.Terminator);
        TelNetClient.Sock.RaiseExcept:= true;

        writeprogramlog('Получена строка ' + Result);

        case TelNetClient.Sock.LastError of
          0: ;
          ErrTimeOut: TimeOutErrors:= TimeOutErrors + 1;
          else
            WriteProgramLog('RecvString Error' + TelNetClient.Sock.LastErrorDesc);
        end;
      end;

    cVXI:
      begin

      end
  end
  except
    on E: Exception do
      WriteProgramLog('RecvString exception: ' + E.Message);
  end;
end;

function tConnectionForm.RecvString(TimeOut: longword): string;
var
  t: longword;
begin
  case ConnectionKind of
    //cNone: ;
    cSerial:
      begin
        if (serport.instanceactive) and (serport.CTS) then
        begin
          SerPort.RaiseExcept:= false;
          Result:= SerPort.RecvString(Timeout);
          SerPort.RaiseExcept:= true;

          writeprogramlog('Получена строка ' + Result);

          case SerPort.LastError of
            0: ;
            ErrTimeOut: TimeOutErrors:= TimeOutErrors + 1;
            else
              WriteProgramLog('RecvString Error' + SerPort.LastErrorDesc);
          end;
        end;
      end;
    cTelNet:
      begin;
        t:= TelNetClient.Timeout;
        TelNetClient.Timeout:= TimeOut;;

        TelNetClient.Sock.RaiseExcept:= false;
        Result:= TelNetClient.RecvTerminated(CurrentDevice^.Terminator);
        TelNetClient.Sock.RaiseExcept:= true;

        TelNetClient.Timeout:= t;

        writeprogramlog('Получена строка ' + Result);

        case TelNetClient.Sock.LastError of
          0: ;
          ErrTimeOut: TimeOutErrors:= TimeOutErrors + 1;
          else
            WriteProgramLog('RecvString Error' + TelNetClient.Sock.LastErrorDesc);
        end;
      end
  end
end;

procedure tConnectionForm.SaveState(FileStream: tFileStream);
var
  i, j: integer;
  s: string;
begin
  if not Assigned(FileStream) then
    exit;
  s:= Name + '(' + LineEnding;
  FileStream.Write(s[1], length(s));
  for i:= 0 to ComponentCount - 1 do
  begin
    if Components[i] is TComboBox then
    with TComboBox(Components[i]) do
    begin
      s:= Name + '=' + strf(ItemIndex) + LineEnding;
      FileStream.Write(s[1], length(s));
    end
    else
    if Components[i] is TSpinEdit then
    with TSpinEdit(Components[i]) do
    begin
      s:= Name + '=' + strf(Value) + LineEnding;
      FileStream.Write(s[1], length(s));
    end
    else
    if Components[i] is TFloatSpinEdit then
    with TFloatSpinEdit(Components[i]) do
    begin
      s:= Name + '=' + strf(Value) + LineEnding;
      FileStream.Write(s[1], length(s));
    end
    else
    if Components[i] is TCheckBox then
    with TCheckBox(Components[i]) do
    begin
      s:= Name + '=' + strf(integer(Checked)) + LineEnding;
      FileStream.Write(s[1], length(s));
    end
    else
    if Components[i] is TCheckGroup then
    with TCheckGroup(Components[i]) do
    begin
      s:= Name + '=' + strf(Items.Count) + LineEnding;
      FileStream.Write(s[1], length(s));

      for j:= 0 to Items.Count - 1 do
      begin
        s:= '#' + strf(j) + '=' + strf(integer(Checked[j])) + LineEnding;
        FileStream.Write(s[1], length(s));
      end;
    end
    else
    if Components[i] is TPageControl then
    with TPageControl(Components[i]) do
    begin
      s:= Name + '=' + strf(TabIndex) + LineEnding;
      FileStream.Write(s[1], length(s));
    end
    else
    if Components[i] is TFileNameEdit then
    with TFileNameEdit(Components[i]) do
    begin
      s:= Name + '=' + Text + LineEnding;
      FileStream.Write(s[1], length(s));
    end;
  end;
  s:= ')' + LineEnding;
  FileStream.Write(s[1], length(s));
end;

function tConnectionForm.RestoreState(FileStream: tFileStream): integer;
var
  i, j, p, v, e: integer;
  d: double;
  StringStream: tStringStream;
  s: string;
begin
  if not Assigned(FileStream) then
    exit(-1);
  StringStream:= tStringStream.Create('');

  StringStream.CopyFrom(FileStream, 0);    //0 = whole

  p:= pos(Name, StringStream.DataString);

  s:= CopyFromTo(StringStream.DataString, p, '(', ')');
  //showmessage(s);
  Result:= 0;

  for i:= 0 to ComponentCount - 1 do
  begin
    if Components[i] is TComboBox then
    with TComboBox(Components[i]) do
    begin
      p:= pos(Name, s);
      if p = 0 then
      begin
        ItemIndex:= 0;
        continue;
      end;
      val(CopyFromTo(s, p, '=', LineEnding), v, e);
      if e = 0 then
        ItemIndex:= v
      else
        Result:= -2;
      if ItemIndex < 0 then
        ItemIndex:= 0;
    end
    else
    if Components[i] is TSpinEdit then
    with TSpinEdit(Components[i]) do
    begin
      p:= pos(Name, s);
      if p = 0 then
      begin
        Value:= 0;
        continue;
      end;
      val(CopyFromTo(s, p, '=', LineEnding), v, e);
      if e = 0 then
        Value:= v
      else
        Result:= -3;
    end
    else
    if Components[i] is TFloatSpinEdit then
    with TFloatSpinEdit(Components[i]) do
    begin
      p:= pos(Name, s);
      if p = 0 then
      begin
        Value:= 0;
        continue;
      end;
      val(CopyFromTo(s, p, '=', LineEnding), d, e);
      if e = 0 then
        Value:= d
      else
        Result:= -4;
    end
    else
    if Components[i] is TCheckBox then
    with TCheckBox(Components[i]) do
    begin
      p:= pos(Name, s);
      if p = 0 then
      begin
        Checked:= false;
        continue;
      end;
      val(CopyFromTo(s, p, '=', LineEnding), v, e);
      if e = 0 then
        Checked:= boolean(v)
      else
        Result:= -5;
    end
    else
    if Components[i] is TCheckGroup then
    with TCheckGroup(Components[i]) do
    begin
      p:= pos(Name, s);
      if p = 0 then
      begin
        //
        continue;
      end;
      val(CopyFromTo(s, p, '=', LineEnding), v, e);

      for j:= 0 to min(v - 1, Items.Count - 1) do
      begin
        p:= pos('#' + strf(j), s);
        val(CopyFromTo(s, p, '=', LineEnding), v, e);
        if e = 0 then
          Checked[j]:= boolean(v)
        else
          Result:= -6
      end;
    end
    else
    if Components[i] is TPageControl then
    with TPageControl(Components[i]) do
    begin
      p:= pos(Name, s);
      if p = 0 then
      begin
        TabIndex:= 0;
        continue;
      end;
      val(CopyFromTo(s, p, '=', LineEnding), v, e);
      if e = 0 then
        TabIndex:= v
      else
        Result:= -7;
    end
    else
    if Components[i] is TFileNameEdit then
    with TFileNameEdit(Components[i]) do
    begin
      p:= pos(Name, s);
      if p = 0 then
      begin
        Text:= '';
        continue;
      end;
      Text:= CopyFromTo(s, p, '=', LineEnding);
    end;
  end;
  StringStream.Destroy;
end;

function tConnectionForm.RestoreState(FileName: string): integer;
var
  FileStream: tFileStream;
begin
  FileStream:= TFilestream.Create(FileName, fmOpenRead);
  Result:= RestoreState(FileStream);
  FileStream.Destroy;
end;

procedure tConnectionForm.EnsureValidItemIndices;
var
  i: integer;
begin
  for i:= 0 to ComponentCount - 1 do
    begin
      if Components[i] is TComboBox then
      with TComboBox(Components[i]) do
      begin
        writeprogramlog(name, true);
        if ItemIndex < 0 then
          ItemIndex:= 0;
      end;
    end;
end;

end.

