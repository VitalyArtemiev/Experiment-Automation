unit LogModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, AxisSource;

type

  eLogState = (lInActive, lActive, lPaused);

  tLogModule = class;

  tLogEvent = procedure(Sender: tLogModule) of object;
  tLogEventEC = function(Sender: tLogModule): integer of object;   //Error code

  { tLogModule }

  tLogModule = class
  private
    fOnCreateFile: TLogEvent;
    fOnSaveLog: TLogEventEC;
    fHeader: string;
    fStub: string;
    fTimeFormat: string;
    function GetAndResetHeader: string;
    procedure SetTimeFormat(AValue: string);
  protected//???
    FOnBeforeStart: TLogEvent;
    FOnContinue: TLogEvent;
    FOnPause: TLogEvent;
    FOnProcessBuffers: TLogEvent;
    FOnStop: TLogEvent;
    FState: eLogState;
    FOnStateChange, FOnStart: TLogEvent;
    StartTime, PauseTime, PauseLength: TDateTime;
    TimeCS: tRTLCriticalSection;

    function GetTime: TDateTime;
    procedure SetState(AValue: eLogState);
  public
    ExperimentLog: TFileStream;
    //Data arrays
    CoordinateSources: array of TAxisSource;
    srcTime: tAxisSource;                                                       //not necessarily time, maybe # of point
    //Points read by the working thread, points passed to data arrays
    ReadPoints, ProcessedPoints: longword;
    //Set FilePath in OnCreateFile
    Filename, FilePath: string;
    //Assign the working thread to this
    ReadingsThread: TThread;
    //Pass data through this
    ThreadList: TThreadList;
    DataList: TList;

    function FullLogDir: string; inline;

    constructor Create;
    destructor Destroy; override;
    //Methods you should call
    function CreateFile: integer;
    function SaveFile: integer;
    procedure Toggle;                                                           //convenient for start/pause buttons
    procedure Start;
    procedure Stop;
    procedure Pause;
    procedure Continue;
    procedure ProcessBuffers;
    procedure Clear;
    //Set this in OnCreateFile, as well as FilePath
    property TimeFormat: string read fTimeFormat write SetTimeFormat;           //this is always included into file name
    property Stub: string read fStub write fStub;                               //this is added to file name
    property Header: string read GetAndResetHeader write fHeader;               //this is written at the top of output file, it is reset every log automatically
    //Events you should assign
    property OnBeforeStart: TLogEvent read FOnBeforeStart write FOnBeforeStart; //apply settings, init data arrays
    property OnStart: TLogEvent read FOnStart write FOnStart;                   //start threads
    property OnPause: TLogEvent read FOnPause write FOnPause;
    property OnContinue: TLogEvent read FOnContinue write FOnContinue;
    property OnStop: TLogEvent read FOnStop write FOnStop;
    property OnCreateFile: TLogEvent read fOnCreateFile write fOnCreateFile;    //here you can modify path, timeformat, stub, extension and header
    property OnSaveLog: TLogEventEC read fOnSaveLog write fOnSaveLog;
    property OnStateChange: TLogEvent read FOnStateChange write FOnStateChange; //to manage interface
    property OnProcessBuffers: TLogEvent read FOnProcessBuffers write FOnProcessBuffers;//receive data from threads and pass to data arrays
    property State: eLogState read FState write SetState;
    property ElapsedTime: TDateTime read GetTime;
  end;

const
  DefaultLogFolder = 'Data';
  DefaultLogExtension = '.log';
  DefaultTimeFormat = 'yyyy_mm_dd';

implementation

uses
  SerConF;

{ tLogModule }

function tLogModule.GetAndResetHeader: string;
begin
  Result:= fHeader;
  fHeader:= '';
end;

procedure tLogModule.SetTimeFormat(AValue: string);
begin
  try
    FormatDateTime(AValue, now);
  except
    on e: Exception do
    begin
      ShowMessage('Неверный формат даты');
      exit
    end;
  end;
  fTimeFormat:= AValue;
end;

function tLogModule.GetTime: TDateTime;
begin
  Result:= Now - StartTime - PauseLength;
end;

procedure tLogModule.SetState(AValue: eLogState);
begin
  FState:= AValue;
  if Assigned(OnStateChange) then
    onStateChange(Self);
end;

function tLogModule.FullLogDir: string;
begin
  if FilePath <> '' then
    Result:= GetCurrentDir + '\' + FilePath + '\'
  else
    Result:= GetCurrentDir + '\';
end;

constructor tLogModule.Create;
begin
  State:= lInActive;
  Threadlist:= TThreadList.Create;
  TimeFormat:= DefaultTimeFormat;
  FilePath:= DefaultLogFolder;
end;

destructor tLogModule.Destroy;
begin
  Clear;
  freeandnil(ThreadList);
end;

function tLogModule.CreateFile: integer;
var
  s: string;   //crutch bc cant do header[1]
begin
  if Header = '' then
    Result:= 2
  else
    Result:= 0;

  DateTimeToString(FileName, TimeFormat, Now);    //check???

  if Assigned(OnCreateFile) then
    OnCreateFile(Self);

  if Stub <> '' then
    FileName:= Stub + '_' + FileName;

  if pos('.', Filename) = 0 then
    FileName+= DefaultLogExtension;

  if not DirectoryExists(FullLogDir) then
    if not CreateDir(FullLogDir) then
    begin
      WriteProgramLog('Could not create path');
      FilePath:= '';
    end;

  if FilePath <> '' then
    FileName:= FilePath + '\' + FileName;

  try
    ExperimentLog:= TFileStream.Create(FileName, fmCreate)
  except
    on E:Exception do
    begin
      showmessage('Файл ' + FileName + ' не создан: ' + E.Message);
      exit(1);
    end;
  end;
  s:= Header;
  if s <> '' then
    ExperimentLog.Write(s[1], length(s));
end;

function tLogModule.SaveFile: integer;
begin
  if Assigned(OnSaveLog) then
    Result:= OnSaveLog(Self);

  freeandnil(ExperimentLog);
end;

procedure tLogModule.Toggle;
begin
  case State of
    lInActive: Start;
    lActive:   Pause;
    lPaused:   Continue;
  end;
end;

procedure tLogModule.Start;
begin
  if State = lActive then
    Stop;

  if Assigned(OnBeforeStart) then
    OnBeforeStart(Self);

  {btStartPauseLog.Caption:= 'Приостановить';  //to an event  onbeforestart
  WriteProgramLog('Data collection start');
  btClearClick(Self);

  if Finished or not ReadingsForm.ParamsApplied then  //skip if stepf is already going
  begin
    Cursor:= crHourGlass;
    ReadingsForm.btApplyClick(Self);
    sleep(ReadingsForm.eDelay.Value);
  end;
  Cursor:= crDefault;

  ReadingMode:= eReadMode(cbReadingsMode.ItemIndex);

  case ReadingMode of
    rSimultaneous:
    begin
      if LogFreq and (not cgTransfer.Checked[ReferenceIndex]) and (not UseGenFreq) then    //when logfreq set by mainform
      begin
        if cgTransfer.CheckEnabled[ReferenceIndex] then
          cgTransfer.Checked[ReferenceIndex]:= true
        else
        begin
          ShowMessage('Необходимо логирование опорной чатоты');
          WriteProgramLog('Data collection could not start: not logging ref');
          btStartPauseLog.Caption:= 'Начать снятие';
          exit;
        end;
      cgTransferItemClick(Self, -1);
      end;
      if not cgTransfer.Enabled then
      begin
        ShowMessage('Необходимо логирование опорной чатоты');
        WriteProgramLog('Data collection could not start: not logging ref');
        btStartPauseLog.Caption:= 'Начать снятие';
        exit;
      end;

      LogTime:= true;

      if cgTransfer.Checked[ReferenceIndex] and (not LogFreq)
        then LogFreq:= true;

      j:= 0;
      with cgTransfer do
      begin
        for i:= 0 to high(ReadPars) do
        begin
          ReadPars[i]:= false;
          Params.TransferPars[i]:= false;
          if Checked[i] then
          begin
            ReadPars[i]:= true;
            Params.TransferPars[i]:= true;
            ParToRead[j]:= i;
            inc(j);
          end;
        end;
      end;

      for i:= j to high(ParToRead) do
        ParToRead[i]:= -1;

      setlength(CoordinateSources, cgTransfer.Items.Count);

      for j:= 0 to high(ParToRead) do
      begin
        if ParToRead[j] < 0 then
          break;
        CoordinateSources[ParToRead[j]]:= TAxisSource.Create(128);
      end;
      if CH1Index >= 0 then
        srcCH1:= CoordinateSources[CH1Index];
      if CH2Index >= 0 then
        srcCH2:= CoordinateSources[CH2Index];
    end;
    rBuffer:
    begin
      LogTime:= true;  //???
      if LogFreq then
        cbUseGenFreq.Checked:= true;
      cbUseGenFreqChange(Self);
      setlength(CoordinateSources, 2);
      CoordinateSources[0]:=  TAxisSource.Create(128);
      CoordinateSources[1]:=  TAxisSource.Create(128);
      srcCH1:= CoordinateSources[0];
      srcCH2:= CoordinateSources[1];
    end;
  end;

  if LogTime then
    srcTime:= TAxisSource.Create(128);
  if LogFreq and UseGenFreq then
    srcFreq:= TAxisSource.Create(128);
  if LogAmpl then
    srcAmpl:= TAxisSource.Create(128);

  if ReadingMode = rBuffer then
  begin
    EnterCriticalSection(CommCS);
      AddCommand(dStartStorage);
      PassCommands;
    LeaveCriticalSection(CommCS);
    cbChart1Show.ItemIndex:= cbChart1Show.Items.IndexOf(cbCH1.Text);
    cbChart2Show.ItemIndex:= cbChart2Show.Items.IndexOf(cbCH2.Text);
  end;

  if SampleRate <> 0 then
    TimeStep:= 1/SampleRate
  else
    TimeStep:= 1;
  UpdateTimer.Interval:= eUpdateInterval.Value;
  UpdateTimer.Enabled:= true;

  t:= 0;
  ReadPoints:= 0;
  }

  State:= lActive;
  CreateFile;
  PauseLength:= 0;
  ReadPoints:= 0;
  ProcessedPoints:= 0;
  StartTime:= Now;

  if Assigned(OnStart) then
    OnStart(Self);
  {case ReadingMode of   //onstart
    rBuffer:
      ReadingsThread:= tBufferThread.Create(PointsInBuffer);
    rSimultaneous:
    begin
      if OnePointPerStep then
        ReadingsThread:= nil //Assigned in stepform
      else
        ReadingsThread:= tSimultaneousThread.Create;
    end;
  end;   }
end;

procedure tLogModule.Stop;
begin
  if State = lInActive then
    exit;

  WriteProgramLog('Data collection end');

  if assigned(ReadingsThread) then
    ReadingsThread.Terminate;

  if Assigned(OnStop) then
    OnStop(Self);

  {UpdateTimer.Enabled:= false;    to event    OnStop

  if (ReadingMode = rBuffer) or (Force and (LogState = lInActive)) then
  begin
    EnterCriticalSection(CommCS);
      AddCommand(dResetStorage);
      PassCommands;
    LeaveCriticalSection(CommCS);
  end;

  OnePointPerStep:= false;


  if LogState <> lInActive then
  begin
    if Config.AutoExportParams then
       MainForm.ExportParams(false, ReportHeader);
    inc(ExperimentNumber);
  end;
  btStartPauseLog.Caption:= 'Начать снятие';
  //btApply.Enabled:= true;
  }

  if assigned(ReadingsThread) then
    ReadingsThread.WaitFor;
  ProcessBuffers;

  WriteProgramLog('Сохранение - результат: ' + strf(SaveFile));

  State:= lInActive;
  freeandnil(ExperimentLog);
  freeandnil(ReadingsThread);
end;

procedure tLogModule.Pause;
begin
  State:= lPaused;
  if assigned(ReadingsThread) then
    ReadingsThread.Terminate;
  PauseTime:= Now;

  if Assigned(OnPause) then
    OnPause(Self);

  {btStartPauseLog.Caption:= 'Продолжить';  //event onpause
  WriteProgramLog('Data collection pause');

  UpdateTimer.Enabled:= false;

  if ReadingMode = rBuffer then
  begin
    EnterCriticalSection(CommCS);
      AddCommand(dPauseStorage);
      PassCommands;
    LeaveCriticalSection(CommCS);
  end; }

  if assigned(ReadingsThread) then
    ReadingsThread.WaitFor;
  ProcessBuffers;
  freeandnil(ReadingsThread);
end;

procedure tLogModule.Continue;
begin
  {btStartPauseLog.Caption:= 'Приостановить';  //oncontinue
  WriteProgramLog('Data collection resume');

  UpdateTimer.Enabled:= true;

  if ReadingMode = rBuffer then
  begin
    EnterCriticalSection(CommCS);
      AddCommand(dStartStorage);
      PassCommands;
    LeaveCriticalSection(CommCS);
  end;

   case ReadingMode of
    rBuffer:
      ReadingsThread:= tBufferThread.Create(PointsInBuffer);
    rSimultaneous:
    begin
      if OnePointPerStep then
        ReadingsThread:= nil //
      else
        ReadingsThread:= tSimultaneousThread.Create;
    end;
  end; }
  if Assigned(OnContinue) then
    OnContinue(Self);
  State:= lActive;
  PauseLength+= Now - PauseTime;
end;

procedure tLogModule.ProcessBuffers;
begin
  DataList:= ThreadList.LockList;
    if Assigned(OnProcessBuffers) then
      OnProcessBuffers(Self);
  ThreadList.UnlockList;
end;

procedure tLogModule.Clear;
var
  i: Integer;
begin
  if State <> lInActive then
    exit;

  if Assigned(ThreadList) then
  begin
    DataList:= ThreadList.LockList;
      Datalist.Clear;
    ThreadList.UnlockList;
  end;

  ProcessedPoints:= 0;

  freeandnil(srcTime);
  for i:= 0 to high(CoordinateSources) do
    freeandnil(CoordinateSources[i]);
end;

end.

