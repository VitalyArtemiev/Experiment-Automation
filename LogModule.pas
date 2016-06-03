unit LogModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, AxisSource;

type

  eLogState = (lInActive, lActive, lPaused);

  { tLogModule }

  tLogModule = class
  private
    fOnAppendFileName: tNotifyEvent;
    fOnSaveLog: tNotifyEvent;
    fHeader: string;
    fStub: string;
    fTimeFormat: string;
    function GetAndResetHeader: string;
    procedure SetTimeFormat(AValue: string);
  protected//???
    FOnBeforeStart: tNotifyEvent;
    FOnContinue: tNotifyEvent;
    FOnPause: tNotifyEvent;
    FOnProcessBuffers: tNotifyEvent;
    FOnStop: tNotifyEvent;
    FState: eLogState;
    FOnStateChange, FOnStart: tNotifyEvent;
    ProcessedPoints: longword;
    StartTime, PauseTime, PauseLength: TDateTime;
    srcTime: TAxisSource;
    TimeCS: tRTLCriticalSection;
    CoordinateSources: array of TAxisSource;
    ExperimentLog: TFileStream;

    function GetTime: TDateTime;
    procedure SetState(AValue: eLogState);
  public
    Filename, Filepath: string;
    ReadingsThread: TThread;
    ThreadList: TThreadList;
    constructor Create;
    destructor Destroy; override;

    function CreateFile: integer;
    function SaveFile: integer;
    procedure Toggle;
    procedure Start;
    procedure Stop;
    procedure Pause;
    procedure Continue;
    procedure ProcessBuffers;

    property Header: string read GetAndResetHeader write fHeader;
    property Stub: string read fStub write fStub;
    property TimeFormat: string read fTimeFormat write SetTimeFormat;

    property OnBeforeStart: tNotifyEvent read FOnBeforeStart write FOnBeforeStart;
    property OnStart: tNotifyEvent read FOnStart write FOnStart;
    property OnPause: tNotifyEvent read FOnPause write FOnPause;
    property OnContinue: tNotifyEvent read FOnContinue write FOnContinue;
    property OnStop: tNotifyEvent read FOnStop write FOnStop;
    property OnAppendFileName: tNotifyEvent read fOnAppendFileName write fOnAppendFileName;
    property OnSaveLog: tNotifyEvent read fOnSaveLog write fOnSaveLog;
    property OnStateChange: tNotifyEvent read FOnStateChange write FOnStateChange;   //to manage interface
    property OnProcessBuffers: tNotifyEvent read FOnProcessBuffers write FOnProcessBuffers;
    property State: eLogState read FState write SetState;
    property ElapsedTime: TDateTime read GetTime;
  end;

implementation

uses
  SerConF;

const
  DefaultLogExtension = '.log';
  DefaultTimeFormat = 'yyyy_mm_dd';

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

constructor tLogModule.Create;
begin
  Threadlist:= TThreadList.Create;
  TimeFormat:= DefaultTimeFormat;
end;

destructor tLogModule.Destroy;
begin
  ThreadList.Free;
end;

function tLogModule.CreateFile: integer;
begin
  if Header = '' then
    Result:= 2
  else
    Result:= 0;

  DateTimeToString(FileName, TimeFormat, Now);    //check???

  if Stub <> '' then
    FileName+= '_' + Stub;

  if Assigned(OnAppendFileName) then
    OnAppendFileName(Self);

  if pos('.', Filename) = 0 then
    FileName+= DefaultLogExtension;
  //directoryexists
  FileName:= FilePath + FileName;

  try
    ExperimentLog:= TFileStream.Create(FileName, fmCreate)
  except
    on E:Exception do
    begin
      showmessage('Файл ' + FileName + ' не создан: ' + E.Message);
      exit(1);
    end;
  end;

  ExperimentLog.Write(Header[1], length(Header));
end;

function tLogModule.SaveFile: integer;
begin
  if Assigned(OnSaveLog) then
    OnSaveLog(Self);

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

  if State <> lInActive then
  begin
   if assigned(ReadingsThread) then
     ReadingsThread.WaitFor;
   ProcessBuffers;

   WriteProgramLog('Сохранение - результат: ' + strf(SaveFile));

   State:= lInActive;
   freeandnil(ExperimentLog);
   freeandnil(ReadingsThread);
  end;
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
  {btStartPauseLog.Caption:= 'Приостановить';
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
  ThreadList.LockList;
  if Assigned(OnProcessBuffers) then
    OnProcessBuffers(Self);
  ThreadList.UnlockList;
end;

end.

