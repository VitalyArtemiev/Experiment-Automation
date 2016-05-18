unit ReadingsF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TASources, TATools,
  TATransformations, TADbSource, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, Spin, PairSplitter, Buttons, ComCtrls, synaser,
  MainF, SerConF, ReadingThreads, TACustomSource, AxisSource;

type
  Buffer = array of double;
  pBuffer = ^Buffer;

  eLogState = (lInActive, lActive, lPaused);

  eReadMode = (rBuffer, rSimultaneous);

  { TReadingsForm }

  tReadingsForm = class(TSerConnectForm)
    btAutoPhase: TButton;
    btAutoRange: TButton;
    btAutoReserve1: TButton;
    btAutoReserve2: TButton;
    btAutoSensitivity: TButton;
    btStartPauseLog: TButton;
    btStopLog: TButton;
    btClear: TButton;
    btOffset: TButton;

    cbChart1Show: TComboBox;
    cbChart2Show: TComboBox;
    cbInputRange: TComboBox;
    cbReserve1: TComboBox;
    cbReserve2: TComboBox;
    cbSampleRate: TComboBox;
    cbSensitivity: TComboBox;
    cbShowPoints: TCheckBox;

    cbCh1: TComboBox;
    cbCh2: TComboBox;
    cbTimeConstant: TComboBox;
    cbXAxis: TComboBox;
    cbReadingsMode: TComboBox;
    cgTransfer: TCheckGroup;
    cbUseGenFreq: TCheckBox;
    cbRatio1: TComboBox;
    cbRatio2: TComboBox;
    ChartToolset1DataPointHintTool1: TDataPointHintTool;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    ChartToolset1ZoomMouseWheelTool1: TZoomMouseWheelTool;

    eAxisLimit: TFloatSpinEdit;
    eDelay: TSpinEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label29: TLabel;

    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;

    Label9: TLabel;

    GraphSplitter: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    eUpdateInterval: TSpinEdit;

    Chart1: TChart;
    Chart2: TChart;

    Chart1LineSeries1: TLineSeries;
    Chart2LineSeries1: TLineSeries;

    ChartToolset1: TChartToolset;
    pnGraphControl: TPanel;
    pnConnection: TPanel;
    sbParamScroll: TScrollBox;
    sbSettingScroll: TScrollBox;

    Source1: TUserDefinedChartSource;
    Source2: TUserDefinedChartSource;
    UpdateTimer: TTimer;

    procedure btAutoPhaseClick(Sender: TObject);
    procedure btAutoRangeClick(Sender: TObject);
    procedure btAutoReserve1Click(Sender: TObject);
    procedure btAutoReserve2Click(Sender: TObject);
    procedure btAutoSensitivityClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject); override;
    procedure btApplyClick(Sender: TObject);
    procedure btOffsetClick(Sender: TObject);
    procedure btQueryClick(Sender: TObject); override;
    procedure btStartPauseLogClick(Sender: TObject);
    procedure btStatusClick(Sender: TObject);
    procedure btStopLogClick(Sender: TObject);
    procedure cbChart1ShowChange(Sender: TObject);
    procedure cbChart2ShowChange(Sender: TObject);
    procedure cbReadingsModeChange(Sender: TObject);
    procedure ParamsChange(Sender: TObject);
    procedure cbShowPointsChange(Sender: TObject);
    procedure cbUseGenFreqChange(Sender: TObject);
    procedure cbXAxisChange(Sender: TObject);
    procedure cgTransferItemClick(Sender: TObject; Index: integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GraphSplitterResize(Sender: TObject);
    procedure Source1GetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure Source2GetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure UpdateTimerTimer(Sender: TObject);

  private
    { private declarations }
    ReportHeader: boolean;
    RNCS, TimeCS, OPSCS, RTDCS: TRTLCriticalSection;
    FLogState: eLogState;
    DrawnBuffers: longword;//???
    t, SampleRate: double;
    ReadingMode: eReadMode;
    MaxSimultPars, TotalPars,
    FirstIndex, ReferenceIndex, CH1Index, CH2Index: shortint;
    PointsInBuffer, MinDelay: longword;

    ReadPars: array of boolean;
    ProcessedPoints: longword;
    StartTime, PauseTime, PauseLength: TDateTime;

    CoordinateSources: array of TAxisSource;
    srcTime, srcFreq, srcAmpl, srcCH1, srcCH2: TAxisSource;

    procedure SetLogState(AValue: eLogState);

    function GetTime: TDateTime;
  public
    { public declarations }
    ParToRead: array of shortint;
    ThreadList: TThreadList;
    LogTime, LogFreq, LogAmpl, UseGenFreq, OnePointPerStep, OffsetTracked: boolean;
    TimeStep: double;

    CurrLogFileName: string;
    ReadPoints: longint;
    property LogState: eLogState read FLogState write SetLogState;
    property ElapsedTime: TDateTime read GetTime;

    procedure EnableControls(Enable: boolean); override;

    procedure BeginLog;
    procedure PauseLog;
    procedure ContinueLog;
    procedure StopLog(Force: boolean = false);
    function CreateLog: string;
    function SaveLog: integer;
    function RecvSnap(p: array of shortint): PBuffer;
    procedure ProcessBuffers;
  end;

var
  ReadingsForm: TReadingsForm;
  ReadingsThread: TThread;


implementation

uses Dateutils, StrUtils, stepf, optionf, DeviceF, OffsetF;

procedure tReadingsForm.Source1GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
var
  i: word;
  Src: TAxisSource;
begin
  case cbXAxis.ItemIndex of
    0: if LogTime then AItem.X:= srcTime.Values[Aindex];
    1: if LogFreq then
      begin
         if UseGenFreq then
           AItem.X:= srcFreq.Values[Aindex]
         else AItem.X:= CoordinateSources[ReferenceIndex].Values[Aindex];
      end;
    2: if LogAmpl then AItem.X:= srcAmpl.Values[Aindex];
  end;
                    { TODO 3 -cFeature : show gen freq? }
  if (ReadingMode = rBuffer) then
    i:= 0
  else
    i:= cbChart1Show.ItemIndex;

  Src:= CoordinateSources[i];
  try
  if Src <> nil then
    AItem.Y:= Src.Values[Aindex]
  else Source1.PointsNumber:= 0;
  except
     on e:exception do writeprogramlog(e.message +' '+ strf(aindex)+' ' + strf(Source2.PointsNumber) +' '+ strf(src.count))
  end;
end;

procedure tReadingsForm.Source2GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
var
  i: word;
  Src: TAxisSource;
begin
  case cbXAxis.ItemIndex of
    0: if LogTime then AItem.X:= srcTime.Values[Aindex];
    1: if logFreq then
       begin
         if UseGenFreq then
           AItem.X:= srcFreq.Values[Aindex]
         else AItem.X:= CoordinateSources[ReferenceIndex].Values[Aindex];
       end;
    2: if LogAmpl then AItem.X:= srcAmpl.Values[Aindex];
  end;

  if ReadingMode = rBuffer then
    i:= 1
  else
    i:= cbChart2Show.ItemIndex;

  Src:= CoordinateSources[i];
  try
  if Src <> nil then
    AItem.Y:= Src.Values[Aindex]
  else Source2.PointsNumber:= 0;
  except
     on e:exception do writeprogramlog(e.message +' '+ strf(aindex)+' ' + strf(Source2.PointsNumber) +' '+ strf(src.count))
  end;
end;

procedure tReadingsForm.FormCreate(Sender: TObject);
var
  i: longint;
begin
  WriteProgramLog('Creating Readings form');

  Top:= MainForm.Top;
  Left:= Screen.Width div 2;

  btQuery.Caption:= 'Запрос' + LineEnding + 'текущих' + LineEnding + 'значений';
  btReset.Caption:= 'Сбросить' + LineEnding + '‌настройки'+ LineEnding + 'прибора';
  btCustomCommand.Caption:= 'Польз.' + LineEnding + 'команда';
  btStatus.Caption:= 'Состояние' + LineEnding + ' прибора';

  MaxSimultPars:= -1;
  DeviceKind:= dDetector;
  DeviceIndex:= iDefaultDevice;
  ConnectionKind:= cNone;
  DrawnBuffers:= 0;
  t:= 0;

  LogFreq:= false;
  LogTime:= true;
  LogAmpl:= false;

  for i:= 0 to PortCount - 1 do
    cbPortSelect.AddItem(MainForm.cbPortSelect.Items[i], nil);

  if cbPortSelect.ItemIndex < 0 then cbPortSelect.ItemIndex:= 0;

  if PortCount = 1 then
    StatusBar.Panels[spStatus].Text:= 'Нет доступных COM-портов';

  Threadlist:= TThreadList.Create;
  InitCriticalSection(CommCS);
  InitCriticalSection(RNCS);
  InitCriticalSection(TimeCS);
  InitCriticalSection(OPSCS);
  InitCriticalSection(RTDCS);
end;

procedure tReadingsForm.FormDestroy(Sender: TObject);
begin
  if LogState <> lInActive then StopLog;
  btClearClick(Self);
  SerPort.Free;
  TelNetClient.Free;
  ThreadList.Free;
  DoneCriticalSection(CommCS);
  DoneCriticalSection(RNCS);
  DoneCriticalSection(TimeCS);
  DoneCriticalSection(OPSCS);
  DoneCriticalSection(RTDCS);
end;

procedure tReadingsForm.FormShow(Sender: TObject);
begin
  GetSupportedDevices(DeviceKind);
  if cbRatio1.ItemIndex < 0 then
    cbRatio1.ItemIndex:= 0;
  if cbRatio2.ItemIndex < 0 then
    cbRatio2.ItemIndex:= 0;
end;

procedure tReadingsForm.GraphSplitterResize(Sender: TObject);
begin
  with GraphSplitter do
    Position:= Width div 2;
end;

procedure tReadingsForm.EnableControls(Enable: boolean);    //+++
begin
  btOffset.Enabled:=          Enable;
  btApply.Enabled:=           Enable;
  btStartPauseLog.Enabled:=   Enable;
  btStopLog.Enabled:=         Enable;
  btCustomCommand.Enabled:=   Enable;
  btReset.Enabled:=           Enable;
  btStatus.Enabled:=          Enable;
  btQuery.Enabled:=           Enable;
  cbReadingsMode.Enabled:=    Enable;
  cbUseGenFreq.Enabled:=      Enable;
  cbSampleRate.Enabled:=      Enable;
  cbTimeConstant.Enabled:=    Enable;
  cbSensitivity.Enabled:=     Enable;
  cbReserve1.Enabled:=        Enable;
  cbReserve2.Enabled:=        Enable;
  cbInputRange.Enabled:=      Enable;
  btAutoPhase.Enabled:=       Enable;
  btAutoSensitivity.Enabled:= Enable;
  btAutoReserve1.Enabled:=    Enable;
  btAutoReserve2.Enabled:=    Enable;
  btAutoRange.Enabled:=       Enable;
  cbUseGenFreq.Enabled:=      Enable;
  cbCh1.Enabled:=             Enable;
  cbCh2.Enabled:=             Enable;
  eDelay.Enabled:=            Enable;
end;

procedure tReadingsForm.UpdateTimerTimer(Sender: TObject);
begin
  ProcessBuffers;
end;

procedure tReadingsForm.SetLogState(AValue: eLogState);
begin
  FLogState:= AValue;
  if FLogState = lActive then
  begin
    cbCh1.Enabled:=             false;
    cbCh2.Enabled:=             false;
    cbRatio1.Enabled:=          false;
    cbRatio2.Enabled:=          false;
    cbReadingsMode.Enabled:=    false;
    cbSampleRate.Enabled:=      false;
    cbTimeConstant.Enabled:=    false;
    cbSensitivity.Enabled:=     false;
    cbReserve1.Enabled:=        false;
    cbReserve2.Enabled:=        false;
    cbInputRange.Enabled:=      false;
    btAutoPhase.Enabled:=       false;
    btAutoSensitivity.Enabled:= false;
    btAutoReserve1.Enabled:=    false;
    btAutoReserve2.Enabled:=    false;
    btAutoRange.Enabled:=       false;
    cgTransfer.Enabled:=        false;
    cbUseGenFreq.Enabled:=      false;
    seRecvTimeout.Enabled:=     false;
    btClear.Enabled:=           false;
    btApply.Enabled:=           false;
  end
  else
  begin
    cbCh1.Enabled:=             true;
    cbCh2.Enabled:=             true;
    cbRatio1.Enabled:=          true;
    cbRatio2.Enabled:=          true;
    cbReadingsMode.Enabled:=    true;
    cbSampleRate.Enabled:=      true;
    cbTimeConstant.Enabled:=    true;
    cbSensitivity.Enabled:=     true;
    cbReserve1.Enabled:=        true;
    cbReserve2.Enabled:=        true;
    cbInputRange.Enabled:=      true;
    btAutoPhase.Enabled:=       true;
    btAutoSensitivity.Enabled:= true;
    btAutoReserve1.Enabled:=    true;
    btAutoReserve2.Enabled:=    true;
    btAutoRange.Enabled:=       true;
    cgTransfer.Enabled:=        true;
    cbUseGenFreq.Enabled:=      true;
    seRecvTimeout.Enabled:=     true;
    btApply.Enabled:=           true;
    if FLogState = lInActive then
      btClear.Enabled:= true;
  end;
end;

function tReadingsForm.GetTime: TDateTime;
begin
  EnterCriticalSection(TimeCS);
  try
    Result:= Now - StartTime - PauseLength;
  finally
    LeaveCriticalSection(TimeCS);
  end;
end;

procedure tReadingsForm.BeginLog;
var
  i, j: integer;
begin
  if LogState = lActive then StopLog;
  btStartPauseLog.Caption:= 'Приостановить';
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
        if ParToRead[j] < 0 then break;
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
      if LogFreq then cbUseGenFreq.Checked:= true;
      cbUseGenFreqChange(Self);
      setlength(CoordinateSources, 2);
      CoordinateSources[0]:=  TAxisSource.Create(128);
      CoordinateSources[1]:=  TAxisSource.Create(128);
      srcCH1:= CoordinateSources[0];
      srcCH2:= CoordinateSources[1];
    end;
  end;

  if LogTime then srcTime:= TAxisSource.Create(128);
  if LogFreq and UseGenFreq then srcFreq:= TAxisSource.Create(128);
  if LogAmpl then srcAmpl:= TAxisSource.Create(128);

  if ReadingMode = rBuffer then
  begin
    EnterCriticalSection(CommCS);
      AddCommand(dStartStorage);
      PassCommands;
    LeaveCriticalSection(CommCS);
    cbChart1Show.ItemIndex:= cbChart1Show.Items.IndexOf(cbCH1.Text);
    cbChart2Show.ItemIndex:= cbChart2Show.Items.IndexOf(cbCH2.Text);
  end;

  t:= 0;
  ReadPoints:= 0;
  LogState:= lActive;
  CurrLogFileName:= CreateLog;

  if SampleRate <> 0 then
    TimeStep:= 1/SampleRate
  else
    TimeStep:= 1;
  UpdateTimer.Interval:= eUpdateInterval.Value;
  UpdateTimer.Enabled:= true;
  PauseLength:= 0;
  StartTime:= Now;

  case ReadingMode of
    rBuffer:
      ReadingsThread:= tBufferThread.Create(PointsInBuffer);
    rSimultaneous:
    begin
      if OnePointPerStep then
        ReadingsThread:= nil //Assigned in stepform
      else
        ReadingsThread:= tSimultaneousThread.Create;
    end;
  end;
end;

procedure tReadingsForm.PauseLog;
begin
  btStartPauseLog.Caption:= 'Продолжить';
  WriteProgramLog('Data collection pause');
  LogState:= lPaused;
  UpdateTimer.Enabled:= false;

  if ReadingMode = rBuffer then
  begin
    EnterCriticalSection(CommCS);
      AddCommand(dPauseStorage);
      PassCommands;
    LeaveCriticalSection(CommCS);
  end;

  ReadingsThread.Terminate;
  PauseTime:= Now;
  if assigned(ReadingsThread) then
    ReadingsThread.WaitFor;
  ProcessBuffers;
 // btApply.Enabled:= true;
end;

procedure tReadingsForm.ContinueLog;
begin
  btStartPauseLog.Caption:= 'Приостановить';
 // btApply.Enabled:= false;
  WriteProgramLog('Data collection resume');

  LogState:= lActive;
  UpdateTimer.Enabled:= true;
  ReadingsThread.Start;

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
  end;
  PauseLength+= Now - PauseTime;
end;

procedure tReadingsForm.StopLog(Force: boolean);
begin
  WriteProgramLog('Data collection end');

  if assigned(ReadingsThread) then
    ReadingsThread.Terminate;
  UpdateTimer.Enabled:= false;

  if (ReadingMode = rBuffer) or (Force and (LogState = lInActive)) then
  begin
    EnterCriticalSection(CommCS);
      AddCommand(dResetStorage);
      PassCommands;
    LeaveCriticalSection(CommCS);
  end;

  if LogState <> lInActive then
  begin
   if assigned(ReadingsThread) then
     ReadingsThread.WaitFor;
   ProcessBuffers;

   WriteProgramLog('Сохранение - результат: ' + strf(SaveLog));

   OnePointPerStep:= false;
   LogState:= lInActive;
   freeandnil(ExperimentLog);
   freeandnil(ReadingsThread);
   if Config.AutoExportParams then
     MainForm.ExportParams(false, ReportHeader);
   inc(ExperimentNumber);
  end;
  btStartPauseLog.Caption:= 'Начать снятие';
  //btApply.Enabled:= true;
end;

function tReadingsForm.CreateLog: string;
var
  FileName, s1, s2: string;
  i: integer;
begin
  Result:= '';
  DateTimeToString(FileName, 'yyyy_mm_dd', Now);    //check???

  if Config.AutoExportParams then
  begin
    if ReportNumber = 0 then
    begin
      inc(ReportNumber);
      str(ReportNumber, s1);
      while FileExists(FileName + '_' + s1 + '.txt') or
            FileExists(FileName + '_' + s1 + '_1' + '.log') do        //to cfg?
      begin
        inc(ReportNumber);
        str(ReportNumber, s1);
      end;
    end;

    ReportHeader:= true;
    str(ReportNumber, s1);
    str(ExperimentNumber, s2);
    FileName+= '_' + s1 + '_' + s2;
  end
  else
  begin
    str(ExperimentNumber, s1);
    while FileExists(FileName + '_' + s1 + '.log') do        //to cfg?
    begin
      inc(ExperimentNumber);
      str(ExperimentNumber, s1);
    end;

    ReportHeader:= false;
    FileName+= '_' + s1;
  end;

  FileName+= '.log';

  WriteProgramLog('Creating log file');
  WriteProgramLog(FileName);

  try
    ExperimentLog:= TFileStream.Create(FileName, fmCreate);
    s1:= '';
    if LogTime then s1+= cbXAxis.Items.Strings[0] + HT;
    if LogFreq and UseGenFreq then s1+= 'Частота (генератор)' + HT;
    if LogAmpl then s1+= 'Амплитуда (генератор)' + HT;

    case ReadingMode of
      rBuffer:
        s1+= cbCh1.Text + HT + cbCh2.Text;
      rSimultaneous:
      begin
        for i:= 0 to high(ParToRead) do
        begin
          if ParToRead[i] = -1 then break;
          if ParToRead[i] = CH1Index then
            s1+= 'Дисп.1 (' + cbCh1.Text + ')' + HT
          else
          if ParToRead[i] = CH2Index then
            s1+= 'Дисп.2 (' + cbCh2.Text + ')' + HT
          else
            s1+= cbChart1Show.Items[ParToRead[i]];
          s1+= HT;
        end;
      end;
    end;
    s1+= LineEnding;
    ExperimentLog.Write(s1[1], length(s1));
           //исключаем длину строки в [0]

  except
     on E:Exception do
       showmessage('Файл ' + FileName + ' не создан: ' + E.Message);
  end;

  Result:= FileName;
end;

function tReadingsForm.SaveLog: integer;
var
  i, j: integer;
  s: string;
  Sources: array of tAxisSource;
begin
  if IsEmptyStr(CurrLogFileName, [' ']) then exit(-1);

  if LogTime then
  begin
    setlength(Sources, length(Sources) + 1);
    Sources[high(Sources)]:= SrcTime;
  end;

  if LogFreq and UseGenFreq then
  begin
    setlength(Sources, length(Sources) + 1);
    Sources[high(Sources)]:= SrcFreq;
  end;

  if LogAmpl then
  begin
    setlength(Sources, length(Sources) + 1);
    Sources[high(Sources)]:= SrcAmpl;
  end;

  case ReadingMode of
    rBuffer:
    begin
      setlength(Sources, length(Sources) + 2);
      Sources[high(Sources) - 1]:= CoordinateSources[0];
      Sources[high(Sources)]:= CoordinateSources[1];
    end;
    rSimultaneous:
    begin
      for i:= 0 to cgTransfer.Items.Count - 1 do
        if ReadPars[i] then
        begin
          setlength(Sources, length(Sources) + 1);
          Sources[high(Sources)]:= CoordinateSources[i];
        end;
    end;
  end;

  Result:= length(Sources);

  for i:= 0 to ReadPoints do
  begin
    s:= '';
    for j:= 0 to high(Sources) do
      s+= strf(Sources[j].Values[i]) + HT;
    s+= LineEnding;
    if assigned(ExperimentLog) then
      ExperimentLog.Write(s[1], length(s))
    else
      exit(-2);
  end;
end;

function tReadingsForm.RecvSnap(p: array of shortint): PBuffer;
var
  num, i: integer;
  ParamArr: tIntegerArray; //CONSTRUCTOR!!!
  s: string;
begin
  if (p[0] < 0) or (p[1] < 0) then
  begin
    Result:= nil;
    exit;
  end;

  i:= 0;
  while i <= high(p) do
  begin
    if p[i] = -1 then break
    else inc(i);
  end;
  num:= i;
  setlength(ParamArr, num);

  WriteProgramLog(strf(num) + ' SNAP elements');

  for i:= 0 to high(ParamArr) do
    ParamArr[i]:= p[i] + FirstIndex;

  try
  EnterCriticalSection(CommCS);
    AddCommand(dReadSimultaneous, true, ParamArr);
    PassCommands;
    s:= RecvString;
  finally
    LeaveCriticalSection(CommCS);
    //WriteProgramLog('Error: ' + serport.lasterrordesc);
  end;

  //sleep(60 + random(30));
  //s:= strf(random)+',' +strf(random)+',' + strf(random);
  //writeprogramlog(s);
  if IsEmptyStr(s, [' ']) then
  begin
    Result:= nil;
    exit;
  end;

  new(Result);
  setlength(Result^, num);//MEM LEAK 2 blocks 24 and 4 still leaking +++ 3 blocks 8 32 4

  for i:= 0 to num - 2 do
  begin
    val(copy(s, 1, pos(CurrentDevice^.ParSeparator, s) - 1), Result^[i]);
    delete(s, 1, pos(CurrentDevice^.ParSeparator, s));
  end;
  val(s, Result^[i + 1]);
end;

procedure tReadingsForm.ProcessBuffers;
var
  i, j, k, lk: longint;
  l, c: longword;
  v, f, a: double;
  DataList: TList;
begin
  DataList:= ThreadList.LockList;

  if LogFreq and UseGenFreq then f:= StepF.F;
  if LogAmpl then a:= StepF.A;

  if DataList.Count > 0 then
  case ReadingMode of
    rBuffer:
    begin
      for i:= 0 to (DataList.Count div 2) - 1 do
      begin
        l:= length(Buffer(DataList.Items[i * 2]^));
        ProcessedPoints+= l;
        c:= srcCh1.Count;
        srcCH1.Add(Buffer(DataList.Items[i * 2]^));
        srcCH2.Add(Buffer(DataList.Items[i * 2 + 1]^));

        if LogFreq and UseGenFreq then
        begin
          srcFreq.Capacity:= srcFreq.Capacity + l;
          srcFreq.Count:= srcFreq.Count + l;
        end;

        if LogAmpl then
        begin
          srcAmpl.Capacity:= srcAmpl.Capacity + l;
          srcAmpl.Count:= srcAmpl.Count + l;
        end;

        if LogTime then
        begin
          srcTime.Capacity:= srcTime.Capacity + l;
          srcTime.Count:= srcTime.Count + l;
        end;

        l:= c + l - 1;

        for j:= c to l do
        begin
          if LogFreq and UseGenFreq then
            srcFreq.Values[j]:= f;

          if LogAmpl then
            srcAmpl.Values[j]:= a;

          srcTime.Values[j]:= t;
          t+= TimeStep;
        end;

        dispose(PBuffer(DataList.Items[i * 2]));
        dispose(PBuffer(DataList.Items[i * 2 + 1]));
      end;
        DataList.Clear;
    end;

    rSimultaneous:
    begin
      for i:= 0 to (DataList.Count div 2) - 1 do
      begin
        lk:= -1;

        if LogFreq and UseGenFreq then
          srcFreq.Add(f);

        if LogAmpl then
          srcAmpl.Add(a);

        t:= SecondSpan(TDateTime(DataList.Items[i * 2]^), 0);
        dispose(PDateTime(DataList.Items[i * 2]));
        if LogTime then
          srcTime.Add(t);

        for j:= 0 to high(Buffer(DataList.Items[i * 2 + 1]^)) do
        begin
          v:= Buffer(DataList.Items[i * 2 + 1]^)[j];

          //WriteProgramLog('');
          //WriteProgramLog('i ' + strf(i) + ' j ' + strf(j));

          for k:= (lk + 1) to high(ReadPars) do
          begin
            //WriteProgramLog('lk ' + strf(lk)+' k '+strf(k));
            if ReadPars[k] then
            begin
              //writeprogramlog('k' + strf(k));
              CoordinateSources[k].Add(v);
              lk:= k;
              break;
            end;
          end;
        end;

        dispose(PBuffer(DataList.Items[i * 2 + 1]));
      end;
      DataList.Clear;

      if LogTime then
        ProcessedPoints:= srcTime.Count
      else
      if LogFreq then
      begin
       if UseGenFreq then
         ProcessedPoints:= srcFreq.Count
       else ProcessedPoints:= CoordinateSources[ReferenceIndex].Count
      end
      else
      ProcessedPoints:= srcAmpl.Count;
    end;
  end;
  ThreadList.UnlockList;

  Source1.PointsNumber:= ProcessedPoints;
  Source2.PointsNumber:= ProcessedPoints;
  //writeprogramlog('c ' + strf(srctime.count) + ' ' + strf(srctime.capacity));
  //writeprogramlog('c ' + strf(srcref.count) + ' ' + strf(srcref.capacity));
  //writeprogramlog('c ' + strf(srcdisp1.count) + ' ' + strf(srcdisp1.capacity));
  //writeprogramlog('c ' + strf(srcdisp2.count) + ' ' + strf(srcdisp2.capacity));
      //Log+= 'Processed' + LineEnding;
      //ProgramLog.Write(Log[1], length(Log));
    {  except
     on E:Exception do
     begin
       WriteProgramLog(E.Message +'i' + strf(i) +'j' + strf(j)+'k' + strf(k)+'lk' + strf(lk)+'v' + strf(v));

  end;

      end;}
      WriteProgramLog('pb done');
  StatusBar.Panels[spStatus].Text:= 'Точек: ' + strf(ProcessedPoints);
end;

procedure tReadingsForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if LogState <> lInActive then StopLog;
  MainForm.miShowReadingsF.Checked:= false;
end;

procedure tReadingsForm.btStartPauseLogClick(Sender: TObject);
begin
  case LogState of
    lInActive: BeginLog;
    lActive:   PauseLog;
    lPaused:   ContinueLog;
  end;
end;

procedure tReadingsForm.btStatusClick(Sender: TObject);
begin
  inherited btStatusClick(Sender);
end;

procedure tReadingsForm.btnConnectClick(Sender: TObject);
var
  i, c : word;
  s: string;
begin
  if MainForm.cbPortSelect.ItemIndex = cbPortSelect.ItemIndex then
  begin
    if MainForm.ConnectionKind = cSerial then
    begin
      showmessage('К данному порту уже осуществляется подключение');
      exit
    end
    else
    if MainForm.ConnectionKind = cTelNet then
      showmessage('check ip');
       { TODO 2 -cImprovement : check ip }
  end;

  OptionForm.TabControl.TabIndex:= 2;
  EnterCriticalSection(CommCS);
    inherited btnConnectClick(Sender);
    AddCommand(dResetStorage);
    PassCommands;
  LeaveCriticalSection(CommCS);

  if DeviceIndex = iDefaultDevice then exit;

  Params.DetectorPort:= ReadingsForm.CurrentDevice^.Port;
  Params.LastDetector:= ReadingsForm.CurrentDevice^.Model;

  OptionForm.eDevice1.ItemIndex:= DeviceIndex - 1;

  cgTransfer.Items.Clear;
  cbCh1.Items.Clear;
  cbCh2.Items.Clear;
  cbRatio1.Items.Clear;
  cbRatio2.Items.Clear;
  cbChart1Show.Items.Clear;
  cbChart2Show.Items.Clear;
  cbSensitivity.Items.Clear;
  cbTimeConstant.Items.Clear;
  cbReserve1.Items.Clear;
  cbReserve2.Items.Clear;
  cbInputRange.Items.Clear;

  with DeviceForm.sgDetCommands do
  begin
    cgTransfer.Items.AddText(Cells[DeviceIndex, integer(hTransferParams)]);
    cbChart1Show.Items:= cgTransfer.Items;
    cbChart2Show.Items:= cgTransfer.Items;
    cbCh1.Items.AddText(Cells[DeviceIndex, integer(hCH1Options)]);
    cbCh2.Items.AddText(Cells[DeviceIndex, integer(hCH2Options)]);
    cbRatio1.Items.AddText(Cells[DeviceIndex, integer(hRatio1Options)]);
    cbRatio2.Items.AddText(Cells[DeviceIndex, integer(hRatio2Options)]);
    cbSensitivity.Items.AddText(Cells[DeviceIndex, integer(hSensitivityOptions)]);
    cbTimeConstant.Items.AddText(Cells[DeviceIndex, integer(hTimeConstOptions)]);
    cbReserve1.Items.AddText(Cells[DeviceIndex, integer(hCloseReserveOptions)]);
    cbReserve2.Items.AddText(Cells[DeviceIndex, integer(hWideReserveOptions)]);
    cbInputRange.Items.AddText(Cells[DeviceIndex, integer(hRangeOptions)]);

    MinDelay:= valf(Cells[DeviceIndex, integer(hMinDelay)]);
    eDelay.MinValue:= MinDelay;

    TotalPars:= cgTransfer.Items.Count;
    setlength(ReadPars, TotalPars);
    MaxSimultPars:= valf(Cells[DeviceIndex, integer(hMaxSimultPars)]);
    setlength(ParToRead, MaxSimultPars);

    FirstIndex:= valf(Cells[DeviceIndex, integer(hFirstIndex)]);
    PointsInBuffer:= valf(Cells[DeviceIndex, integer(hPointsInBuffer)]);
    s:= Cells[DeviceIndex, integer(hIndices)];
    i:= pos(',', s);
    if i = 0 then
      ReferenceIndex:= valf(s)
    else
    begin
      ReferenceIndex:= valf(copy(s, 1, i - 1));
      delete(s, 1, i);

      i:= pos(',', s);
      CH1Index:= valf(copy(s, 1, i - 1));
      delete(s, 1, i);

      CH2Index:= valf(s);
    end;
  end;

  if OffsetForm.GetParams > 0 then
    btOffset.Show
  else
    btOffset.Hide;

  with Params do
  begin
    cbSensitivity.ItemIndex:= Sensitivity;
    cbTimeConstant.ItemIndex:= TimeConstant;
    cbReserve1.ItemIndex:= CloseReserve;
    cbReserve2.ItemIndex:= WideReserve;
    cbInputRange.ItemIndex:= InputRange;
    cbCh1.ItemIndex:= Display1;
    cbCh2.ItemIndex:= Display2;
    cbRatio1.ItemIndex:= Ratio1;
    cbRatio2.ItemIndex:= Ratio2;
    cbChart1Show.ItemIndex:= Show1;
    cbChart2Show.ItemIndex:= Show2;
    eDelay.Value:= Delay;

    for i:= 0 to cgTransfer.Items.Count - 1 do
      cgTransfer.Checked[i]:= TransferPars[i];
  end;

  if cbRatio1.ItemIndex < 0 then
  begin
    cbRatio1.Hide;
    Label14.Hide;
    cbRatio2.Hide;
    Label15.Hide;
  end
  else
  begin
    cbRatio1.Show;
    Label14.Show;
    cbRatio2.Show;
    Label15.Show;
  end;

  if cbReserve1.ItemIndex < 0 then
  begin
    cbReserve1.Hide;
    btAutoReserve1.Hide;
    Label16.Hide;
  end
  else
  begin
    cbReserve1.Show;
    btAutoReserve1.Show;
    Label16.Show;
  end;

  if cbReserve2.ItemIndex < 0 then
  begin
    cbReserve2.Hide;
    btAutoReserve2.Hide;
    Label17.Hide;
  end
  else
  begin
    cbReserve2.Show;
    btAutoReserve2.Show;
    Label17.Show;
  end;

  if cbInputRange.ItemIndex < 0 then
  begin
    cbInputRange.Hide;
    btAutoRange.Hide;
    Label18.Hide;
  end
  else
  begin
    cbInputRange.Show;
    btAutoRange.Show;
    Label18.Show;
  end;

  c:= 0;
  for i:= 0 to cgTransfer.Items.Count - 1 do
    if cgTransfer.Checked[i] then inc(c);

  if c < 2 then
  begin
    cgTransfer.Checked[ReferenceIndex]:= true;
    if CH1Index >= 0 then
      cgTransfer.Checked[CH1Index]:= true;
    if (CH2Index >= 0) and (c + 2 < MaxSimultPars) then
      cgTransfer.Checked[CH2Index]:= true;
  end;
end;

procedure tReadingsForm.btClearClick(Sender: TObject);
var
  DataList: TList;
  i: integer;
begin
  DataList:= ThreadList.LockList;
    Datalist.Clear;
  ThreadList.UnlockList;

  Source1.PointsNumber:= 0;
  Source2.PointsNumber:= 0;
  ProcessedPoints:= 0;

  freeandnil(srcTime);
  freeandnil(srcFreq);
  freeandnil(srcAmpl);
  for i:= 0 to high(CoordinateSources) do
    freeandnil(CoordinateSources[i]);

  StatusBar.Panels[spStatus].Text:= '';
end;

procedure tReadingsForm.btAutoPhaseClick(Sender: TObject);
begin
  EnterCriticalSection(CommCS);
    AddCommand(dAutoPhase, false);
    PassCommands;
  LeaveCriticalSection(CommCS);
  //btQueryClick(Self);
end;

procedure tReadingsForm.btAutoSensitivityClick(Sender: TObject);
var
  SerPollSB: byte;
  s: string;
  st, l: tDateTime;
begin
  EnableControls(false);
  Cursor:= crHourGlass;
  EnterCriticalSection(CommCS);
    AddCommand(dAutoSensitivity, false);
    PassCommands;
  LeaveCriticalSection(CommCS);

  Sleep(MinDelay);

  l:= EncodeTime(0, 0, 20, 0);
  st:= Now;
  repeat
    AddCommand(cSerialPoll, true);
    PassCommands;
    s:= Recvstring(5000);
    val(s, SerPollSB);
    StatusBar.Panels[spStatus].Text:= BinStr(SerPollSB, 8);
    StatusBar.Update;
    if Now - st > l then
    begin
      ShowMessage('Устройство не ответило');
      break;
    end;
  until ((SerPollSB div %10) mod %10 = %1);  //interface ready bit

  btQueryClick(Self);
  EnableControls(true);
  Cursor:= crDefault;
  StatusBar.Panels[spStatus].Text:= '';
end;

procedure tReadingsForm.btAutoRangeClick(Sender: TObject);
begin
  EnterCriticalSection(CommCS);
    AddCommand(dAutoRange, false);
    PassCommands;
  LeaveCriticalSection(CommCS);
  btQueryClick(Self);
end;

procedure tReadingsForm.btAutoReserve1Click(Sender: TObject);
begin
  EnterCriticalSection(CommCS);
    AddCommand(dAutoCloseReserve, false);
    PassCommands;
  LeaveCriticalSection(CommCS);
  btQueryClick(Self);
end;

procedure tReadingsForm.btAutoReserve2Click(Sender: TObject);
begin
   EnterCriticalSection(CommCS);
    AddCommand(dAutoWideReserve, false);
    PassCommands;
  LeaveCriticalSection(CommCS);
  btQueryClick(Self);
end;

procedure tReadingsForm.btApplyClick(Sender: TObject);
begin
  {if cbSampleRate.ItemIndex <> cbSampleRate.Items.Count - 1 then
    SampleRate:= (intpower(2, cbSampleRate.ItemIndex)) * 0.0625
  else SampleRate:= 0; }
  SampleRate:= 1; //no way to get real sample rate in a compatible way; SR865 needs to be polled
  with Params do
  begin
    GenFreq:= UseGenFreq;
    SampleRate:= cbSampleRate.ItemIndex;
    TimeConstant:= cbTimeConstant.ItemIndex;
    Sensitivity:= cbSensitivity.ItemIndex;
    CloseReserve:= cbReserve1.ItemIndex;
    WideReserve:= cbReserve2.ItemIndex;
    InputRange:= cbInputRange.ItemIndex;

    Display1:= cbCh1.ItemIndex;
    Display2:= cbCh2.ItemIndex;
    Ratio1:= cbRatio1.ItemIndex;
    Ratio2:= cbRatio2.ItemIndex;
    Show1:= cbChart1Show.ItemIndex;
    Show2:= cbChart2Show.ItemIndex;

    Delay:= eDelay.Value;
    UpdateInterval:= eUpdateInterval.Value;
    AxisLimit:= eAxisLimit.Value;
    XAxis:= cbXAxis.ItemIndex;
    ReadingsMode:= cbReadingsMode.ItemIndex;

    CurrentDevice^.Timeout:= seRecvTimeOut.Value; { TODO : sort out timeouts }

    EnterCriticalSection(CommCS);
    if cbRatio1.Visible then
    begin
      AddCommand(dDisplaySelect, false, tIntegerArray.Create(1, Display1, Ratio1));
      AddCommand(dDisplaySelect, false, tIntegerArray.Create(2, Display2, Ratio2));
    end
    else
    begin
      AddCommand(dDisplaySelect, false, tIntegerArray.Create(1, Display1));
      AddCommand(dDisplaySelect, false, tIntegerArray.Create(2, Display2));
    end;

      AddCommand(dSampleRate, false, SampleRate);
      AddCommand(dSensitivity, false, Sensitivity);
      AddCommand(dTimeConstant, false, TimeConstant);
      AddCommand(dReferenceSource, false, 0);   //external ref freq
      if cbReserve1.Visible then
        AddCommand(dCloseReserve, false, CloseReserve);
      if cbReserve2.Visible then
        AddCommand(dWideReserve, false, WideReserve);
      if cbInputRange.Visible then
        AddCommand(dInputRange, false, InputRange);
      PassCommands;
    LeaveCriticalSection(CommCS);
  end;
  ParamsApplied:= true;
end;

procedure tReadingsForm.btOffsetClick(Sender: TObject);
begin
  OffsetTracked:= true;
  OffsetForm.Show;
end;

procedure tReadingsForm.btQueryClick(Sender: TObject);
var
  s1, s2, s3: string;
  i, e: integer;
begin
  Purge;
  EnterCriticalSection(CommCS);
    AddCommand(dDisplaySelect, true, 1);
    AddCommand(dDisplaySelect, true, 2);
    AddCommand(dSampleRate, true);
    AddCommand(dSensitivity, true);
    AddCommand(dTimeConstant, true);
    if cbReserve1.Visible then
      AddCommand(dCloseReserve, true);
    if cbReserve2.Visible then
      AddCommand(dWideReserve, true);
    if cbInputRange.Visible then
      AddCommand(dInputRange, true);
    PassCommands;

    s1:= RecvString;
    s2:= RecvString;

    s3:= RecvString;
    val(s3, i, e);
    if e = 0 then cbSampleRate.ItemIndex:= i;

    s3:= RecvString;
    val(s3, i, e);
    if e = 0 then cbSensitivity.ItemIndex:= i;

    s3:= RecvString;
    val(s3, i, e);
    if e = 0 then cbTimeConstant.ItemIndex:= i;

    if cbReserve1.Visible then
    begin
      s3:= RecvString;
      val(s3, i, e);
      if e = 0 then cbReserve1.ItemIndex:= i;
    end;

    if cbReserve2.Visible then
    begin
      s3:= RecvString;
      val(s3, i, e);
      if e = 0 then cbReserve2.ItemIndex:= i;
    end;

    if cbInputRange.Visible then
    begin
      s3:= RecvString;
      val(s3, i, e);
      if e = 0 then cbInputRange.ItemIndex:= i;
    end;
  LeaveCriticalSection(CommCS);

  if cbRatio1.Visible then
  begin
    s3:= CurrentDevice^.ParSeparator;

    val(copy(s1, 1, pos(s3, s1) - 1), i, e);
    if e <> 0 then exit;
    cbCh1.ItemIndex:= i;
    delete(s1, 1, pos(s3, s1));

    val(s1, i);
    cbRatio1.ItemIndex:= i;

    val(copy(s2, 1, pos(s3, s2) - 1), i, e);
    if e <> 0 then exit;
    cbCh2.ItemIndex:= i;
    delete(s2, 1, pos(s3, s2));

    val(s2, i);
    cbRatio2.ItemIndex:= i;
  end
  else
  begin
    val(s1, i, e);
    if e = 0 then cbCh1.ItemIndex:= i;

    val(s2, i, e);
    if e = 0 then cbCh2.ItemIndex:= i;
  end;
end;

procedure tReadingsForm.btStopLogClick(Sender: TObject);
begin
  StopLog(true);
end;

procedure tReadingsForm.cbChart1ShowChange(Sender: TObject);
begin
  if (LogState = lActive) and (ReadingMode = rBuffer) then
  begin
    cbChart1Show.ItemIndex:= cbChart1Show.Items.IndexOf(cbCH1.Text);
    if cbChart1Show.ItemIndex < 0 then
      cbChart1Show.ItemIndex:= CH1Index;
  end;
  Source1.PointsNumber:= ProcessedPoints;
  Source1.Reset;
end;

procedure tReadingsForm.cbChart2ShowChange(Sender: TObject);
begin
  if (LogState = lActive) and (ReadingMode = rBuffer) then
  begin
    cbChart2Show.ItemIndex:= cbChart2Show.Items.IndexOf(cbCH2.Text);
    if cbChart2Show.ItemIndex < 0 then
      cbChart2Show.ItemIndex:= CH2Index;
  end;
  Source2.PointsNumber:= ProcessedPoints;
  Source2.Reset;
end;

procedure tReadingsForm.cbReadingsModeChange(Sender: TObject);
begin
  case CbReadingsMode.ItemIndex of
    integer(rSimultaneous):
    begin
     cgTransfer.Enabled:= true;
     Label13.Hide;
     cbSampleRate.Hide;
     sbParamScroll.Show;
     cgTransfer.Show;
     cbXAxis.Items.Strings[0]:= 'Время, с';
     end;
    else
    begin
      MainForm.cbPointPerStep.Checked:= false;
      cgTransfer.Enabled:= false;
      Label13.Show;
      cbSampleRate.Show;
      cgTransfer.Hide;
      sbParamScroll.Hide;
      cbXAxis.Items.Strings[0]:= 'Номер точки';
    end;
  end;
end;

procedure tReadingsForm.ParamsChange(Sender: TObject);
begin
  ParamsApplied:= false;
end;

procedure tReadingsForm.cbShowPointsChange(Sender: TObject);
begin
  Chart1LineSeries1.ShowPoints:= cbShowPoints.Checked;
  Chart2LineSeries1.ShowPoints:= cbShowPoints.Checked;
end;

procedure tReadingsForm.cbUseGenFreqChange(Sender: TObject);
begin
  UseGenFreq:= cbUseGenFreq.Checked;
  if UseGenFreq then
    LogFreq:= true;
end;

procedure tReadingsForm.cbXAxisChange(Sender: TObject);
begin
  Source1.Reset;
  Source2.Reset;
end;

procedure tReadingsForm.cgTransferItemClick(Sender: TObject; Index: integer);
var
  i, ParNum: word;
begin
  if (Index = ReferenceIndex) and not cgTransfer.Checked[Index] then
    if not UseGenFreq then
      LogFreq:= false;

  ParNum:= 0;
  with cgTransfer do
  begin
    for i:= 0 to Items.Count - 1 do
      if Checked[i] then
        inc(ParNum);
    if ParNum = MaxSimultPars then
    begin
      for i:= 0 to Items.Count - 1 do
        if not Checked[i] then
          CheckEnabled[i]:= false;
    end
    else
    for i:= 0 to Items.Count - 1 do
      CheckEnabled[i]:= true;
    if ParNum < 2 then
    begin
     cbReadingsMode.ItemIndex:= 0;
     cgTransfer.Enabled:= false;
    end;
  end;
  cbReadingsModeChange(Self);
end;

{$R *.lfm}

end.

