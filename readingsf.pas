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
    btStartPauseLog: TButton;
    btStopLog: TButton;
    btClear: TButton;

    cbChart1Show: TComboBox;
    cbChart2Show: TComboBox;
    cbTimeConstant: TComboBox;
    cbSensitivity: TComboBox;
    cbSampleRate: TComboBox;
    cbShowPoints: TCheckBox;

    cbCh1: TComboBox;
    cbCh2: TComboBox;
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
    cbReserve: TComboBox;

    eAxisLimit: TFloatSpinEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label2: TLabel;

    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;

    Label9: TLabel;

    PairSplitter1: TPairSplitter;
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

    Source1: TUserDefinedChartSource;
    Source2: TUserDefinedChartSource;
    UpdateTimer: TTimer;

    procedure btClearClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject); override;
    procedure btApplyClick(Sender: TObject);
    procedure btQueryClick(Sender: TObject); override;
    procedure btStartPauseLogClick(Sender: TObject);
    procedure btStatusClick(Sender: TObject);
    procedure btStopLogClick(Sender: TObject);
    procedure cbChart1ShowChange(Sender: TObject);
    procedure cbChart2ShowChange(Sender: TObject);
    procedure cbReadingsModeChange(Sender: TObject);
    procedure cbShowPointsChange(Sender: TObject);
    procedure cbUseGenFreqChange(Sender: TObject);
    procedure cbXAxisChange(Sender: TObject);
    procedure cgTransferItemClick(Sender: TObject; Index: integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PairSplitter1Resize(Sender: TObject);
    procedure Source1GetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure Source2GetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure UpdateTimerTimer(Sender: TObject);

  private
    { private declarations }
    ReportHeader: boolean;


  private
    RNCS, TimeCS, OPSCS, RTDCS: TRTLCriticalSection;
    FOPS: boolean;
    FLogState: eLogState;
    DrawnBuffers: longword;
    t, SampleRate: double;
    ReadingMode: eReadMode;
    MaxSimultPars, TotalPars,
    FirstIndex, ReferenceIndex, CH1Index, CH2Index: shortint;
    PointsInBuffer: longword;

    ReadPars: array of boolean;
    ProcessedPoints: longword;
    StartTime, PauseTime, PauseLength: TDateTime;

    CoordinateSources: array of TAxisSource;
    srcTime, srcFreq, srcAmpl, srcCH1, srcCH2: TAxisSource;

    function GetOPS: boolean;
    procedure SetLogState(AValue: eLogState);
    procedure SetOPS(AValue: boolean);

    function GetTime: TDateTime;
  public
    { public declarations }
    ParToRead: array of shortint;
    ThreadList: TThreadList;
    LogTime, LogFreq, LogAmpl, UseGenFreq: boolean;

    TimeStep: double;

    CurrLogFileName: string;
    ReadPoints: longint;
    property LogState: eLogState read FLogState write SetLogState;
    property OnePointPerStep: boolean read GetOPS write SetOPS;
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

uses Dateutils, math, stepf, optionf, DeviceF;

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
  DrawnBuffers:= 0;
  t:= 0;

  LogFreq:= false;
  LogTime:= true;
  LogAmpl:= false;

  for i:= 0 to PortCount - 1 do
    cbPortSelect.AddItem(MainForm.cbPortSelect.Items[i], nil);

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
end;

procedure tReadingsForm.PairSplitter1Resize(Sender: TObject);
begin
  with PairSplitter1 do
    Position:= Width div 2;
end;

procedure tReadingsForm.EnableControls(Enable: boolean);    //+++
begin
  btApply.Enabled:= Enable;
  btStartPauseLog.Enabled:= Enable;
  btStopLog.Enabled:= Enable;
  btCustomCommand.Enabled:= Enable;
  btReset.Enabled:= Enable;
  btStatus.Enabled:= Enable;
  btQuery.Enabled:= Enable;
  cbReadingsMode.Enabled:= Enable;
  cbUseGenFreq.Enabled:= Enable;
  cbSampleRate.Enabled:= Enable;
  cbTimeConstant.Enabled:= Enable;
  cbSensitivity.Enabled:= Enable;
  cbUseGenFreq.Enabled:= Enable;
  cbCh1.Enabled:= Enable;
  cbCh2.Enabled:= Enable;
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
    cbCh1.Enabled:= false;
    cbCh2.Enabled:= false;
    cbRatio1.Enabled:= false;
    cbRatio2.Enabled:= false;
    cbReadingsMode.Enabled:= false;
    cbSampleRate.Enabled:= false;
    cbTimeConstant.Enabled:= false;
    cbSensitivity.Enabled:= false;
    cgTransfer.Enabled:= false;
    cbUseGenFreq.Enabled:= false;
    seRecvTimeout.Enabled:= false;
    btApply.Enabled:= false;
    btClear.Enabled:= false;
  end
  else
  begin
    cbCh1.Enabled:= true;
    cbCh2.Enabled:= true;
    cbRatio1.Enabled:= true;
    cbRatio2.Enabled:= true;
    cbReadingsMode.Enabled:= true;
    cbSampleRate.Enabled:= true;
    cbTimeConstant.Enabled:= true;
    cbSensitivity.Enabled:= true;
    cgTransfer.Enabled:= true;
    cbUseGenFreq.Enabled:= true;
    seRecvTimeout.Enabled:= true;
    btApply.Enabled:= true;
    if FLogState = lInActive then btClear.Enabled:= true;
  end;
end;

function tReadingsForm.GetOPS: boolean;
begin
  EnterCriticalSection(OPSCS);
  try
    Result:= FOPS;
  finally
    LeaveCriticalSection(OPSCS);
  end;
end;

procedure tReadingsForm.SetOPS(AValue: boolean);
begin
  EnterCriticalSection(OPSCS);
  try
    FOPS:= AValue;
  finally
    LeaveCriticalSection(OPSCS);
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

  ReadingMode:= eReadMode(cbReadingsMode.ItemIndex);

    if ReadingMode = rSimultaneous then
    begin
      if LogFreq and (not cgTransfer.Checked[ReferenceIndex]) and (not UseGenFreq) then
        cgTransfer.Checked[ReferenceIndex]:= true;
      cgTransferItemClick(Self, 0);

      if not cgTransfer.Enabled then  { TODO 1 -cBug : wtf is this shit??? }
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
    end
    else
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

  btApplyClick(Self);
  t:= 0;
  ReadPoints:= 0;
  LogState:= lActive;
  CurrLogFileName:= CreateLog;

  if SampleRate <> 0 then
    TimeStep:= 1/SampleRate
  else
    TimeStep:= 1;
  UpdateTimer.Enabled:= true;
  PauseLength:= 0;
  StartTime:= Now;

  case ReadingMode of
    rBuffer:
      ReadingsThread:= tBufferThread.Create(PointsInBuffer);
    rSimultaneous:
    begin
      if OnePointPerStep then
        ReadingsThread:= tOnePerStepThread.Create
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
end;

procedure tReadingsForm.ContinueLog;
begin
  btStartPauseLog.Caption:= 'Приостановить';
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
    //showmessage(experimentlog.FileName);
    s1:= '';
    if LogFreq and UseGenFreq then s1+= 'Частота (генератор)' + HT;
    if LogAmpl then s1+= 'Амплитуда (генератор)' + HT;

    case ReadingMode of
      rBuffer:
        s1+= cbXAxis.Items.Strings[0] + HT + cbCh1.Text + HT + cbCh2.Text;
      rSimultaneous:
      begin
        if LogTime then s1+= cbXAxis.Items.Strings[0] + HT;
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
  if CurrLogFileName = '' then exit(-1);

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
  if s = '' then
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
  if ConnectionKind = cSerial then
  begin
   if (MainForm.Serport.InstanceActive) and
    (MainForm.cbPortSelect.ItemIndex = cbPortSelect.ItemIndex) then
    begin
      showmessage('К данному порту уже осуществляется подключение');
      exit
    end;
  end
  else
  if ConnectionKind = cTelnet then
  begin
    if ReadingsForm.ConnectionKind = cTelNet then showmessage('check ip');
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

  with Params do
  begin
    cbSensitivity.ItemIndex:= Sensitivity;
    cbTimeConstant.ItemIndex:= TimeConstant;
    cbCh1.ItemIndex:= Display1;
    cbCh2.ItemIndex:= Display2;
    cbRatio1.ItemIndex:= Ratio1;
    cbRatio2.ItemIndex:= Ratio2;
    cbChart1Show.ItemIndex:= Show1;
    cbChart2Show.ItemIndex:= Show2;

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
end;

procedure tReadingsForm.btApplyClick(Sender: TObject);
begin
  {if cbSampleRate.ItemIndex <> cbSampleRate.Items.Count - 1 then
    SampleRate:= (intpower(2, cbSampleRate.ItemIndex)) * 0.0625
  else SampleRate:= 0; }
  SampleRate:= 1;
  case ReadingMode of
    rBuffer:
      cbXAxis.Items.Strings[0]:= 'Номер точки';
    rSimultaneous:
      cbXAxis.Items.Strings[0]:= 'Время, с';
  end;
   { TODO 2 -cFeature : Autosens }
  with Params do
  begin
    GenFreq:= UseGenFreq;
    SampleRate:= cbSampleRate.ItemIndex;
    TimeConstant:= cbTimeConstant.ItemIndex;
    Sensitivity:= cbSensitivity.ItemIndex;

    Display1:= cbCh1.ItemIndex;
    Display2:= cbCh2.ItemIndex;
    Ratio1:= cbRatio1.ItemIndex;
    Ratio2:= cbRatio2.ItemIndex;
    Show1:= cbChart1Show.ItemIndex;
    Show2:= cbChart2Show.ItemIndex;

    UpdateInterval:= eUpdateInterval.Value;
    AxisLimit:= eAxisLimit.Value;
    XAxis:= cbXAxis.ItemIndex;
    ReadingsMode:= cbReadingsMode.ItemIndex;
  end;

  CurrentDevice^.Timeout:= seRecvTimeOut.Value;

  UpdateTimer.Interval:= eUpdateInterval.Value;

  EnterCriticalSection(CommCS);
  if cbRatio1.ItemIndex >= 0 then
  begin
    AddCommand(dDisplaySelect, false, tIntegerArray.Create(1, cbCH1.ItemIndex, cbRatio1.ItemIndex));
    AddCommand(dDisplaySelect, false, tIntegerArray.Create(2, cbCH2.ItemIndex, cbRatio2.ItemIndex));
  end
  else
  begin
    AddCommand(dDisplaySelect, false, tIntegerArray.Create(1, cbCH1.ItemIndex));
    AddCommand(dDisplaySelect, false, tIntegerArray.Create(2, cbCH2.ItemIndex));
  end;

    AddCommand(dSampleRate, false, cbSampleRate.ItemIndex);
    AddCommand(dSensitivity, false, cbSensitivity.ItemIndex);
    AddCommand(dTimeConstant, false, cbTimeConstant.ItemIndex);
    AddCommand(dReferenceSource, false, 0);   //external ref freq
    PassCommands;
  LeaveCriticalSection(CommCS);
end;

procedure tReadingsForm.btQueryClick(Sender: TObject);
var
  s1, s2, s3: string;
  i, e: integer;
begin
  EnterCriticalSection(CommCS);
    AddCommand(dDisplaySelect, true, 1);
    AddCommand(dDisplaySelect, true, 2);
    AddCommand(dSampleRate, true);
    AddCommand(dSensitivity, true);                          { TODO 2 -cBug : fix query }
    AddCommand(dTimeConstant, true);
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
  LeaveCriticalSection(CommCS);

 { if DeviceIndex = iSR830 then
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
  if DeviceIndex = iSR844 then
  begin
    val(s1, i, e);
    if e = 0 then cbCh1.ItemIndex:= i;

    val(s2, i, e);
    if e = 0 then cbCh2.ItemIndex:= i;
  end; }
end;

procedure tReadingsForm.btStopLogClick(Sender: TObject);
begin
  StopLog(true);
end;

procedure tReadingsForm.cbChart1ShowChange(Sender: TObject);
begin
  if (LogState = lActive) and (ReadingMode = rBuffer) then
    cbChart1Show.ItemIndex:= cbChart1Show.Items.IndexOf(cbCH1.Text);
  Source1.PointsNumber:= ProcessedPoints;
  Source1.Reset;
end;

procedure tReadingsForm.cbChart2ShowChange(Sender: TObject);
begin
  if (LogState = lActive) and (ReadingMode = rBuffer) then
    cbChart2Show.ItemIndex:= cbChart2Show.Items.IndexOf(cbCH2.Text);
  Source2.PointsNumber:= ProcessedPoints;
  Source2.Reset;
end;

procedure tReadingsForm.cbReadingsModeChange(Sender: TObject);
begin
  if CbReadingsMode.ItemIndex = integer(rSimultaneous) then
  begin
   cgTransfer.Enabled:= true;
   Label13.Hide;
   cbSampleRate.Hide;
   sbParamScroll.Show;
   cgTransfer.Show;
  end
  else
  begin
    MainForm.cbPointPerStep.Checked:= false;
    cgTransfer.Enabled:= false;
    Label13.Show;
    cbSampleRate.Show;
    cgTransfer.Hide;
    sbParamScroll.Hide;
  end;
end;

procedure tReadingsForm.cbShowPointsChange(Sender: TObject);
begin
  Chart1LineSeries1.ShowPoints:= cbShowPoints.Checked;
  Chart2LineSeries1.ShowPoints:= cbShowPoints.Checked;
end;

procedure tReadingsForm.cbUseGenFreqChange(Sender: TObject);
begin
  UseGenFreq:= cbUseGenFreq.Checked;
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
  if not UseGenFreq then LogFreq:= false;

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

