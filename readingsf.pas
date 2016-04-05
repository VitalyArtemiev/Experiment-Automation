unit ReadingsF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TASources, TATools,
  TATransformations, TADbSource, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, Spin, PairSplitter, Buttons, ComCtrls, synaser,
  MainF, SerConF, TACustomSource, AxisSource;

type
  SR844Buffer = array[0..PointsInSR844Buffer - 1] of double;
  Buffer = array of double;
  pBuffer = ^Buffer;

  tLogState = (lInActive, lActive, lPaused);

  { TReadingsThread }

  tReadingsThread = class(TThread)
    procedure Execute; override;
  private
    BuffByte1, BuffByte2: array [0..4 * (PointsInSR844Buffer - 1)] of byte;
    procedure StopSync;
  end;

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
    function GetRTD: boolean;
    procedure SetRTD(AValue: boolean);
  private
    { private declarations }
    ReportHeader: boolean;
    ReadingsThread: TReadingsThread;
    ThreadList: TThreadList;
  private
    RNCS, TimeCS, OPSCS, RTDCS: TRTLCriticalSection;
    FRN, FOPS, FRTD: boolean;
    FLogState: tLogState;
    DrawnBuffers: longword;
    t, SampleRate: double;
    ReadingMode, RM: word;
    MaxSimultPars, TotalPars,
    FirstIndex, ReferenceIndex, CH1Index, CH2Index: shortint;
    ParToRead: array {[0..5]} of shortint;
    ReadPars: array {[1..11]} of boolean;
    ProcessedPoints: longword;
    StartTime, PauseTime, PauseLength: TDateTime;

    {QNone, QX, QY, QRv, QRdBm, QPhi, QAUXIN1, QAUXIN2, QRefFreq, QCH1Disp, QCH2Disp,
      QAUXIN3, QAUXIN4: word; }

    CoordinateSources: array of TAxisSource;
    srcTime, srcFreq, srcAmpl, srcCH1, srcCH2: TAxisSource;
    {srcX: TAxisSource;
    srcY: TAxisSource;
    srcRV: TAxisSource;
    srcRdBm: TAxisSource;
    srcPhi: TAxisSource;
    srcAUXIN1: TAxisSource;
    srcAUXIN2: TAxisSource;
    srcAUXIN3: TAxisSource;
    srcAUXIN4: TAxisSource;
    srcRef: TAxisSource;
    srcDisp1: TAxisSource;
    srcDisp2: TAxisSource;}

    function GetOPS: boolean;
    procedure SetLogState(AValue: tLogState);
    procedure SetOPS(AValue: boolean);
    procedure SetRN(RN: boolean);
    function GetRN: boolean;

    function GetTime: TDateTime;
  public
    { public declarations }
    //ReadingDone,
    LogTime, LogFreq, LogAmpl, UseGenFreq: boolean;
    Buffer1, Buffer2: SR844Buffer;

    TimeStep: double;
    BuffString1, BuffString2: ansistring;
    //BuffByte1, BuffByte2: array[0..4*(PointsInSR844Buffer-1)] of byte;

    CurrLogFileName: string;
    PointsRead: longint;
    property LogState: tLogState read FLogState write SetLogState;
    property ReadingNeeded: boolean read GetRN write SetRN;
    property ReadingsThreadDone: boolean read GetRTD write SetRTD;
    property OnePointPerStep: boolean read GetOPS write SetOPS;
    property ElapsedTime: TDateTime read GetTime;

    procedure EnableControls(Enable: boolean); override;

    procedure BeginLog;
    procedure PauseLog;
    procedure ContinueLog;
    procedure StopLog(Force: boolean = false);
    function CreateLog: string;
   // procedure UpdateLog;
    function RecvSnap(p: array of shortint): PBuffer;
    procedure ProcessBuffers;
  end;

var
  ReadingsForm: TReadingsForm;


implementation

uses Dateutils, math, stepf, optionf, DetConst, DeviceF;

{ TReadingsThread }

procedure TReadingsForm.Source1GetChartDataItem(
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

  i:= cbChart1Show.ItemIndex;

  Src:= CoordinateSources[i];
  try
  if Src <> nil then
    AItem.Y:= Src.Values[Aindex]
  else Source1.PointsNumber := 0  ;
  except
     on e:exception do writeprogramlog(e.message +' '+ strf(aindex)+' ' + strf(Source2.PointsNumber) +' '+ strf(src.count))
  end;
end;

procedure TReadingsForm.Source2GetChartDataItem(
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

  i:= cbChart2Show.ItemIndex;

  Src:= CoordinateSources[i];
  {case i of
    1: Src:= srcX;
    2: Src:= srcY;
    3: Src:= srcRV;
    11:Src:= srcDisp2;
    else
    begin
      if i = QRdbM    then Src:= srcRdBm
      else
      if i = QPhi     then Src:= srcPhi
      else
      if i = QAUXIN1  then Src:= srcAUXIN1
      else
      if i = QAUXIN2  then Src:= srcAUXIN2
      else
      if i = QAUXIN3  then Src:= srcAUXIN3
      else
      if i = QAUXIN4  then Src:= srcAUXIN4
      else
      if i = QRefFreq then Src:= srcRef
      else
      if i = QCH1Disp then Src:= srcDisp1
      else
      if i = QCH2Disp then Src:= srcDisp2
    end;
  end;       }
                                      //range err prolly wrong pointsnumber
  try
  if Src <> nil then
    AItem.Y:= Src.Values[Aindex]
  else Source2.PointsNumber := 0;
  except
     on e:exception do writeprogramlog(e.message +' '+ strf(aindex)+' ' + strf(Source2.PointsNumber) +' '+ strf(src.count))
  end;
end;

procedure TReadingsThread.Execute;
var
  s: string;
  i, StoredPoints, PointsToRead: longint;
  o: ^smallint;
  p1, p2: PBuffer;
  pt: ^TDateTime;
  DataList: TList;

begin
  PointsToRead:= 0;
  storedpoints:= 0;
  repeat
    if ReadingsForm.ReadingNeeded then
    with ReadingsForm do
      case ReadingMode of     //защитить от  доступа
        0:
        begin
          try
          EnterCriticalSection(CommCS);
            AddCommand(dStoredPoints, true);               //paus+
            PassCommands;
            s:= RecvString;
          finally
            LeaveCriticalSection(CommCS);
          end;

          val(s, StoredPoints);

          //if random(1000)=0 then storedpoints+= 2;          //*
          //AddCommand(PAUS, false);
          //PassCommands;
          PointsToRead:= StoredPoints - PointsRead;
          //if random(10000) = 0 then PointstoRead:= 1;
          if PointsToRead > 0 then
          begin
            Serport.RaiseExcept:= false;

            {setlength(ParamArr, 3);  //static?
            ParamArr[0]:= 1;
            ParamArr[1]:= PointsRead;
            ParamArr[2]:= PointsToRead; }
            try
              EnterCriticalSection(CommCS);
                AddCommand(dReadPointsNative, true, tIntegerArray.Create(1, PointsRead, PointsToRead));
                PassCommands;
                SerPort.RecvBufferEx(@BuffByte1, PointsToRead * 4, CurrentDevice^.Timeout);

                //ParamArr[0]:= 2;

                AddCommand(dReadPointsNative, true, tIntegerArray.Create(2, PointsRead, PointsToRead));
                PassCommands;
                SerPort.RecvBufferEx(@BuffByte2, PointsToRead * 4, CurrentDevice^.Timeout);
            finally
                Serport.RaiseExcept:= true;
              LeaveCriticalSection(CommCS);
            end;

            for i:= 0 to PointsToRead - 1 do
            begin
              o:= @BuffByte1[i*4];
              Buffer1[i]:= smallint(o^) * power(2, BuffByte1[i*4 + 2] - 124);    //конвертация из внутр. формата

              o:= @BuffByte2[i*4];
              Buffer2[i]:= smallint(o^) * power(2, BuffByte2[i*4 + 2] - 124);
            end;

            new(p1);
            new(p2);
            p1^:= Buffer1;  //длина автоматически присваивается
            p2^:= Buffer2;
            setlength(p1^, PointsToRead);    //исправляем длину, чтоб не передать лишние
            setlength(p2^, PointsToRead);
            DataList:= ThreadList.LockList;
              DataList.Add(p1);
              DataList.Add(p2);
              i:= DataList.Count;
            ThreadList.UnlockList;
            PointsRead:= StoredPoints;
          end;
          //AddCommand(STRT, false);
          //PassCommands;
          { TODO 1 -cBug : Continuos buffer }
          if OnePointPerStep then ReadingNeeded:= false
        end;

        1:
        begin
          try
          p1:= RecvSnap(ParToRead);

          except on E:Exception do
            writeprogramlog(E.Message);
          end;

          if p1 <> nil then
          begin
            new(pt);
            pt^:= ElapsedTime;
            DataList:= ThreadList.LockList;
              DataList.Add(pt);
              DataList.Add(p1);
            ThreadList.UnlockList;
            inc(PointsRead);
          end;
          if OnePointPerStep then ReadingNeeded:= false
        end;
      end
    else StopSync;    { TODO 2 -cImprovement : Fix this thread shit }

  until Terminated;
end;

procedure TReadingsThread.StopSync;
begin
  ReadingsForm.ReadingsThreadDone:= true;
end;

procedure TReadingsForm.FormCreate(Sender: TObject);
var
  i: longint;
begin
  WriteProgramLog('Creating Readings form');

  Top:= MainForm.Top - 5;//.Height div 2 - Height div 2;
  Left:= Screen.Width div 2;

  btQuery.Caption:= 'Запрос' + LineEnding + 'текущих' + LineEnding + 'значений';
  btReset.Caption:= 'Сбросить' + LineEnding + '‌настройки'+ LineEnding + 'прибора';
  btCustomCommand.Caption:= 'Польз.' + LineEnding + 'команда';

  {QNone:= 0;
  QX:= 1;
  QY:= 2;
  QRv:= 3; }

  //InitSerPort;
  DeviceKind:= dDetector;
  DeviceIndex:= iDefaultDevice;
  DrawnBuffers:= 0;
  t:= 0;

  LogFreq:= false;
  LogTime:= true;
  LogAmpl:= false;

  for i:= 0 to PortCount - 1 do
    cbPortSelect.AddItem(MainForm.cbPortSelect.Items[i], nil);

  ReadingsThread:= TReadingsThread.Create(true);
  ReadingsThread.FreeOnTerminate:= false;
  Threadlist:= TThreadList.Create;
  InitCriticalSection(CommCS);
  InitCriticalSection(RNCS);
  InitCriticalSection(TimeCS);
  InitCriticalSection(OPSCS);
  InitCriticalSection(RTDCS);
end;

procedure TReadingsForm.FormDestroy(Sender: TObject);
begin
  if LogState <> lInActive then StopLog;
  btClearClick(Self);
  ReadingsThread.Terminate;
  //ReadingsThread.WaitFor;
  ReadingsThread.Destroy;
  SerPort.Free;
  TelNetClient.Free;
  ThreadList.Destroy;
  DoneCriticalSection(CommCS);
  DoneCriticalSection(RNCS);
  DoneCriticalSection(TimeCS);
  DoneCriticalSection(OPSCS);
  DoneCriticalSection(RTDCS);
end;

procedure TReadingsForm.FormShow(Sender: TObject);
begin
  GetSupportedDevices(DeviceKind);
end;

procedure TReadingsForm.PairSplitter1Resize(Sender: TObject);
begin
  with PairSplitter1 do
    Position:= Width div 2;
end;

procedure TReadingsForm.EnableControls(Enable: boolean);    //+++
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

procedure TReadingsForm.UpdateTimerTimer(Sender: TObject);
begin
  ProcessBuffers;
end;

function TReadingsForm.GetRTD: boolean;
begin
  EnterCriticalSection(RTDCS);
  try
    Result:= FRTD;
  finally
    LeaveCriticalSection(RTDCS);
  end;
end;

procedure TReadingsForm.SetRTD(AValue: boolean);
begin
  EnterCriticalSection(RTDCS);
  try
    FRTD:= AValue;
  finally
    LeaveCriticalSection(RTDCS);
  end;
end;

procedure TReadingsForm.SetRN(RN: boolean);
begin
  EnterCriticalSection(RNCS);
  try
    FRN:= RN;
    if RN then ReadingsThreadDone:= false;
  finally
    LeaveCriticalSection(RNCS);
  end;
end;

procedure TReadingsForm.SetLogState(AValue: tLogState);
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

function TReadingsForm.GetOPS: boolean;
begin
  EnterCriticalSection(OPSCS);
  try
    Result:= FOPS;
  finally
    LeaveCriticalSection(OPSCS);
  end;
end;

procedure TReadingsForm.SetOPS(AValue: boolean);
begin
  EnterCriticalSection(OPSCS);
  try
    FOPS:= AValue;
  finally
    LeaveCriticalSection(OPSCS);
  end;
end;

function TReadingsForm.GetRN: boolean;
begin
  EnterCriticalSection(RNCS);
  try
    Result:= FRN;
  finally
    LeaveCriticalSection(RNCS);
  end;
end;

function TReadingsForm.GetTime: TDateTime;
begin
  EnterCriticalSection(TimeCS);
  try
    Result:= Now - StartTime - PauseLength;
  finally
    LeaveCriticalSection(TimeCS);
  end;
end;

procedure TReadingsForm.BeginLog;
var
  i, j: word;
begin
  if LogState = lActive then StopLog;
  btStartPauseLog.Caption:= 'Приостановить';
  WriteProgramLog('Data collection start');
  btClearClick(Self);

  ReadingMode:= cbReadingsMode.ItemIndex;
  RM:= ReadingMode;

    if RM = 1 then
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

      if cgTransfer.Checked[ReferenceIndex] and (not LogFreq)
        then LogFreq:= true;

      LogTime:= true;

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

  btApplyClick(Self);
  PointsRead:= 0;
  LogState:= lActive;
  CurrLogFileName:= CreateLog;
  if SampleRate <> 0 then TimeStep:= 1/SampleRate
  else TimeStep:= 1;
  UpdateTimer.Enabled:= true;

  if RM = 0 then
  begin
   // cbChart1Show.ItemIndex:= QCh1Disp - 1;
   //cbChart2Show.ItemIndex:= QCh2Disp - 1;
   { TODO 2 -cImprovement : fix chshow  }
    EnterCriticalSection(CommCS);
      AddCommand(dStartStorage);
      PassCommands;
    LeaveCriticalSection(CommCS);
  end;

  PauseLength:= 0;
  StartTime:= Now;
  ReadingNeeded:= true;
  ReadingsThread.Start;

end;

procedure TReadingsForm.PauseLog;
begin
  btStartPauseLog.Caption:= 'Продолжить';
  WriteProgramLog('Data collection pause');
  LogState:= lPaused;
  UpdateTimer.Enabled:= false;

  if RM = 0 then
  begin
    EnterCriticalSection(CommCS);
      AddCommand(dPauseStorage);
      PassCommands;
    LeaveCriticalSection(CommCS);
  end;

  ReadingNeeded:= false;
  PauseTime:= Now;
end;

procedure TReadingsForm.ContinueLog;
begin
  btStartPauseLog.Caption:= 'Приостановить';
  WriteProgramLog('Data collection resume');

  LogState:= lActive;
  UpdateTimer.Enabled:= true;
  ReadingNeeded:= true;

  if RM = 0 then
  begin
    EnterCriticalSection(CommCS);
      AddCommand(dStartStorage);
      PassCommands;
    LeaveCriticalSection(CommCS);
  end;

  PauseLength+= Now - PauseTime;
end;

procedure TReadingsForm.StopLog(Force: boolean = false);
begin
  WriteProgramLog('Data collection end');

  ReadingNeeded:= false;//???
  UpdateTimer.Enabled:= false;
  t:= 0;

  if (RM = 0) or (Force and (LogState = lInActive)) then
  begin
    EnterCriticalSection(CommCS);
      AddCommand(dResetStorage);
      PassCommands;
    LeaveCriticalSection(CommCS);
  end;

  if LogState <> lInActive then
  begin
   repeat

   until ReadingsThreadDone;

   ProcessBuffers;  //SaveLog;

   OnePointPerStep:= false;
   LogState:= lInActive;
   freeandnil(ExperimentLog);
   if Config.AutoExportParams then
     MainForm.ExportParams(false, ReportHeader);
   inc(ExperimentNumber);
  end;
  btStartPauseLog.Caption:= 'Начать снятие';
end;

function TReadingsForm.CreateLog: string;
var
  FileName, s1, s2: string;
  i: word;
begin
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

    case RM of
      0: s1+= cbCh1.Text + HT + cbCh2.Text;
      1:
      begin
        if LogTime then s1+= 'Время' + HT;
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

function TReadingsForm.RecvSnap(p: array of shortint): PBuffer;
var
  num, i: word;
  ParamArr: array of longint; //CONSTRUCTOR!!!
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

procedure TReadingsForm.ProcessBuffers;
var
  i, j, k, lk: longint;
  s: string;
  v, f, a: double;
  DataList: TList;
begin
  //try
  DataList:= ThreadList.LockList;
  //WriteProgramLog(strf(DataList.Count) + ' buff elements');

  if LogFreq and UseGenFreq then f:= StepF.F;
  if LogAmpl then a:= StepF.A;

  if DataList.Count > 0 then
  case RM of
    0:
    begin
      for i:= 0 to (DataList.Count div 2) - 1 do
      begin
        ProcessedPoints+= length(Buffer(DataList.Items[i * 2]^));
        //WriteProgramLog('high'+strf(high(Buffer(DataList.Items[i * 2]^))));
        for j:= 0 to high(Buffer(DataList.Items[i * 2]^)) do
        begin
          //WriteProgramLog('i '+strf(i)+' j '+strf(j));
          s:= '';

          if LogFreq and UseGenFreq then
          begin
            srcFreq.Add(f);
            s+= strf(f) + HT;
          end;

          if LogAmpl then
          begin
            srcAmpl.Add(a);
            s+= strf(a) + HT;
          end;

          srcTime.Add(t);
          s+= strf(t) + HT;

          v:= Buffer(DataList.Items[i * 2]^)[j];
          srcCH1.Add(v);
          s+= strf(v) + HT;

          v:= Buffer(DataList.Items[i * 2 + 1]^)[j];
          srcCH2.Add(v);
          s+= strf(v) + LineEnding;

          if ExperimentLog <> nil then ExperimentLog.Write(s[1], length(s));

          t+= TimeStep;
        end;
        dispose(PBuffer(DataList.Items[i * 2]));
        dispose(PBuffer(DataList.Items[i * 2 + 1]));
      end;
        DataList.Clear;
    end;

    1:
    begin
      for i:= 0 to (DataList.Count div 2) - 1 do
      begin
        lk:= -1;
        s:= '';

        if LogFreq and UseGenFreq then
        begin
          srcFreq.Add(f);
          s+= strf(f) + HT;
        end;

        if LogAmpl then
        begin
          srcAmpl.Add(a);
          s+= strf(a) + HT;
        end;

        t:= SecondSpan(TDateTime(DataList.Items[i * 2]^), 0);
        dispose(PDateTime(DataList.Items[i * 2]));
        if LogTime then
        begin
          srcTime.Add(t);
          s+= strf(t) + HT;
        end;

        for j:= 0 to high(Buffer(DataList.Items[i * 2 + 1]^)) do
        begin
          v:= Buffer(DataList.Items[i * 2 + 1]^)[j];
          s+= strf(v) + HT;

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

        s+= LineEnding;
        if ExperimentLog <> nil then ExperimentLog.Write(s[1], length(s));

        //t+= TimeStep;
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

procedure TReadingsForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if LogState <> lInActive then StopLog;
  MainForm.miShowReadingsF.Checked:= false;
end;

procedure TReadingsForm.btStartPauseLogClick(Sender: TObject);
begin
  case LogState of
    lInActive: BeginLog;
    lActive:   PauseLog;
    lPaused:   ContinueLog;
  end;
end;

procedure TReadingsForm.btStatusClick(Sender: TObject);
begin
  inherited btStatusClick(Sender);
end;

procedure TReadingsForm.btnConnectClick(Sender: TObject);
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

  deviceindex:= 2;

  if DeviceIndex = iDefaultDevice then exit;

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

  cgTransfer.Items.AddText(DeviceForm.sgDetCommands.Cells[DeviceIndex, integer(hTransferParams)]);
  cbChart1Show.Items:= cgTransfer.Items;
  cbChart2Show.Items:= cgTransfer.Items;
  cbCh1.Items.AddText(DeviceForm.sgDetCommands.Cells[DeviceIndex, integer(hCH1Options)]);
  cbCh2.Items.AddText(DeviceForm.sgDetCommands.Cells[DeviceIndex, integer(hCH2Options)]);
  cbRatio1.Items.AddText(DeviceForm.sgDetCommands.Cells[DeviceIndex, integer(hRatio1Options)]);
  cbRatio2.Items.AddText(DeviceForm.sgDetCommands.Cells[DeviceIndex, integer(hRatio2Options)]);
  cbSensitivity.Items.AddText(DeviceForm.sgDetCommands.Cells[DeviceIndex, integer(hSensitivityOptions)]);
  cbTimeConstant.Items.AddText(DeviceForm.sgDetCommands.Cells[DeviceIndex, integer(hTimeConstOptions)]);

  TotalPars:= cgTransfer.Items.Count;
  setlength(ReadPars, TotalPars);
  MaxSimultPars:= valf(DeviceForm.sgDetCommands.Cells[DeviceIndex, integer(hMaxSimultPars)]);
  setlength(ParToRead, MaxSimultPars);

  s:= DeviceForm.sgDetCommands.Cells[DeviceIndex, integer(hIndices)];
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

procedure TReadingsForm.btClearClick(Sender: TObject);
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

procedure TReadingsForm.btApplyClick(Sender: TObject);
begin
  if cbSampleRate.ItemIndex <> 14 then
    SampleRate:= (intpower(2, cbSampleRate.ItemIndex)) * 0.0625
  else SampleRate:= 0;
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

procedure TReadingsForm.btQueryClick(Sender: TObject);
var
  s1, s2, s3: string;
  i, e: integer;
begin
  EnterCriticalSection(CommCS);
    AddCommand(dDisplaySelect, true, 1);
    AddCommand(dDisplaySelect, true, 2);
    AddCommand(dSampleRate, true);
    AddCommand(dSensitivity, true);
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

procedure TReadingsForm.btStopLogClick(Sender: TObject);
begin
  StopLog(true);
end;

procedure TReadingsForm.cbChart1ShowChange(Sender: TObject);
begin
  Source1.PointsNumber:= ProcessedPoints;
  Source1.Reset;
end;

procedure TReadingsForm.cbChart2ShowChange(Sender: TObject);
begin
  Source2.PointsNumber:= ProcessedPoints;
  Source2.Reset;
end;

procedure TReadingsForm.cbReadingsModeChange(Sender: TObject);
begin
  if CbReadingsMode.ItemIndex = 1 then
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

procedure TReadingsForm.cbShowPointsChange(Sender: TObject);
begin
  Chart1LineSeries1.ShowPoints:= cbShowPoints.Checked;
  Chart2LineSeries1.ShowPoints:= cbShowPoints.Checked;
end;

procedure TReadingsForm.cbUseGenFreqChange(Sender: TObject);
begin
  UseGenFreq:= cbUseGenFreq.Checked;
end;

procedure TReadingsForm.cbXAxisChange(Sender: TObject);
begin
  Source1.Reset;
  Source2.Reset;
end;

procedure TReadingsForm.cgTransferItemClick(Sender: TObject; Index: integer);
var
  i, ParNum: word;
begin
  if (Index = ReferenceIndex) and not cgTransfer.Checked[Index] then
  if not UseGenFreq then LogFreq:= false;

  ParNum:= 0;
  with cgTransfer do
  begin
    for i:= 0 to Items.Count - 1 do
    if Checked[i] then inc(ParNum);
    if ParNum = MaxSimultPars then
    begin
     for i:= 0 to Items.Count - 1 do
     if not Checked[i] then CheckEnabled[i]:= false;
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

