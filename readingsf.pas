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
    ParToRead: array [0..5] of word;
    ReadPars: array [1..11] of boolean;
    ProcessedPoints: longword;
    StartTime, PauseTime, PauseLength: TDateTime;

    QNone, QX, QY, QRv, QRdBm, QPhi, QAUXIN1, QAUXIN2, QRefFreq, QCH1Disp, QCH2Disp,
      QAUXIN3, QAUXIN4: word;

    srcTime: TAxisSource;
    srcFreq: TAxisSource;
    srcAmpl: TAxisSource;
    srcX: TAxisSource;
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
    srcDisp2: TAxisSource;

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
    function RecvSnap(p1, p2: word; p3: word = 0;
                                    p4: word = 0;
                                    p5: word = 0;
                                    p6: word = 0): PBuffer;
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
         else AItem.X:= srcRef.Values[Aindex];
      end;
    2: if LogAmpl then AItem.X:= srcAmpl.Values[Aindex];
  end;

  i:= cbChart1Show.ItemIndex + 1;

  case i of
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
  end;

  if Src <> nil then AItem.Y:= Src.Values[Aindex] else Source1.PointsNumber := 0  ;
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
         else AItem.X:= srcRef.Values[Aindex];
       end;
    2: if LogAmpl then AItem.X:= srcAmpl.Values[Aindex];
  end;

  i:= cbChart2Show.ItemIndex + 1;

  case i of
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
  end;
                                      //range err prolly wrong pointsnumber
  try
  if Src <> nil then AItem.Y:= Src.Values[Aindex] else Source2.PointsNumber := 0;
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
            AddCommand(SPTS, true);               //paus+
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

            setlength(ParamArr, 3);  //static?
            ParamArr[0]:= 1;
            ParamArr[1]:= PointsRead;
            ParamArr[2]:= PointsToRead;
            try
              EnterCriticalSection(CommCS);
                AddCommand(TRCL, true, ParamArr);
                PassCommands;
                SerPort.RecvBufferEx(@BuffByte1, PointsToRead * 4, ConnectParams.Timeout);

                ParamArr[0]:= 2;

                AddCommand(TRCL, true, ParamArr);
                PassCommands;
                SerPort.RecvBufferEx(@BuffByte2, PointsToRead * 4, ConnectParams.Timeout);
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
          p1:= RecvSnap(ParToRead[0],ParToRead[1],ParToRead[2],
                        ParToRead[3],ParToRead[4],ParToRead[5]);

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

  QNone:= 0;
  QX:= 1;
  QY:= 2;
  QRv:= 3;

  SerPort:= TBlockSerial.Create;
  SerPort.ConvertLineEnd:= true;
  SerPort.RaiseExcept:= true;
  SerPort.DeadLockTimeOut:= 10000;
  DeviceIndex:= iDefaultDevice;
  DrawnBuffers:= 0;
  t:= 0;

  LogFreq:= false;
  LogTime:= true;
  LogAmpl:= false;

  {SupportedDevices[iSR844].Commands:= SR844Command;
  SupportedDevices[iSR830].Commands:= SR830Command; }
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
  SerPort.Destroy;
  ThreadList.Destroy;
  DoneCriticalSection(CommCS);
  DoneCriticalSection(RNCS);
  DoneCriticalSection(TimeCS);
  DoneCriticalSection(OPSCS);
  DoneCriticalSection(RTDCS);
end;

procedure TReadingsForm.FormShow(Sender: TObject);
begin
  GetSupportedDevices(DeviceForm.sgDetCommands);
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
      if LogFreq and (not cgTransfer.Checked[QRefFreq - 1]) and (not UseGenFreq) then
        cgTransfer.Checked[QRefFreq - 1]:= true;
      cgTransferItemClick(Self, 0);

      if not cgTransfer.Enabled then
      begin
        ShowMessage('Необходимо логирование опорной чатоты');
        WriteProgramLog('Data collection could not start: not logging ref');
        btStartPauseLog.Caption:= 'Начать снятие';
        exit;
      end;

      if cgTransfer.Checked[QRefFreq - 1] and (not LogFreq)
        then LogFreq:= true;

      LogTime:= true;

      j:= 0;
      ReadPars[11]:= false;
      Params.TransferPars[11]:= false;
      with cgTransfer do
      begin
        for i:= 1 to Items.Count do
        begin
          ReadPars[i]:= false;
          Params.TransferPars[i]:= false;
          if Checked[i - 1] then
          begin
            ReadPars[i]:= true;
            Params.TransferPars[i]:= true;
            ParToRead[j]:= i;
            inc(j);
          end;
        end;
      end;

      for i:= j to 5 do ParToRead[i]:= QNone;

      for j:= 0 to 5 do
      begin
        if ParToRead[j] = QNone then break
        else
        if ParToRead[j] = QX       then srcX:=      TAxisSource.Create(128)
        else
        if ParToRead[j] = QY       then srcY:=      TAxisSource.Create(128)
        else
        if ParToRead[j] = QRv      then srcRV:=     TAxisSource.Create(128)
        else
        if ParToRead[j] = QRdBm    then srcRdBm:=   TAxisSource.Create(128)
        else
        if ParToRead[j] = QPhi     then srcPhi:=    TAxisSource.Create(128)
        else
        if ParToRead[j] = QAUXIN1  then srcAUXIN1:= TAxisSource.Create(128)
        else
        if ParToRead[j] = QAUXIN2  then srcAUXIN2:= TAxisSource.Create(128)
        else
        if ParToRead[j] = QRefFreq then srcRef:=    TAxisSource.Create(128)
        else
        if ParToRead[j] = QCH1Disp then srcDisp1:=  TAxisSource.Create(128)
        else
        if ParToRead[j] = QCH2Disp then srcDisp2:=  TAxisSource.Create(128)
        else
        if ParToRead[j] = QAUXIN3  then srcAUXIN3:= TAxisSource.Create(128)
        else
        if ParToRead[j] = QAUXIN4  then srcAUXIN4:= TAxisSource.Create(128);
      end;
    end
    else
    begin
      LogTime:= true;  //???
      if LogFreq then cbUseGenFreq.Checked:= true;
      cbUseGenFreqChange(Self);

      srcDisp1:=  TAxisSource.Create(128);
      srcDisp2:=  TAxisSource.Create(128);
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
    cbChart1Show.ItemIndex:= QCh1Disp - 1;
    cbChart2Show.ItemIndex:= QCh2Disp - 1;

    EnterCriticalSection(CommCS);
      AddCommand(STRT);
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
      AddCommand(PAUS);
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
      AddCommand(STRT);
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
      AddCommand(REST);
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
          if ParToRead[i] = QNone then break;
          if ParToRead[i] = QCh1Disp then
            s1+= 'Дисп.1 (' + cbCh1.Text + ')' + HT
          else
          if ParToRead[i] = QCh2Disp then
            s1+= 'Дисп.2 (' + cbCh2.Text + ')' + HT
          else
          s1+= cbChart1Show.Items[ParToRead[i] - 1];
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

function TReadingsForm.RecvSnap(p1, p2: word; p3: word = 0;
                  p4: word = 0; p5: word = 0; p6: word = 0): PBuffer;
var
  num, i: word;
  ParamArr: array of longint; //CONSTRUCTOR!!!
  s: string;
begin
  if (p1 = 0) or (p2 = 0) then
  begin
    Result:= nil;
    exit;
  end;

  if p3 = 0 then num:= 2
  else
  if p4 = 0 then num:= 3
  else
  if p5 = 0 then num:= 4
  else
  if p6 = 0 then num:= 5
  else
  num:= 6;

  //WriteProgramLog(strf(num) + ' SNAP elements');

  setlength(ParamArr, num);
  ParamArr[0]:= p1;
  ParamArr[1]:= p2;
  if num > 2 then
  ParamArr[2]:= p3;
  if num > 3 then
  ParamArr[3]:= p4;
  if num > 4 then
  ParamArr[4]:= p5;
  if num = 6 then
  ParamArr[5]:= p6;

  try
  EnterCriticalSection(CommCS);
    AddCommand(SNAP, true, ParamArr);
    PassCommands;
    s:= RecvString;
  finally
    LeaveCriticalSection(CommCS);
    //WriteProgramLog('Error: ' + serport.lasterrordesc);
  end;

  //sleep(60 + random(30));
  //s:= strf(random)+',' +strf(random)+',' + strf(random);

  if s = '' then
  begin
    Result:= nil;
    exit;
  end;

  new(Result);
  setlength(Result^, num);//MEM LEAK 2 blocks 24 and 4 still leaking +++ 3 blocks 8 32 4

  for i:= 0 to num - 2 do
  begin
    val(copy(s, 1, pos(SupportedDevices[DeviceIndex].ParSeparator, s) - 1), Result^[i]);
    delete(s, 1, pos(SupportedDevices[DeviceIndex].ParSeparator, s));
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
          srcDisp1.Add(v);
          s+= strf(v) + HT;

          v:= Buffer(DataList.Items[i * 2 + 1]^)[j];
          srcDisp2.Add(v);
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
        lk:= 0;
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

          for k:= (lk + 1) to cgTransfer.Items.Count do
          begin
            //WriteProgramLog('lk ' + strf(lk)+' k '+strf(k));
            if ReadPars[k] then
            begin
              //writeprogramlog('k' + strf(k));
              case k of
                1: srcX.Add(v);
                2: srcY.Add(v);
                3: srcRV.Add(v);
                11:srcDisp2.Add(v);
                else
                begin
                  if k = QRdbM    then srcRdBm.Add(v)
                  else
                  if k = QPhi     then srcPhi.Add(v)
                  else
                  if k = QAUXIN1  then srcAUXIN1.Add(v)
                  else
                  if k = QAUXIN2  then srcAUXIN2.Add(v)
                  else
                  if k = QAUXIN3  then srcAUXIN3.Add(v)
                  else
                  if k = QAUXIN4  then srcAUXIN4.Add(v)
                  else
                  if k = QRefFreq then srcRef.Add(v)
                  else
                  if k = QCH1Disp then srcDisp1.Add(v)
                  else
                  if k = QCH2Disp then srcDisp2.Add(v)
                end;
              end;
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

      if LogTime then ProcessedPoints:= srcTime.Count
      else
      if LogFreq then
      begin
       if UseGenFreq then ProcessedPoints:= srcFreq.Count
       else ProcessedPoints:= srcRef.Count
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
begin
   if (MainForm.Serport.InstanceActive) and
    (MainForm.cbPortSelect.ItemIndex = cbPortSelect.ItemIndex) then
    begin
      showmessage('К данному порту уже осуществляется подключение');
      exit
    end;
  OptionForm.TabControl.TabIndex:= 1;
  EnterCriticalSection(CommCS);
    inherited btnConnectClick(Sender);
    AddCommand(REST);
    PassCommands;
  LeaveCriticalSection(CommCS);

  if DeviceIndex = iDefaultDevice then exit;

  cgTransfer.Items.Clear;
  cbCh1.Items.Clear;
  cbCh2.Items.Clear;
  cbChart1Show.Items.Clear;
  cbChart2Show.Items.Clear;
  cbSensitivity.Items.Clear;
  cbTimeConstant.Items.Clear;
  case DeviceIndex of
    iSR844:
    begin
      Label14.Hide;
      Label15.Hide;
      cbRatio1.Hide;
      cbRatio2.Hide;
      cgTransfer.Items.AddStrings(SR844TransferParams);
      cbCh1.Items.AddStrings(SR844CH1Params);
      cbCh2.Items.AddStrings(SR844CH2Params);
      cbChart1Show.Items.AddStrings(SR844TransferParams);
      cbChart2Show.Items.AddStrings(SR844TransferParams);
      cbSensitivity.Items.AddStrings(SR844Sensitivities);
      cbTimeConstant.Items.AddStrings(SR844TimeConstants);

      QRdBm:= 4;
      QPhi:= 5;
      QAUXIN1:= 6;
      QAUXIN2:= 7;
      QRefFreq:= 8;
      QCH1Disp:= 9;
      QCH2Disp:= 10;

      QAUXIN3:= 255;
      QAUXIN4:= 255;
    end;
    iSR830:
    begin
      Label14.Show;
      Label15.Show;
      cbRatio1.Show;
      cbRatio2.Show;
      cgTransfer.Items.AddStrings(SR830TransferParams);
      cbCh1.Items.AddStrings(SR830CH1Params);
      cbCh2.Items.AddStrings(SR830CH2Params);
      cbChart1Show.Items.AddStrings(SR830TransferParams);
      cbChart2Show.Items.AddStrings(SR830TransferParams);
      cbSensitivity.Items.AddStrings(SR830Sensitivities);
      cbTimeConstant.Items.AddStrings(SR830TimeConstants);

      QPhi:= 4;
      QAUXIN1:= 5;
      QAUXIN2:= 6;
      QAUXIN3:= 7;
      QAUXIN4:= 8;
      QRefFreq:= 9;
      QCH1Disp:= 10;
      QCH2Disp:= 11;

      QRdBm:= 255;
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

    for i:= 1 to cgTransfer.Items.Count do
        cgTransfer.Checked[i - 1]:= TransferPars[i];
  end;

  c:= 0;
  for i:= 0 to cgTransfer.Items.Count - 1 do
    if cgTransfer.Checked[i] then inc(c);

  if c < 2 then
  begin
    cgTransfer.Checked[QRefFreq - 1]:= true;
    cgTransfer.Checked[QCH1Disp - 1]:= true;
    cgTransfer.Checked[QCH2Disp - 1]:= true;
  end;
end;

procedure TReadingsForm.btClearClick(Sender: TObject);
var
  DataList: TList;
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
  freeandnil(srcX);
  freeandnil(srcY);
  freeandnil(srcRV);
  freeandnil(srcRdBm);
  freeandnil(srcPhi);
  freeandnil(srcAUXIN1);
  freeandnil(srcAUXIN2);
  freeandnil(srcAUXIN3);
  freeandnil(srcAUXIN4);
  freeandnil(srcRef);
  freeandnil(srcDisp1);
  freeandnil(srcDisp2);
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

  ConnectParams.Timeout:= seRecvTimeOut.Value;

  UpdateTimer.Interval:= eUpdateInterval.Value;
  if DeviceIndex = iSR844 then setlength(ParamArr, 2);
  if DeviceIndex = iSR830 then setlength(ParamArr, 3);
  ParamArr[0]:= 1;
  ParamArr[1]:= cbCh1.ItemIndex;
  if DeviceIndex = iSR830 then ParamArr[2]:= cbRatio1.ItemIndex;

  EnterCriticalSection(CommCS);
    AddCommand(DDEF, false, ParamArr);

    ParamArr[0]:= 2;
    ParamArr[1]:= cbCh2.ItemIndex;
    if DeviceIndex = iSR830 then ParamArr[2]:= cbRatio2.ItemIndex;
    AddCommand(DDEF, false, ParamArr);

    AddCommand(SRAT, false, cbSampleRate.ItemIndex);
    AddCommand(SENS, false, cbSensitivity.ItemIndex);
    AddCommand(OFLT, false, cbTimeConstant.ItemIndex);
    AddCommand(FMOD, false, 0);   //external ref freq
    PassCommands;
  LeaveCriticalSection(CommCS);
end;

procedure TReadingsForm.btQueryClick(Sender: TObject);
var
  s1, s2, s3: string;
  i, e: integer;
begin
  EnterCriticalSection(CommCS);
    AddCommand(DDEF, true, 1);
    AddCommand(DDEF, true, 2);
    AddCommand(SRAT, true);
    AddCommand(SENS, true);
    AddCommand(OFLT, true);
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

  if DeviceIndex = iSR830 then
  begin
    s3:= SupportedDevices[DeviceIndex].ParSeparator;

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
  end;
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
   cgTransfer.Show;
  end
  else
  begin
    MainForm.cbPointPerStep.Checked:= false;
    cgTransfer.Enabled:= false;
    Label13.Show;
    cbSampleRate.Show;
    cgTransfer.Hide;
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
  if (Index + 1 = QRefFreq) and not cgTransfer.Checked[Index] then
  if not UseGenFreq then LogFreq:= false;

  ParNum:= 0;
  with cgTransfer do
  begin
    for i:= 0 to Items.Count - 1 do
    if Checked[i] then inc(ParNum);
    if ParNum = 6 then
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

