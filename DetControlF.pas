unit DetControlF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TASources, TATools,
  TATransformations, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, PairSplitter, Buttons, ComCtrls, Menus, EditBtn, MainF,
  BaseConF, ReadingThreads, TACustomSource, AxisSource, Types, LogModule;

type
  { TDetControlForm }

  TDetControlForm = class(tConnectionForm)
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
    ChartToolset: TChartToolset;
    DataPointHintTool: TDataPointHintTool;
    fneDataFileStub: TDirectoryEdit;
    ZoomDragTool: TZoomDragTool;
    PanDragTool: TPanDragTool;
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
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    ZoomOut: TMenuItem;
    ZoomIn: TMenuItem;
    RestoreScale: TMenuItem;
    Divider: TMenuItem;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    eUpdateInterval: TSpinEdit;
    Chart1: TChart;
    Chart2: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart2LineSeries1: TLineSeries;
    pnGraphControl: TPanel;
    pnConnection: TPanel;
    pmChart: TPopupMenu;
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
    procedure ChartMenuItemClick(Sender: TObject);
    procedure DataPointHintToolHint(ATool: TDataPointHintTool;
      const APoint: TPoint; var AHint: String);
    procedure fneDataFileStubAcceptDirectory(Sender: TObject; var Value: String
      );
    procedure fneDataFileStubButtonClick(Sender: TObject);
    procedure fneDataFileStubEditingDone(Sender: TObject);
    procedure PanDragToolAfterMouseDown(ATool: TChartTool; APoint: TPoint);
    procedure PanDragToolAfterMouseMove(ATool: TChartTool; APoint: TPoint);
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
    procedure pmChartPopup(Sender: TObject);
    procedure RestoreScaleClick(Sender: TObject);
    procedure Source1GetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure Source2GetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure ZoomInClick(Sender: TObject);
    procedure ZoomOutClick(Sender: TObject);

  private
    { private declarations }
    ReportHeader, ForceStop: boolean;
    DrawnBuffers: longword;//for continuous buffer (not implemented)
    t, SampleRate: double; //t for buffer and writing to sources
    ReadingMode: eReadMode;
    MaxSimultPars, TotalPars,
    ReferenceIndex, CH1Index, CH2Index: shortint; { TODO 2 -cImprovement : to device record }
    PointsInBuffer: longword;

    iTC, iSe, iPa, iCH1, iCH2, iRCH1, iRCH2, iBF, iCR, iWR, iRa{, iOf}: array of string;

    ReadPars: array of boolean;
    srcFreq, srcAmpl, srcCH1, srcCH2: TAxisSource;

    procedure CheckTransferCount;
    //methods for logmodule events
    procedure BeforeStart(Sender: tLogModule);
    procedure Start(Sender: tLogModule);
    procedure Pause(Sender: tLogModule);
    procedure Continue(Sender: tLogModule);
    procedure Stop(Sender: tLogModule);
    procedure CreateFile(Sender: tLogModule);
    function SaveLog(Sender: tLogModule):integer;
    procedure StateChange(Sender: tLogModule);
    procedure ProcessBuffers(Sender: tLogModule);
  public
    { public declarations }
    ReportSaved: boolean;
    LogStub, LogExtension, DataFolder: string;
    Log: tLogModule;

    ParToRead: array of shortint;
    LogTime, LogFreq, LogAmpl, UseGenFreq, OnePointPerStep, OffsetTracked,
      AutoApply: boolean;
    TimeStep: double;

    procedure EnableControls(Enable: boolean); override;
    procedure GetDeviceParams; override;

    function RecvSnap(p: array of shortint): PBuffer;
  end;

var
  DetControlForm: TDetControlForm;

implementation

uses
  DateUtils, StrUtils, TAChartUtils, stepf, optionf, DeviceF, TempControlF,
  OffsetF
  {$IFOPT D+}
  ,synaser
  {$ENDIF}
  ;

procedure TDetControlForm.Source1GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
var
  i: word;
  Src: TAxisSource;
begin
  case cbXAxis.ItemIndex of
    0: if LogTime then
         AItem.X:= Log.srcTime.Values[Aindex];
    1: if LogFreq then
      begin
         if UseGenFreq then
           AItem.X:= srcFreq.Values[Aindex]
         else
           AItem.X:= Log.CoordinateSources[ReferenceIndex].Values[Aindex];
      end;
    2: if LogAmpl then
         AItem.X:= srcAmpl.Values[Aindex];
  end;
                    { TODO 3 -cFeature : show gen freq? }
  if (ReadingMode = rBuffer) then
    i:= 0
  else
    i:= cbChart1Show.ItemIndex;

  Src:= Log.CoordinateSources[i];
  if Src <> nil then
    AItem.Y:= Src.Values[Aindex]
  else Source1.PointsNumber:= 0;
end;

procedure TDetControlForm.Source2GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
var
  i: word;
  Src: TAxisSource;
begin
  case cbXAxis.ItemIndex of
    0: if LogTime then
         AItem.X:= Log.srcTime.Values[Aindex];
    1: if logFreq then
       begin
         if UseGenFreq then
           AItem.X:= srcFreq.Values[Aindex]
         else
           AItem.X:= Log.CoordinateSources[ReferenceIndex].Values[Aindex];
       end;
    2: if LogAmpl then
         AItem.X:= srcAmpl.Values[Aindex];
  end;

  if ReadingMode = rBuffer then
    i:= 1
  else
    i:= cbChart2Show.ItemIndex;

  Src:= Log.CoordinateSources[i];
  if Src <> nil then
    AItem.Y:= Src.Values[Aindex]
  else Source2.PointsNumber:= 0;
end;

procedure TDetControlForm.FormCreate(Sender: TObject);
var
  i: longint;
begin
  WriteProgramLog('Creating Readings form');

  Top:= MainForm.Top;
  Left:= MainForm.Left + MainForm.Width + 8;

  btQuery.Caption:= 'Запрос' + LineEnding + 'текущих' + LineEnding + 'значений';
  btReset.Caption:= 'Сбросить' + LineEnding + '‌настройки'+ LineEnding + 'прибора';
  btCustomCommand.Caption:= 'Польз.' + LineEnding + 'команда';
  btStatus.Caption:= 'Состояние' + LineEnding + ' прибора';

  DisplayMessages:= true;
  ParamsApplied:= false;
  MaxSimultPars:= -1;
  DeviceKind:= dDetector;
  DeviceIndex:= iDefaultDevice;
  ConnectionKind:= cNone;
  DrawnBuffers:= 0;
  t:= 0;
  DataFolder:= DefaultLogFolder;
  LogStub:= '';
  ExperimentNumber:= 1;
  ReportSaved:= false;

  LogFreq:= false;
  LogTime:= true;
  LogAmpl:= false;

  for i:= 0 to PortCount - 1 do
    cbPortSelect.AddItem(MainForm.cbPortSelect.Items[i], nil);

  if cbPortSelect.ItemIndex < 0 then
    cbPortSelect.ItemIndex:= 0;

  if PortCount = 1 then
    StatusBar.Panels[spStatus].Text:= 'Нет доступных COM-портов';

  Log:= tLogModule.Create;

  Log.OnBeforeStart:= @BeforeStart;
  Log.OnStart:= @Start;
  Log.OnPause:= @Pause;
  Log.OnContinue:= @Continue;
  Log.OnStop:= @Stop;
  Log.OnCreateFile:= @CreateFile;
  Log.OnSaveLog:= @SaveLog;
  Log.OnStateChange:= @StateChange;
  Log.OnProcessBuffers:= @ProcessBuffers;

  InitCriticalSection(CommCS);
end;

procedure TDetControlForm.FormDestroy(Sender: TObject);
begin
  Log.Stop;
  btClearClick(Self); //log stops here
  btnDisconnectClick(Self);

  {SerPort.Free;
  TelNetClient.Free; }
  Log.Free;

  DoneCriticalSection(CommCS);
end;

procedure TDetControlForm.FormShow(Sender: TObject);
begin
  GetSupportedDevices(DeviceKind);
  if cbRatio1.ItemIndex < 0 then
    cbRatio1.ItemIndex:= 0;
  if cbRatio2.ItemIndex < 0 then
    cbRatio2.ItemIndex:= 0;
end;

procedure TDetControlForm.GraphSplitterResize(Sender: TObject);
begin
  with GraphSplitter do
    Position:= Width div 2;
end;

procedure TDetControlForm.pmChartPopup(Sender: TObject);
var
  i: integer;
  cb: tCombobox;
begin
  with TChart(pmChart.PopupComponent) do
    case Name of
      'Chart1': cb:= cbChart1Show;
      'Chart2': cb:= cbChart2Show;
      else cb:= cbChart1Show;
    end;

  with cb do
  begin
    if ItemIndex >= 0 then
    begin
      for i:= 0 to pmChart.Items.Count - 1 do
        pmChart.Items[i].Checked:= false;
      if ItemIndex + 4 < pmChart.Items.Count then
        pmChart.Items[ItemIndex + 4].Checked:= true;
    end;
  end;
end;

procedure TDetControlForm.EnableControls(Enable: boolean);    //+++
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
  cbRatio1.Enabled:=          Enable;
  cbRatio2.Enabled:=          Enable;
  eDelay.Enabled:=            Enable;
end;

procedure TDetControlForm.GetDeviceParams;
var
  i, c: integer;
  s: string;
begin
  {$IFOPT D+}
  if Debug then
  if DeviceIndex = 0 then
  begin
   deviceindex:= 1;
   connectionkind:= cserial;
   DetControlForm.serport:= tblockserial.create;
  end;
  {$ENDIF}

  for i:= 4 to pmChart.Items.Count - 1 do
    pmChart.Items.Delete(4);

  with DeviceForm.sgDetCommands do
  begin
    SeparateIndices(Cells[DeviceIndex, integer(hTimeConstOptions)], cbTimeConstant.Items, iTC);
    SeparateIndices(Cells[DeviceIndex, integer(hSensitivityOptions)], cbSensitivity.Items, iSe);
    SeparateIndices(Cells[DeviceIndex, integer(hTransferParams)], cgTransfer.Items, iPa);

    if cgTransfer.Width < 90 then
    begin
      cgTransfer.Columns:= 2;
    end
    else
    begin
      cgTransfer.Columns:= 1;
    end;

    cbChart1Show.Items:= cgTransfer.Items;
    cbChart2Show.Items:= cgTransfer.Items;
    for i:= 0 to cgTransfer.Items.Count - 1 do
    with pmChart do
    begin
      Items.Add(tMenuItem.Create(pmChart));
      Items[Items.Count - 1].Caption:= cgTransfer.Items[i];
      Items[Items.Count - 1].OnClick:= @ChartMenuItemClick;
    end;

    SeparateIndices(Cells[DeviceIndex, integer(hCH1Options)], cbCh1.Items, iCH1);
    SeparateIndices(Cells[DeviceIndex, integer(hCH2Options)], cbCh2.Items, iCH2);
    SeparateIndices(Cells[DeviceIndex, integer(hRatio1Options)], cbRatio1.Items, iRCH1);
    SeparateIndices(Cells[DeviceIndex, integer(hRatio2Options)], cbRatio2.Items, iRCH2);
    SeparateIndices(Cells[DeviceIndex, integer(hBufferRateOptions)], cbSampleRate.Items, iBF);
    SeparateIndices(Cells[DeviceIndex, integer(hCloseReserveOptions)], cbReserve1.Items, iCR);
    SeparateIndices(Cells[DeviceIndex, integer(hWideReserveOptions)], cbReserve2.Items, iWR);
    SeparateIndices(Cells[DeviceIndex, integer(hRangeOptions)], cbInputRange.Items, iRa);

    MinDelay:= valf(Cells[DeviceIndex, integer(hMinDelay)]);
    eDelay.MinValue:= MinDelay;

    TotalPars:= cgTransfer.Items.Count;
    setlength(ReadPars, TotalPars);
    MaxSimultPars:= valf(Cells[DeviceIndex, integer(hMaxSimultPars)]);
    setlength(ParToRead, MaxSimultPars);

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

  if cbRatio1.Items.Count = 0 then
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

  if cbReserve1.Items.Count = 0 then
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

  if cbReserve2.Items.Count = 0 then
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

  if cbInputRange.Items.Count = 0 then
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
    if cgTransfer.Checked[i] then
      inc(c);

  if c < 2 then
  begin
    cgTransfer.Checked[ReferenceIndex]:= true;
    if CH1Index >= 0 then
      cgTransfer.Checked[CH1Index]:= true;
    if (CH2Index >= 0) and (c + 2 < MaxSimultPars) then
      cgTransfer.Checked[CH2Index]:= true;
  end;

  fneDataFileStubEditingDone(Self);
end;

procedure TDetControlForm.UpdateTimerTimer(Sender: TObject);
begin
  Log.ProcessBuffers;
end;

procedure TDetControlForm.ZoomInClick(Sender: TObject);
var
  c: tChart;
  Rect: tDoubleRect;
  Center: tDoublePoint;
begin
  c:= tChart(pmChart.PopupComponent);
  with c do
  begin
    Rect:= CurrentExtent;

    Center.x:= (Rect.a.x + Rect.b.x) / 2;
    Center.y:= (Rect.a.y + Rect.b.y) / 2;

    Rect.a.x:= (Rect.a.x - Center.X) / ChartZoomFactor + Center.X;
    Rect.b.x:= (Rect.b.x - Center.X) / ChartZoomFactor + Center.X;
    Rect.a.y:= (Rect.a.y - Center.y) / ChartZoomFactor + Center.Y;
    Rect.b.y:= (Rect.b.y - Center.y) / ChartZoomFactor + Center.Y;

    LogicalExtent:= Rect;
  end;
end;

procedure TDetControlForm.ZoomOutClick(Sender: TObject);
var
  c: tChart;
  Rect: tDoubleRect;
  Center: tDoublePoint;
begin
  c:= tChart(pmChart.PopupComponent);
  with c do
  begin
    Rect:= CurrentExtent;

    Center.x:= (Rect.a.x + Rect.b.x) / 2;
    Center.y:= (Rect.a.y + Rect.b.y) / 2;

    Rect.a.x:= (Rect.a.x - Center.X) * ChartZoomFactor + Center.X;
    Rect.b.x:= (Rect.b.x - Center.X) * ChartZoomFactor + Center.X;
    Rect.a.y:= (Rect.a.y - Center.y) * ChartZoomFactor + Center.Y;
    Rect.b.y:= (Rect.b.y - Center.y) * ChartZoomFactor + Center.Y;

    LogicalExtent:= Rect;
  end;
end;

procedure TDetControlForm.CheckTransferCount;
var
  i, ParNum: word;
begin
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

procedure TDetControlForm.RestoreScaleClick(Sender: TObject);
begin
  tChart(pmChart.PopupComponent).ZoomFull(true);
end;

procedure TDetControlForm.BeforeStart(Sender: tLogModule);
var
  i, j: integer;
begin
  with Sender do
  begin
    //btStartPauseLog.Caption:= 'Приостановить';  //to an event  onbeforestart
    WriteProgramLog('Data collection start');
    btClearClick(Self);

    if AutoApply and (StepForm.Finished or not DetControlForm.ParamsApplied) then  //skip if stepf is already going
    begin
      SetCursorAll(crHourGlass);
      DetControlForm.btApplyClick(Self);
      sleep(DetControlForm.eDelay.Value);
    end;
    SetCursorAll(crDefault);

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
            //btStartPauseLog.Caption:= 'Начать снятие';
            exit;
          end;
        cgTransferItemClick(Self, -1);
        end;
        if not cgTransfer.Enabled then
        begin
          ShowMessage('Необходимо логирование опорной чатоты');
          WriteProgramLog('Data collection could not start: not logging ref');
          //btStartPauseLog.Caption:= 'Начать снятие';
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
            if Checked[i] then
            begin
              ReadPars[i]:= true;
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
      cbChart1Show.ItemIndex:= cbChart1Show.Items.IndexOf(cbCH1.Text);
      cbChart2Show.ItemIndex:= cbChart2Show.Items.IndexOf(cbCH2.Text);
    end;

    if SampleRate <> 0 then
      TimeStep:= 1/SampleRate
    else
      TimeStep:= 1;

    if not OnePointPerStep then
    begin
      UpdateTimer.Interval:= eUpdateInterval.Value;
      UpdateTimer.Enabled:= true;
    end;

    t:= 0;
  end;
end;

procedure TDetControlForm.Start(Sender: tLogModule);
begin
  with Sender do
    case ReadingMode of   //onstart
      rBuffer:
      begin
        EnterCriticalSection(CommCS);
          AddCommand(dStartStorage);
          PassCommands;
        LeaveCriticalSection(CommCS);
        ReadingsThread:= tDetBufferThread.Create(PointsInBuffer);
      end;
      rSimultaneous:
      begin
        if OnePointPerStep then
          ReadingsThread:= nil //Assigned in stepform
        else
          ReadingsThread:= tDetSimultaneousThread.Create;
      end;
    end;
end;

procedure TDetControlForm.Pause(Sender: tLogModule);
begin
 // btStartPauseLog.Caption:= 'Продолжить';  //event onpause
  WriteProgramLog('Data collection pause');

  UpdateTimer.Enabled:= false;

  if ReadingMode = rBuffer then
  begin
    EnterCriticalSection(CommCS);
      AddCommand(dPauseStorage);
      PassCommands;
    LeaveCriticalSection(CommCS);
  end;
end;

procedure TDetControlForm.Continue(Sender: tLogModule);
begin
  //btStartPauseLog.Caption:= 'Приостановить';
  WriteProgramLog('Data collection resume');

  UpdateTimer.Enabled:= true;

  if ReadingMode = rBuffer then
  begin
    EnterCriticalSection(CommCS);
      AddCommand(dStartStorage);
      PassCommands;
    LeaveCriticalSection(CommCS);
  end;

  with Sender do
    case ReadingMode of
      rBuffer:
        ReadingsThread:= tDetBufferThread.Create(PointsInBuffer);
      rSimultaneous:
      begin
        if OnePointPerStep then
          ReadingsThread:= nil // Stepf
        else
          ReadingsThread:= tDetSimultaneousThread.Create;
      end;
    end;
end;

procedure TDetControlForm.Stop(Sender: tLogModule);
begin
  UpdateTimer.Enabled:= false;    //to event    OnStop

  if (ReadingMode = rBuffer) or (ForceStop and (Log.State = lInActive)) then
  begin
    EnterCriticalSection(CommCS);
      AddCommand(dResetStorage);
      PassCommands;
    LeaveCriticalSection(CommCS);
    ForceStop:= false;
  end;

  OnePointPerStep:= false;

  if Log.State <> lInActive then
  begin
    if Config.AutoExportParams then
    begin
      MainForm.ReportFolder:= Log.FilePath;
      MainForm.SaveReport(false, ReportHeader);
      ReportSaved:= true;
    end;
    inc(ExperimentNumber);
  end;
end;

procedure TDetControlForm.CreateFile(Sender: tLogModule);
var
  s1, s2: string;
  i: integer;
begin
  with Sender do
  begin

    Stub:= LogStub;
    FilePath:= DataFolder;

    if Config.AutoExportParams then
    begin
      if ReportNumber = 0 then
      begin
        inc(ReportNumber);
        str(ReportNumber, s1);
        while FileExists(FilePath + '\' + LogStub + '_' + FileName + '_' + s1 + '.txt') or
              FileExists(FilePath + '\' + LogStub + '_' + FileName + '_' + s1 + '_1' + LogExtensions[integer(dDetector)]) do
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
      while FileExists(FilePath + '\' + LogStub + '_' + FileName + '_' + s1 + LogExtensions[integer(dDetector)]) do
      begin
        inc(ExperimentNumber);
        str(ExperimentNumber, s1);
      end;

      ReportHeader:= false;
      FileName+= '_' + s1;
    end;
    FileName+= LogExtensions[integer(dDetector)];

    if LogTime then
      Header:= Header + cbXAxis.Items.Strings[0] + HT;
    if LogFreq and UseGenFreq then
      Header:= Header + 'Частота (генератор)' + HT;
    if LogAmpl then
      Header:= Header + 'Амплитуда (генератор)' + HT;

    case ReadingMode of
      rBuffer:
        Header:= Header + cbCh1.Text + HT + cbCh2.Text;
      rSimultaneous:
      begin
        for i:= 0 to high(ParToRead) do
        begin
          if ParToRead[i] = -1 then break;
          if ParToRead[i] = CH1Index then
            Header:= Header + 'Дисп.1 (' + cbCh1.Text + ')' + HT
          else
          if ParToRead[i] = CH2Index then
            Header:= Header + 'Дисп.2 (' + cbCh2.Text + ')' + HT
          else
            Header:= Header + cbChart1Show.Items[ParToRead[i]];
          Header:= Header + HT;
        end;
      end;
    end;
    Header:= Header + LineEnding;
  end;
end;

function TDetControlForm.SaveLog(Sender: tLogModule): integer;
var
  i, j: integer;
  s: string;
  Sources: array of tAxisSource;
begin
  with Sender do
  begin
    if IsEmptyStr(FileName, [' ']) then
      exit(-1);

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

    if assigned(ExperimentLog) then
      for i:= 0 to ReadPoints - 1 do
      begin
        s:= '';
        for j:= 0 to high(Sources) do
          s+= strf(Sources[j].Values[i]) + HT;
        s+= LineEnding;

          ExperimentLog.Write(s[1], length(s))
      end
    else
      exit(-2);

    StatusBar.Panels[spStatus].Text:= 'Данные сохранены в ' + FileName;
  end;
end;

procedure TDetControlForm.StateChange(Sender: tLogModule);
begin
  with tLogModule(Sender) do
    if State = lActive then
    begin
      btStartPauseLog.Caption:= PauseCaption;
      btReset.Enabled:=           false;
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
      btClear.Enabled:=           false;
      btApply.Enabled:=           false;
      pnConnection.Enabled:=      false;
    end
    else
    begin
      btReset.Enabled:=           true;
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
      btApply.Enabled:=           true;
      if State = lInActive then
      begin
        btClear.Enabled:=         true;
        pnConnection.Enabled:=    true;
        btStartPauseLog.Caption:= StartCaption;
        ReportSaved:= false;
      end
      else
        btStartPauseLog.Caption:= ContinueCaption;
    end;
end;

procedure TDetControlForm.ProcessBuffers(Sender: tLogModule);
var
  i, j, k, lk: longint;
  l, c: longword;
  v, f, a: double;
begin
  with Sender do
  begin
    if LogFreq and UseGenFreq then
      f:= StepF.F;
    if LogAmpl then
      a:= StepF.A;

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

            CoordinateSources[ParToRead[j]].Add(v);
            //WriteProgramLog('i ' + strf(i) + ' j ' + strf(j));
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
end;

function TDetControlForm.RecvSnap(p: array of shortint): PBuffer;
var
  num, i, j: integer;
  ParamArr: tStringArray; //CONSTRUCTOR!!!
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

  for i:= 0 to high(ParamArr) do
    ParamArr[i]:= ipa[p[i]];

  {for i:= 0 to high(ParamArr) do
    WriteProgramLog(ParamArr[i]);}

  try
    EnterCriticalSection(CommCS);
      AddCommand(dReadSimultaneous, true, ParamArr);
      PassCommands;
      s:= RecvString;
  finally
    LeaveCriticalSection(CommCS);
  end;

  if debug then
  begin
    sleep(60 + random(30));
    s:= strf(1)+',' +strf(0.51)+',' + strf(1.5);
  end;
  //writeprogramlog(s);
  if s = '' then
  begin
    Result:= nil;
    exit;
  end;

  new(Result);
  setlength(Result^, num);

  for i:= 0 to num - 2 do
  begin
    j:= pos(CurrentDevice^.ParSeparator, s);
    val(copy(s, 1, j - 1), Result^[i]);
    delete(s, 1, j);
  end;
  val(s, Result^[i + 1]);
end;

procedure TDetControlForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  Log.Stop;
  MainForm.miShowReadingsF.Checked:= false;
end;

procedure TDetControlForm.btStartPauseLogClick(Sender: TObject);
begin
  AutoApply:= true;
  Log.Toggle;
end;

procedure TDetControlForm.btStatusClick(Sender: TObject);
begin
  inherited btStatusClick(Sender);
end;

procedure TDetControlForm.btnConnectClick(Sender: TObject);
begin
  with MainForm do
  if cbPortSelect.ItemIndex = DetControlForm.cbPortSelect.ItemIndex then
  begin
    case ConnectionKind of
      cSerial:
      begin
        showmessage('К данному порту уже осуществляется подключение');
        exit
      end;
      cTelNet:
        if (CurrentDevice^.Host = DetControlForm.CurrentDevice^.Host) and
           (CurrentDevice^.Port = DetControlForm.CurrentDevice^.Port) then
          begin
            showmessage('По данному адресу уже осуществляется подключение');
            exit
          end;
    end;
  end;

  with TempControlForm do
  if cbPortSelect.ItemIndex = DetControlForm.cbPortSelect.ItemIndex then
  begin
    case ConnectionKind of
      cSerial:
      begin
        showmessage('К данному порту уже осуществляется подключение');
        exit
      end;
      cTelNet:
        if (CurrentDevice^.Host = DetControlForm.CurrentDevice^.Host) and
           (CurrentDevice^.Port = DetControlForm.CurrentDevice^.Port) then
          begin
            showmessage('По данному адресу уже осуществляется подключение');
            exit
          end;
    end;
  end;

  OptionForm.TabControl.TabIndex:= 1;
  OptionForm.DevicePage.TabIndex:= 1;

  inherited btnConnectClick(Sender);                                            //only override getdeviceparams

  if DeviceIndex = iDefaultDevice then
    exit;

  Params.DetectorPort:= DetControlForm.CurrentDevice^.Port;
  Params.LastDetector:= DetControlForm.CurrentDevice^.Model;

  OptionForm.eDevice1.ItemIndex:= DeviceIndex - 1;

  cbReadingsModeChange(Self);
  CheckTransferCount;                                                           //this needs to happen after restorestate

  EnterCriticalSection(CommCS);
    AddCommand(dResetStorage);
    PassCommands;
  LeaveCriticalSection(CommCS);
end;

procedure TDetControlForm.btClearClick(Sender: TObject);
begin
  Source1.PointsNumber:= 0;
  Source2.PointsNumber:= 0;
  Log.Clear;
  freeandnil(srcFreq);
  freeandnil(srcAmpl);

  StatusBar.Panels[spStatus].Text:= '';
end;

procedure TDetControlForm.btAutoPhaseClick(Sender: TObject);
begin
  EnterCriticalSection(CommCS);
    AddCommand(dAutoPhase, false);
    PassCommands;
  LeaveCriticalSection(CommCS);
  //btQueryClick(Self);
end;

procedure TDetControlForm.btAutoSensitivityClick(Sender: TObject);
var
  SerPollSB: byte;
  s: string;
  st, l: tDateTime;
begin
  EnableControls(false);
  SetCursorAll(crHourGlass);
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
  until ((SerPollSB div %10) mod %10 = %1);                                     //interface ready bit

  btQueryClick(Self);
  EnableControls(true);
  SetCursorAll(crDefault);
  StatusBar.Panels[spStatus].Text:= '';
end;

procedure TDetControlForm.btAutoRangeClick(Sender: TObject);
begin
  EnterCriticalSection(CommCS);
    AddCommand(dAutoRange, false);
    PassCommands;
  LeaveCriticalSection(CommCS);
  btQueryClick(Self);
end;

procedure TDetControlForm.btAutoReserve1Click(Sender: TObject);
begin
  EnterCriticalSection(CommCS);
    AddCommand(dAutoCloseReserve, false);
    PassCommands;
  LeaveCriticalSection(CommCS);
  btQueryClick(Self);
end;

procedure TDetControlForm.btAutoReserve2Click(Sender: TObject);
begin
   EnterCriticalSection(CommCS);
    AddCommand(dAutoWideReserve, false);
    PassCommands;
  LeaveCriticalSection(CommCS);
  btQueryClick(Self);
end;

procedure TDetControlForm.btApplyClick(Sender: TObject);
begin
  {if cbSampleRate.ItemIndex <> cbSampleRate.Items.Count - 1 then
    SampleRate:= (intpower(2, cbSampleRate.ItemIndex)) * 0.0625
  else SampleRate:= 0; }
  SampleRate:= 1;                                                               //no way to get real sample rate in a compatible way; SR865 needs to be polled
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
    //AxisLimit:= eAxisLimit.Value;
    XAxis:= cbXAxis.ItemIndex;
    ReadingsMode:= cbReadingsMode.ItemIndex;

    CurrentDevice^.Timeout:= seRecvTimeOut.Value; { TODO : sort out timeouts }

    EnterCriticalSection(CommCS);
    if cbRatio1.Visible then
    begin
      AddCommand(dDisplaySelect, false, tVariantArray.Create(1, iCH1[Display1], iRCH1[Ratio1]));
      AddCommand(dDisplaySelect, false, tVariantArray.Create(2, iCH2[Display2], iRCH2[Ratio2]));
    end
    else
    begin
      AddCommand(dDisplaySelect, false, tVariantArray.Create(1, iCH1[Display1]));
      AddCommand(dDisplaySelect, false, tVariantArray.Create(2, iCH2[Display2]));
    end;

      AddCommand(dSampleRate, false, iBF[SampleRate]);
      AddCommand(dSensitivity, false, iSe[Sensitivity]);
      AddCommand(dTimeConstant, false, iTC[TimeConstant]);
      AddCommand(dReferenceSource, false, 0);                                   //external ref freq
      if cbReserve1.Visible then
        AddCommand(dCloseReserve, false, iCR[CloseReserve]);
      if cbReserve2.Visible then
        AddCommand(dWideReserve, false, iWR[WideReserve]);
      if cbInputRange.Visible then
        AddCommand(dInputRange, false, iRa[InputRange]);
      PassCommands;
    LeaveCriticalSection(CommCS);
  end;
  ParamsApplied:= true;
end;

procedure TDetControlForm.btOffsetClick(Sender: TObject);
begin
  OffsetTracked:= true;
  OffsetForm.Show;
end;

procedure TDetControlForm.btQueryClick(Sender: TObject);
var
  s1, s2, s3: string;
  i: integer;
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
    i:= AnsiIndexStr(s3, iBF);
    if i>= 0 then
      cbSampleRate.ItemIndex:= i;

    s3:= RecvString;
    i:= AnsiIndexStr(s3, iSe);
    if i>= 0 then
      cbSensitivity.ItemIndex:= i;

    s3:= RecvString;
    i:= AnsiIndexStr(s3, iTC);
    if i>= 0 then
      cbTimeConstant.ItemIndex:= i;

    if cbReserve1.Visible then
    begin
      s3:= RecvString;
      i:= AnsiIndexStr(s3, iCR);
      if i>= 0 then
        cbReserve1.ItemIndex:= i;
    end;

    if cbReserve2.Visible then
    begin
      s3:= RecvString;
      i:= AnsiIndexStr(s3, iWR);
      if i>= 0 then
        cbReserve2.ItemIndex:= i;
    end;

    if cbInputRange.Visible then
    begin
      s3:= RecvString;
      i:= AnsiIndexStr(s3, iRa);
      if i>= 0 then
        cbInputRange.ItemIndex:= i;
    end;
  LeaveCriticalSection(CommCS);

  if cbRatio1.Visible then
  begin
    s3:= CurrentDevice^.ParSeparator;

    i:= AnsiIndexStr(copy(s1, 1, pos(s3, s1) - 1), iCH1);
    if i < 0 then
      exit;
    cbCh1.ItemIndex:= i;
    delete(s1, 1, pos(s3, s1));

    i:= AnsiIndexStr(s1, iRCH1);
    if i < 0 then
      exit;
    cbRatio1.ItemIndex:= i;

    i:= AnsiIndexStr(copy(s1, 1, pos(s3, s2) - 1), iCH2);
    if i < 0 then
      exit;
    cbCh2.ItemIndex:= i;
    delete(s2, 1, pos(s3, s2));

    i:= AnsiIndexStr(s2, iRCH2);
    if i < 0 then
      exit;
    cbRatio2.ItemIndex:= i;
  end
  else
  begin
    i:= AnsiIndexStr(s1, iCH1);
    if i >= 0 then
      cbCh1.ItemIndex:= i;

    i:= AnsiIndexStr(s2, iCH2);
    if i >= 0 then
      cbCh2.ItemIndex:= i;
  end;
end;

procedure TDetControlForm.btStopLogClick(Sender: TObject);
begin
  ForceStop:= true;
  Log.Stop;
end;

procedure TDetControlForm.cbChart1ShowChange(Sender: TObject);                  //FIXIT  how what even wtf
begin
  if (Log.State = lActive) and (ReadingMode = rBuffer) then
  with cbChart1Show do
  begin
    ItemIndex:= Items.IndexOf(cbCH1.Text);
    if ItemIndex < 0 then
      ItemIndex:= CH1Index;
  end;
  Source1.PointsNumber:= Log.ProcessedPoints;
  Source1.Reset;
end;

procedure TDetControlForm.cbChart2ShowChange(Sender: TObject);
begin
  if (Log.State = lActive) and (ReadingMode = rBuffer) then
  with cbChart2Show do
  begin
    ItemIndex:= Items.IndexOf(cbCH2.Text);
    if ItemIndex < 0 then
      ItemIndex:= CH2Index;
  end;
  Source2.PointsNumber:= Log.ProcessedPoints;
  Source2.Reset;
end;

procedure TDetControlForm.cbReadingsModeChange(Sender: TObject);
begin
  case cbReadingsMode.ItemIndex of
    integer(rBuffer):
    begin
      MainForm.cbPointPerStepDet.Checked:= false;
      cgTransfer.Enabled:= false;
      Label13.Show;
      cbSampleRate.Show;
      cgTransfer.Hide;
      sbParamScroll.Hide;
      cbXAxis.Items.Strings[0]:= 'Номер точки';
    end;
    integer(rSimultaneous):
    begin
      cgTransfer.Enabled:= true;
      Label13.Hide;
      cbSampleRate.Hide;
      sbParamScroll.Show;
      cgTransfer.Show;
      cbXAxis.Items.Strings[0]:= 'Время, с';
    end;
    integer(rRealTime):
    begin
      ShowMessage('В разработке');
      cbReadingsMode.ItemIndex:= integer(rSimultaneous);
    end;
  end;
end;

procedure TDetControlForm.ChartMenuItemClick(Sender: TObject);
var
  cb: tComboBox;
  i: integer;
begin
  with TChart(pmChart.PopupComponent) do
    case Name of
      'Chart1': cb:= cbChart1Show;
      'Chart2': cb:= cbChart2Show;
      else cb:= cbChart1Show;
    end;

  with cb do
  begin
    ItemIndex:= Items.IndexOf(tMenuItem(Sender).Caption);
    if ItemIndex >= 0 then
    begin
      for i:= 0 to pmChart.Items.Count - 1 do
        pmChart.Items[i].Checked:= false;
      tMenuItem(Sender).Checked:= true;
    end;
    OnChange(Self);
  end;
end;

procedure TDetControlForm.DataPointHintToolHint(
  ATool: TDataPointHintTool; const APoint: TPoint; var AHint: String);
var
  s: string;
  Point: TDoublePoint;
begin
  case Atool.Series.ParentChart.Name of
    'Chart1': AHint:= cbChart1Show.Text;
    'Chart2': AHint:= cbChart2Show.Text;
  end;
  Point:= ATool.NearestGraphPoint;
  if cbXAxis.Text = 'Номер точки' then
    str(trunc(Point.X), s)
  else
    str(Point.X:0:6, s);

    AHint:= ' ' + cbXAxis.Text + ': ' + s + '; ' + AHint + ': ';
    str(Point.Y:0:6, s);
    AHint+= s;
end;

procedure TDetControlForm.fneDataFileStubAcceptDirectory(Sender: TObject;
  var Value: String);
begin
  DataFolder:= Value;
  if pos(GetCurrentDir, DataFolder) <> 0 then
    delete(DataFolder, 1, length(GetCurrentDir) + 1);
  Value:= fneDataFileStub.Text;
end;

procedure TDetControlForm.fneDataFileStubButtonClick(Sender: TObject);
begin
  with fneDataFileStub do  //i modified lcl (tdirectoryedit) in order for this to work. property text used to be the same as directory, which, i find, is inconsistent. i added a variable fdirectory and modified getdirectory and setdirectory, replacing ftext with fdirectory.
  begin
    if pos('\', DataFolder) = 0 then
      RootDir:= GetCurrentDir + '\' + DataFolder
    else
      RootDir:= DataFolder;

    Directory:= '';
  end;
end;

procedure TDetControlForm.fneDataFileStubEditingDone(Sender: TObject);
begin
  with fneDataFileStub do
  begin
    if (pos('.', Text) = 0) and (pos('\', Text) = 0) then
    begin
      LogStub:= Text;
    end
    else
    begin
      LogStub:= ExtractFileName(Text);

      if pos('.', LogStub) <> 0 then
      begin
        LogExtension:= LogStub;
        LogStub:= Copy2SymbDel(LogExtension, '.');
        LogExtension:= '.' + LogExtension;
      end;
      DataFolder:= ExtractFileDir(Text);
      RootDir:= DataFolder;
      if pos(GetCurrentDir, DataFolder) <> 0 then
        delete(DataFolder, 1, length(GetCurrentDir) + 1);
      Text:= LogStub + LogExtension;                                            //see comment above
    end;
  end;
end;

procedure TDetControlForm.PanDragToolAfterMouseDown(ATool: TChartTool;
  APoint: TPoint);
begin
  pmChart.AutoPopup:= true;
end;

procedure TDetControlForm.PanDragToolAfterMouseMove(ATool: TChartTool;
  APoint: TPoint);
begin
  pmChart.AutoPopup:= false;
end;

procedure TDetControlForm.ParamsChange(Sender: TObject);
begin
  ParamsApplied:= false;
end;

procedure TDetControlForm.cbShowPointsChange(Sender: TObject);
begin
  Chart1LineSeries1.ShowPoints:= cbShowPoints.Checked;
  Chart2LineSeries1.ShowPoints:= cbShowPoints.Checked;
end;

procedure TDetControlForm.cbUseGenFreqChange(Sender: TObject);
begin
  UseGenFreq:= cbUseGenFreq.Checked;
  if UseGenFreq then
    LogFreq:= true;
end;

procedure TDetControlForm.cbXAxisChange(Sender: TObject);
begin
  Source1.Reset;
  Source2.Reset;
end;

procedure TDetControlForm.cgTransferItemClick(Sender: TObject; Index: integer);
var
  i, ParNum: word;
begin
  if (Index = ReferenceIndex) and not cgTransfer.Checked[Index] then
    if not UseGenFreq then
      LogFreq:= false;

  CheckTransferCount;
end;

{$R *.lfm}

end.

