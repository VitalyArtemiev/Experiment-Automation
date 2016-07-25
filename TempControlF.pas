unit TempControlF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TASources, TATools, TAGraph, TASeries, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, Buttons, StdCtrls, Spin, Menus,
  ComCtrls, EditBtn, Types, BaseConF, LogModule, TACustomSource, AxisSource;

type

  { TTempControlForm }

  TTempControlForm = class(tConnectionForm)
    btClear: TButton;
    btStartPauseLog: TButton;
    btStopLog: TButton;
    cbChartShow: TComboBox;
    cbReadingsMode: TComboBox;
    cbSampleRate: TComboBox;
    cbShowPoints: TCheckBox;
    cbXAxis: TComboBox;
    cgTransfer: TCheckGroup;
    Chart: TChart;
    ChartLineSeries: TLineSeries;
    ChartToolset: TChartToolset;
    DataPointHintTool: TDataPointHintTool;
    Divider: TMenuItem;
    eDelay: TSpinEdit;
    eUpdateInterval: TSpinEdit;
    deDataFileStub: TDirectoryEdit;
    Label1: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label29: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PanDragTool: TPanDragTool;
    pmChart: TPopupMenu;
    pnConnection: TPanel;
    pnGraphControl: TPanel;
    RestoreScale: TMenuItem;
    sbParamScroll: TScrollBox;
    Source: TUserDefinedChartSource;
    UpdateTimer: TTimer;
    ZoomDragTool: TZoomDragTool;
    ZoomIn: TMenuItem;
    ZoomOut: TMenuItem;

    procedure btApplyClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject); override;
    procedure btStartPauseLogClick(Sender: TObject);
    procedure btStopLogClick(Sender: TObject);
    procedure cbChartShowChange(Sender: TObject);
    procedure cbReadingsModeChange(Sender: TObject);
    procedure cbSampleRateChange(Sender: TObject);
    procedure cbShowPointsChange(Sender: TObject);
    procedure cgTransferItemClick(Sender: TObject; Index: integer);
    procedure DataPointHintToolHint(ATool: TDataPointHintTool;
      const APoint: TPoint; var AHint: String);
    procedure deDataFileStubAcceptDirectory(Sender: TObject; var Value: String);
    procedure deDataFileStubEditingDone(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btQueryClick(Sender: TObject); override;
    procedure ChartMenuItemClick(Sender: TObject);
    procedure pmChartPopup(Sender: TObject);
    procedure RestoreScaleClick(Sender: TObject);
    procedure SourceGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure ZoomInClick(Sender: TObject);
    procedure ZoomOutClick(Sender: TObject);
  private
    { private declarations }
    srcFreq, srcAmpl: TAxisSource;
    t: double;
    ReadingMode: eReadMode;
    MacroCrutch: string;

    iPA, iBu: array of string;

    procedure CheckTransferCount;
    //methods for logmodule events
    procedure BeforeStart(Sender: tLogModule);
    procedure Start(Sender: tLogModule);
    procedure Pause(Sender: tLogModule);
    procedure Continue(Sender: tLogModule);
    procedure Stop(Sender: tLogModule);
    //also createfile
    function SaveLog(Sender: tLogModule): integer;
    procedure StateChange(Sender: tLogModule);
    procedure ProcessBuffers(Sender: tLogModule);
  public
    { public declarations }
    LogStub, LogExtension, DataFolder: string;
    Log: tLogModule;

    ParToRead: array of shortint;
    LogTime, LogFreq, LogAmpl, OnePointPerStep, AutoApply: boolean;
    //for logmodule
    procedure CreateFile(Sender: tLogModule);

    procedure EnableControls(Enable: boolean); override;
    procedure GetDeviceParams; override;
    procedure AfterConnect; override;

    function RecvSnap(p: array of shortint): PBuffer; //not really a snap since not guaranteed simult
  end;

var
  TempControlForm: TTempControlForm;

implementation

uses
  Dateutils, StrUtils, TAChartUtils, DeviceF, MainF, StepF, DetControlF,
  ReadingThreads
  {$IFOPT D+}
  ,tlntsend
  {$ENDIF}
  ;

{$R *.lfm}

{ TTempControlForm }

procedure TTempControlForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  WriteProgramLog('Creating TempControl form');

  Top:= MainForm.Top;
  Left:= DetControlForm.Left + DetControlForm.Width + 8;

  btQuery.Caption:= 'Запрос' + LineEnding + 'текущих' + LineEnding + 'значений';
  btReset.Caption:= 'Сбросить' + LineEnding + '‌настройки'+ LineEnding + 'прибора';
  btCustomCommand.Caption:= 'Польз.' + LineEnding + 'команда';
  btStatus.Caption:= 'Состояние' + LineEnding + ' прибора';

  DisplayMessages:= true;
  ParamsApplied:= false;
  //MaxSimultPars:= -1;
  DeviceKind:= dTempController;
  DeviceIndex:= iDefaultDevice;
  ConnectionKind:= cNone;
  t:= 0;
  DataFolder:= DefaultLogFolder;
  LogStub:= '';
  //DrawnBuffers:= 0;
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

procedure TTempControlForm.FormDestroy(Sender: TObject);
begin
  Log.Stop;
  btClearClick(Self);

  btnDisconnectClick(Self);

  Log.Free;
  DoneCriticalSection(CommCS);
end;

procedure TTempControlForm.FormShow(Sender: TObject);
begin
  GetSupportedDevices(DeviceKind);
  Chart.Height:= DetControlForm.Chart1.Height;                                  //crutch to avoid misalignment in different OS's
end;

procedure TTempControlForm.btQueryClick(Sender: TObject);
var
  s: string;
  i: integer;
begin
  EnterCriticalSection(CommCS);
    Purge;
    AddCommand(tSampleRate, true);
    PassCommands;
    s:= RecvString;
  LeaveCriticalSection(CommCS);

  if CurrentDevice^.Model = 'PTC10' then  //FUCK PTC10 it returns strings different from those it takes
  begin
    for i:= 0 to high(iBu) do
      if AnsiContainsText(iBu[i], s) then
        cbSampleRate.ItemIndex:= i;
  end
  else
  begin
    i:= AnsiIndexText(s, iBu);
    if i>= 0 then
      cbSampleRate.ItemIndex:= i;
  end;
end;

procedure TTempControlForm.ChartMenuItemClick(Sender: TObject);
var
  cb: tComboBox;
  i: integer;
begin
  cb:= cbChartShow;

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

procedure TTempControlForm.pmChartPopup(Sender: TObject);
var
  i: integer;
begin
  with cbChartShow do
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

procedure TTempControlForm.SourceGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
var
  Src: TAxisSource;
begin
  case cbXAxis.ItemIndex of
    0: if LogTime then
         AItem.X:= Log.srcTime.Values[Aindex];
    1: if LogFreq then
         AItem.X:= srcFreq.Values[Aindex];
    2: if LogAmpl then
         AItem.X:= srcAmpl.Values[Aindex];
  end;
                    { TODO 3 -cFeature : show gen freq? }
  {if (ReadingMode = rBuffer) then
    i:= 0
  else
    i:= cbChartShow.ItemIndex;}

  Src:= Log.CoordinateSources[cbChartShow.ItemIndex];
  //try
  if Src <> nil then
    AItem.Y:= Src.Values[Aindex]
  else ASource.PointsNumber:= 0;
  {except
     on e:exception do writeprogramlog(e.message +' '+ strf(aindex)+' ' + strf(ASource.PointsNumber) +' '+ strf(src.count))
  end;}
end;

procedure TTempControlForm.UpdateTimerTimer(Sender: TObject);
begin
  Log.ProcessBuffers;
end;

procedure TTempControlForm.ZoomInClick(Sender: TObject);
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

procedure TTempControlForm.ZoomOutClick(Sender: TObject);
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

procedure TTempControlForm.CheckTransferCount;
var
  i, ParNum: integer;
begin
  ParNum:= 0;
  with cgTransfer do
  begin
    for i:= 0 to Items.Count - 1 do
      if Checked[i] then
        inc(ParNum);
    {if ParNum = MaxSimultPars then
    begin
      for i:= 0 to Items.Count - 1 do
        if not Checked[i] then
          CheckEnabled[i]:= false;
    end
    else
    for i:= 0 to Items.Count - 1 do
      CheckEnabled[i]:= true; }

    case ParNum of
      0:
        begin
          ShowMessage('Ошибка: не выбрано ни одного параметра');
          Checked[0]:= true;
        end;
      1:
        begin
          for i:= 0 to Items.Count - 1 do
            if Checked[i] then
              CheckEnabled[i]:= false
            else
              CheckEnabled[i]:= true;
        end;
      else
        for i:= 0 to Items.Count - 1 do
            CheckEnabled[i]:= true;
    end;

    if ParNum < 1 then
    begin

    end;
  end;
end;

procedure TTempControlForm.RestoreScaleClick(Sender: TObject);
begin
  tChart(pmChart.PopupComponent).ZoomFull(true);
end;

procedure TTempControlForm.BeforeStart(Sender: tLogModule);
var
  i, j: integer;
  //s: string;
begin
  with Sender do
  begin
    //btStartPauseLog.Caption:= 'Приостановить';  //to an event  onbeforestart
    WriteProgramLog('Data collection start');
    btClearClick(Self);

    if AutoApply and (StepForm.Finished or not TempControlForm.ParamsApplied) then  //skip if stepf is already going
    begin
      SetCursorAll(crHourGlass);
      TempControlForm.btApplyClick(Self);
      sleep(TempControlForm.eDelay.Value);
    end;
    SetCursorAll(crDefault);

    ReadingMode:= eReadMode(cbReadingsMode.ItemIndex);

    case ReadingMode of
      rBuffer:
        exit;
      rSimultaneous:
      begin
        LogTime:= true;

        j:= 0;
        with cgTransfer do
        begin
          for i:= 0 to Items.Count - 1 do  //count checked
            if Checked[i] then
              inc(j);
          setlength(ParToRead, j);
          j:= 0;

          for i:= 0 to Items.Count - 1 do //set partoread
          if Checked[i] then
          begin
            ParToRead[j]:= i;
            inc(j);
          end;
        end;

        setlength(CoordinateSources, cgTransfer.Items.Count);

        for j:= 0 to high(ParToRead) do
          CoordinateSources[ParToRead[j]]:= TAxisSource.Create(128);
      end;
      rRealTime:
      begin
        LogTime:= true;

        j:= 0;
        with cgTransfer do
        begin
          for i:= 0 to Items.Count - 1 do  //count checked
            if Checked[i] then
              inc(j);
          setlength(ParToRead, j);
          j:= 0;

          for i:= 0 to Items.Count - 1 do //set partoread
          if Checked[i] then
          begin
            ParToRead[j]:= i;
            inc(j);
          end;
        end;

        setlength(CoordinateSources, cgTransfer.Items.Count);

        for j:= 0 to high(ParToRead) do
          CoordinateSources[ParToRead[j]]:= TAxisSource.Create(128);

      end;
      {rBuffer:
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
      end; }
    end;

    if LogTime then
      srcTime:= TAxisSource.Create(128);
    if LogFreq then
      srcFreq:= TAxisSource.Create(128);
    if LogAmpl then
      srcAmpl:= TAxisSource.Create(128);

    if not OnePointPerStep then
    begin
      UpdateTimer.Interval:= eUpdateInterval.Value;
      UpdateTimer.Enabled:= true;
    end;

    t:= 0;
  end;
  {EnterCriticalSection(CommCS);  //dont even need it
    AddCommand(tGetChannelNames);
    PassCommands;
    s:= RecvString;
  LeaveCriticalSection(CommCS);}
end;

procedure TTempControlForm.Start(Sender: tLogModule);
var
  s: string;
  i: integer;
begin
  with Sender do
    case ReadingMode of   //onstart
      rBuffer:
      begin
        AddCommand(tResetStorage);
        AddCommand(tStartStorage);
        PassCommands;
        ReadingsThread:= nil;//tDetBufferThread.Create(PointsInBuffer);
      end;
      rSimultaneous:
      begin
        if OnePointPerStep then
          ReadingsThread:= nil //Assigned in stepform
        else
          ReadingsThread:= tTempSimultaneousThread.Create;
      end;
      rRealTime:
      begin
        if CurrentDevice^.Model = 'PTC10' then   //srsly, fuck ptc10. who coded this? can't transfer buffer other than 1 point at a time? also macros are shit. cant even form custom strings. i am not writing another command architecture to accomodate this bullshit.
        begin
          s:= '';
          for i:= 0 to high(ParToRead) do
            s+= 'getlog ' + cgTransfer.Items[ParToRead[i]] + ', next; ';
          s+= ' }';

          EnterCriticalSection(CommCS);
            AddCommand(tResetStorage);
            //AddCommand(tStartRealTime);
            commandstring+= '; name RTAMacro; while (1) { ' + s;
            MacroCrutch:= CommandString; //needed to continue
            PassCommands;
          LeaveCriticalSection(CommCS);
        end
        else
        begin
          EnterCriticalSection(CommCS);
            AddCommand(tResetStorage);
            AddCommand(tStartRealTime);
            PassCommands;
          LeaveCriticalSection(CommCS);
        end;

        ReadingsThread:= tTempRealTimeThread.Create;
      end;
    end;
end;

procedure TTempControlForm.Pause(Sender: tLogModule);
begin
  WriteProgramLog('Data collection pause');

  UpdateTimer.Enabled:= false;

  case ReadingMode of
    rBuffer:
    begin
      EnterCriticalSection(CommCS);
        AddCommand(tPauseStorage);
        PassCommands;
      LeaveCriticalSection(CommCS);
    end;
    //rSimultaneous: ;
    rRealTime:
    begin
      if CurrentDevice^.Model = 'PTC10' then
      begin
        EnterCriticalSection(CommCS);
          CommandString:= 'kill RTAMacro';
          PassCommands;
        LeaveCriticalSection(CommCS);
      end
      else
      begin
        EnterCriticalSection(CommCS);
          AddCommand(tPauseRealTime);
          PassCommands;
        LeaveCriticalSection(CommCS);
      end;
    end;
  end;
  {if ReadingMode = rBuffer then
  begin
    EnterCriticalSection(CommCS);
      AddCommand(dPauseStorage);
      PassCommands;
    LeaveCriticalSection(CommCS);
  end;   }
end;

procedure TTempControlForm.Continue(Sender: tLogModule);
begin
  WriteProgramLog('Data collection resume');

  UpdateTimer.Enabled:= true;
  {if ReadingMode = rBuffer then
  begin
    EnterCriticalSection(CommCS);
      AddCommand(dStartStorage);
      PassCommands;
    LeaveCriticalSection(CommCS);
  end; }

  with Sender do
    case ReadingMode of
      rBuffer:
      begin
        EnterCriticalSection(CommCS);
          AddCommand(tStartStorage);
          PassCommands;
        LeaveCriticalSection(CommCS);
        ReadingsThread:= tDetBufferThread.Create(0{PointsInBuffer});
      end;
      rSimultaneous:
      begin
        if OnePointPerStep then
          ReadingsThread:= nil // Stepf
        else
          ReadingsThread:= tTempSimultaneousThread.Create;
      end;
      rRealTime:
      begin
        if CurrentDevice^.Model = 'PTC10' then
        begin
          EnterCriticalSection(CommCS);
            CommandString:= MacroCrutch;
            PassCommands;
          LeaveCriticalSection(CommCS);
        end
        else
        begin
          EnterCriticalSection(CommCS);
            AddCommand(tStartRealTime);
            PassCommands;
          LeaveCriticalSection(CommCS);
        end;

        ReadingsThread:= tTempRealTimeThread.Create;
      end;
    end;
end;

procedure TTempControlForm.Stop(Sender: tLogModule);
begin
  UpdateTimer.Enabled:= false;    //to event    OnStop

  case ReadingMode of
    rBuffer:
    begin
      EnterCriticalSection(CommCS);
        AddCommand(dResetStorage);
        PassCommands;
      LeaveCriticalSection(CommCS);
    end;
   // rSimultaneous: ;
    rRealTime:
    begin
      if CurrentDevice^.Model = 'PTC10' then
      begin
        EnterCriticalSection(CommCS);
          CommandString:= 'kill RTAMacro';
          PassCommands;
        LeaveCriticalSection(CommCS);
      end
      else
      begin
        EnterCriticalSection(CommCS);
          AddCommand(tStopRealTime);
          PassCommands;
        LeaveCriticalSection(CommCS);
      end;
    end;
  end;

  {if (ReadingMode = rBuffer) or (ForceStop and (Log.State = lInActive)) then
  begin
    EnterCriticalSection(CommCS);
      AddCommand(dResetStorage);
      PassCommands;
    LeaveCriticalSection(CommCS);
    ForceStop:= false;
  end;   }

  OnePointPerStep:= false;

  if (Log.State <> lInActive) and AutoApply then
  begin
    if Config.AutoReport  then
      MainForm.SaveReport(false, MainForm.ReportHeader);
    //to avoid increasing two times
    inc(ExperimentNumber);
  end;
end;

procedure TTempControlForm.CreateFile(Sender: tLogModule);
var
  i: integer;
begin
  with Sender do
  begin
    FilePath:= DataFolder;
    DateTimeToString(FileName, DefaultTimeFormat, Now);
    Stub:= LogStub;
    if LogExtension <> '' then
      Extension:= LogExtension
    else
      Extension:= LogExtensions[integer(dTempController)];

    if LogTime then
      Header:= Header + cbXAxis.Items.Strings[0] + HT;
    if LogFreq then
      Header:= Header + 'Частота (генератор)' + HT;
    if LogAmpl then
      Header:= Header + 'Амплитуда (генератор)' + HT;

    for i:= 0 to high(ParToRead) do
      Header:= Header + cbChartShow.Items[ParToRead[i]] + HT;

    Header:= Header + LineEnding;
  end;
end;

function TTempControlForm.SaveLog(Sender: tLogModule): integer;
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

    if LogFreq then
    begin
      setlength(Sources, length(Sources) + 1);
      Sources[high(Sources)]:= SrcFreq;
    end;

    if LogAmpl then
    begin
      setlength(Sources, length(Sources) + 1);
      Sources[high(Sources)]:= SrcAmpl;
    end;

    {case ReadingMode of
      rBuffer:
      begin
        setlength(Sources, length(Sources) + 2);
        Sources[high(Sources) - 1]:= CoordinateSources[0];
        Sources[high(Sources)]:= CoordinateSources[1];
      end;
      rSimultaneous:
      begin }
        j:= length(Sources);
        setlength(Sources, length(Sources) + length(ParToRead));
        for i:= 0 to length(ParToRead) - 1 do
          Sources[j + i]:= CoordinateSources[ParToRead[i]];
      {end;
    end; }

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

procedure TTempControlForm.StateChange(Sender: tLogModule);
begin
  with tLogModule(Sender) do
    if State = lActive then
    begin
      btStartPauseLog.Caption:= PauseCaption;
      btReset.Enabled:=           false;
      cbReadingsMode.Enabled:=    false;
      cbSampleRate.Enabled:=      false;
      cgTransfer.Enabled:=        false;
      btClear.Enabled:=           false;
      btApply.Enabled:=           false;
      pnConnection.Enabled:=      false;
      cbSampleRate.Enabled:=      false;
    end
    else
    begin
      btReset.Enabled:=           true;
      cbReadingsMode.Enabled:=    true;
      cbSampleRate.Enabled:=      true;
      cgTransfer.Enabled:=        true;
      btApply.Enabled:=           true;
      cbSampleRate.Enabled:=      true;
      if State = lInActive then
      begin
        pnConnection.Enabled:=    true;
        btClear.Enabled:=         true;
        btStartPauseLog.Caption:= StartCaption;
      end
      else
        btStartPauseLog.Caption:= ContinueCaption;
    end;
end;

procedure TTempControlForm.ProcessBuffers(Sender: tLogModule);
var
  i, j: longint;
  v, f, a: double;
begin
  with Sender do
  begin
    if LogFreq then
      f:= StepF.F;
    if LogAmpl then
      a:= StepF.A;

    if DataList.Count > 0 then
    case ReadingMode of {
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
      end; }

      rSimultaneous:
      begin
        for i:= 0 to (DataList.Count div 2) - 1 do
        begin
          if LogFreq then
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
            CoordinateSources[ParToRead[j]].Add(v);

            {for k:= (lk + 1) to high(ReadPars) do
            begin
              //WriteProgramLog('lk ' + strf(lk)+' k '+strf(k));
              if ReadPars[k] then
              begin
                //writeprogramlog('k' + strf(k));
                CoordinateSources[k].Add(v);
                lk:= k;
                break;
              end;
            end;}
          end;

          dispose(PBuffer(DataList.Items[i * 2 + 1]));
        end;
        DataList.Clear;

        if LogTime then
          ProcessedPoints:= srcTime.Count
        else
        if LogFreq then
          ProcessedPoints:= srcFreq.Count
        else
          ProcessedPoints:= srcAmpl.Count;
      end;

      rRealTime:
      begin
         for i:= 0 to DataList.Count - 1 do
        begin
          if LogFreq then
            srcFreq.Add(f);

          if LogAmpl then
            srcAmpl.Add(a);

          if LogTime then
            srcTime.Add(t);

          for j:= 0 to high(Buffer(DataList.Items[i]^)) do
          begin
            v:= Buffer(DataList.Items[i]^)[j];

            //WriteProgramLog('');
            //WriteProgramLog('i ' + strf(i) + ' j ' + strf(j));
            CoordinateSources[ParToRead[j]].Add(v);

            {for k:= (lk + 1) to high(ReadPars) do
            begin
              //WriteProgramLog('lk ' + strf(lk)+' k '+strf(k));
              if ReadPars[k] then
              begin
                //writeprogramlog('k' + strf(k));
                CoordinateSources[k].Add(v);
                lk:= k;
                break;
              end;
            end;}
          end;

          dispose(PBuffer(DataList.Items[i]));
          t+= 1;
        end;

        DataList.Clear;

        if LogTime then
          ProcessedPoints:= srcTime.Count
        else
        if LogFreq then
          ProcessedPoints:= srcFreq.Count
        else
          ProcessedPoints:= srcAmpl.Count;
      end;
    end;

    Source.PointsNumber:= ProcessedPoints;
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

procedure TTempControlForm.EnableControls(Enable: boolean);
begin
  cbSampleRate.Enabled:=    Enable;
  //cgTransfer.Enabled:=      Enable;
  btReset.Enabled:=         Enable;
  btQuery.Enabled:=         Enable;
  btCustomCommand.Enabled:= Enable;
  btStatus.Enabled:=        Enable;
  btApply.Enabled:=         Enable;
  btStartPauseLog.Enabled:= Enable;
  btStopLog.Enabled:=       Enable;
end;

procedure TTempControlForm.GetDeviceParams;
var
  i, c: integer;
begin
  {$IFOPT D+}
  if (DeviceIndex =0) and debug then
  begin
    deviceindex:= 1;
    connectionkind:= ctelnet;
    telnetclient:= tTelNetSend.create;
  end;
  {$ENDIF}

  for i:= 4 to pmChart.Items.Count - 1 do
    pmChart.Items.Delete(4);

  with DeviceForm.sgTempCommands do
  begin
    SeparateIndices(Cells[DeviceIndex, integer(hChannelList)], cgTransfer.Items, iPa);

    if cgTransfer.Width < 120 then
    begin
      cgTransfer.Columns:= 2;
    end
    else
    begin
      cgTransfer.Columns:= 1;
    end;

    cbChartShow.Items:= cgTransfer.Items;

    for i:= 0 to cgTransfer.Items.Count - 1 do
    with pmChart do
    begin
      Items.Add(tMenuItem.Create(pmChart));
      Items[Items.Count - 1].Caption:= cgTransfer.Items[i];
      Items[Items.Count - 1].OnClick:= @ChartMenuItemClick;
    end;
    i:= integer(cbSampleRate.Items);

    SeparateIndices(Cells[DeviceIndex, integer(htSampleRateOptions)], cbSampleRate.Items, iBu);

    MinDelay:= valf(Cells[DeviceIndex, integer(hMinDelay)]);
    eDelay.MinValue:= MinDelay;
  end;

  c:= 0;
  for i:= 0 to cgTransfer.Items.Count - 1 do
    if cgTransfer.Checked[i] then
      inc(c);

  if c < 1 then
  begin
    cgTransfer.Checked[0]:= true;
  end;

  deDataFileStubEditingDone(Self);
end;

procedure TTempControlForm.AfterConnect;
begin
  inherited AfterConnect;

  Params.DetectorPort:= DetControlForm.CurrentDevice^.Port;
  Params.LastDetector:= DetControlForm.CurrentDevice^.Model;

  cbReadingsModeChange(Self);
  CheckTransferCount;
end;

function TTempControlForm.RecvSnap(p: array of shortint): PBuffer;
var
  s: string;
  i, j, k: integer;
  v: double;
begin
  try
    EnterCriticalSection(CommCS);
      AddCommand(tReadSimultaneous, true);
      PassCommands;
      s:= RecvString;
  finally
    LeaveCriticalSection(CommCS);
  end;

  {if debug then
  begin
  s:= '25.25, 26.26, 30.30, Nan, 50.50';
  sleep(60 + random(30));
  end;    }

  if s = '' then
  begin
    Result:= nil;
    exit;
  end;

  new(Result);
  setlength(Result^, Length(p));
  i:= 0;
  j:= 0;

  k:= pos(CurrentDevice^.ParSeparator, s);
  repeat
    if i = p[j] then
    begin
      val(copy(s, 1, k - 1), v);
      Result^[j]:= v;
      inc(j);
    end;
    delete(s, 1, k);
    k:= pos(CurrentDevice^.ParSeparator, s);
    inc(i);
  until (k = 0) or (j = length(p));
  if j < length(p) then
    val(s, Result^[j]);
end;

procedure TTempControlForm.btnConnectClick(Sender: TObject);
begin
  with MainForm do
  if cbPortSelect.ItemIndex = TempControlForm.cbPortSelect.ItemIndex then
  begin
    case ConnectionKind of
      cSerial:
      begin
        showmessage('К данному порту уже осуществляется подключение');
        exit
      end;
      cTelNet:
        if (CurrentDevice^.Host = TempcontrolForm.CurrentDevice^.Host) and
           (CurrentDevice^.Port = TempcontrolForm.CurrentDevice^.Port) then
          begin
            showmessage('По данному адресу уже осуществляется подключение');
            exit
          end;
    end;
  end;

  with DetControlForm do
  if cbPortSelect.ItemIndex = TempControlForm.cbPortSelect.ItemIndex then
  begin
    case ConnectionKind of
      cSerial:
      begin
        showmessage('К данному порту уже осуществляется подключение');
        exit
      end;
      cTelNet:
        if (CurrentDevice^.Host = TempcontrolForm.CurrentDevice^.Host) and
           (CurrentDevice^.Port = TempcontrolForm.CurrentDevice^.Port) then
          begin
            showmessage('По данному адресу уже осуществляется подключение');
            exit
          end;
    end;
  end;

  inherited btnConnectClick(Sender);
end;

procedure TTempControlForm.btClearClick(Sender: TObject);
begin
  Source.PointsNumber:= 0;
  Log.Clear;

  freeandnil(srcFreq);
  freeandnil(srcAmpl);

  StatusBar.Panels[spStatus].Text:= '';
end;

procedure TTempControlForm.btApplyClick(Sender: TObject);
begin
  EnterCriticalSection(CommCS);
    AddCommand(tSampleRate, false, iBu[cbSampleRate.ItemIndex]);
    PassCommands;
  LeaveCriticalSection(CommCS);
  ParamsApplied:= true;
end;

procedure TTempControlForm.btStartPauseLogClick(Sender: TObject);
begin
  AutoApply:= true;
  Log.Toggle;
end;

procedure TTempControlForm.btStopLogClick(Sender: TObject);
begin
  Log.Stop;
end;

procedure TTempControlForm.cbChartShowChange(Sender: TObject);
begin
  {if (LogState = lActive) and (ReadingMode = rBuffer) then
  with cbChartShow do
  begin
    ItemIndex:= Items.IndexOf(cbCH1.Text);
    if ItemIndex < 0 then
      ItemIndex:= CH1Index;
  end;
  }
  Source.PointsNumber:= Log.ProcessedPoints;
  Source.Reset;
end;

procedure TTempControlForm.cbReadingsModeChange(Sender: TObject);
begin
  case cbReadingsMode.ItemIndex of
    integer(rBuffer):
    begin
      //ShowMessage('В разработке');
      //cbReadingsMode.ItemIndex:= integer(rSimultaneous);
      //MainForm.cbPointPerStepTemp.Checked:= false;
      if not CommandSupported(tStartStorage) then   { TODO 1 -cImprovement : Theese should be everywhere }
      begin
        ShowMessage('Режим не поддерживается прибором');
        cbReadingsMode.ItemIndex:= integer(rSimultaneous);
        exit;
      end;
      MainForm.cbPointPerStepTemp.Checked:= false;
      cbXAxis.Items.Strings[0]:= 'Номер точки';
    end;
    integer(rSimultaneous):
    begin
      cbXAxis.Items.Strings[0]:= 'Время, с';
    end;
    integer(rRealTime):
    begin
      MainForm.cbPointPerStepTemp.Checked:= false;
      cbXAxis.Items.Strings[0]:= 'Номер точки';
    end;
  end;
end;

procedure TTempControlForm.cbSampleRateChange(Sender: TObject);
begin
  ParamsApplied:= false;
end;

procedure TTempControlForm.cbShowPointsChange(Sender: TObject);
begin
  ChartLineSeries.ShowPoints:= cbShowPoints.Checked;
end;

procedure TTempControlForm.cgTransferItemClick(Sender: TObject; Index: integer);
begin
  CheckTransferCount;
end;

procedure TTempControlForm.DataPointHintToolHint(ATool: TDataPointHintTool;
  const APoint: TPoint; var AHint: String);
var
  s: string;
  Point: TDoublePoint;
begin
  AHint:= cbChartShow.Text;
  Point:= Atool.NearestGraphPoint;
  if cbXAxis.Text = 'Номер точки' then
    str(trunc(Point.X), s)
  else
    str(Point.X:0:6, s);

  AHint:= ' ' + cbXAxis.Text + ': ' + s + '; ' + AHint + ': ';
  str(Point.Y:0:0, s);
  AHint+= s;
end;

procedure TTempControlForm.deDataFileStubAcceptDirectory(Sender: TObject;
  var Value: String);
begin
  DataFolder:= Value;       //i modified lcl (tdirectoryedit) in order for this to work. property text used to be the same as directory, which, i find, is inconsistent. i added a variable fdirectory and modified getdirectory and setdirectory, replacing ftext with fdirectory.
  if pos(GetCurrentDir, DataFolder) <> 0 then
    delete(DataFolder, 1, length(GetCurrentDir) + 1);
end;

procedure TTempControlForm.deDataFileStubEditingDone(Sender: TObject);
begin
  with deDataFileStub do
  begin
    if (pos(ExtensionSeparator, Text) = 0) and (pos(DirectorySeparator, Text) = 0) then
    begin
      LogStub:= Text;
    end
    else
    begin
      LogStub:= ExtractFileName(Text);

      if pos(ExtensionSeparator, LogStub) <> 0 then
      begin
        LogExtension:= LogStub;
        LogStub:= Copy2SymbDel(LogExtension, ExtensionSeparator);
        LogExtension:= ExtensionSeparator + LogExtension;
      end;
      DataFolder:= ExtractFileDir(Text);
      RootDir:= DataFolder;
      if pos(GetCurrentDir, DataFolder) <> 0 then
        delete(DataFolder, 1, length(GetCurrentDir) + 1);
      Text:= LogStub + LogExtension; //see comment above
    end;
  end;
end;

procedure TTempControlForm.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
begin
  Log.Stop;
  MainForm.miShowTempControlF.Checked:= false;
end;

end.

