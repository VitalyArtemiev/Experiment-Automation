unit TempControlF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TASources, TATools, TAGraph, TASeries, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, Buttons, StdCtrls, Spin, Menus,
  ComCtrls, EditBtn, Types, SerConF, LogModule, TACustomSource, AxisSource;

type

  { TTempControlForm }

  TTempControlForm = class(TSerConnectForm)
    btClear: TButton;
    btStartPauseLog: TButton;
    btStopLog: TButton;
    cbChartShow: TComboBox;
    cbReadingsMode: TComboBox;
    cbSampleRate: TComboBox;
    cbShowPoints: TCheckBox;
    cbUseGenFreq: TCheckBox;
    cbXAxis: TComboBox;
    cgTransfer: TCheckGroup;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    ChartToolset: TChartToolset;
    DataPointHintTool: TDataPointHintTool;
    Divider: TMenuItem;
    eAxisLimit: TFloatSpinEdit;
    eDelay: TSpinEdit;
    eUpdateInterval: TSpinEdit;
    Label1: TLabel;
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

    procedure btClearClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject); override;
    procedure btStartPauseLogClick(Sender: TObject);
    procedure btStopLogClick(Sender: TObject);
    procedure cbChartShowChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btQueryClick(Sender: TObject); override;
    procedure ChartMenuItemClick(Sender: TObject);
    procedure PanDragToolAfterMouseDown(ATool: TChartTool; APoint: TPoint);
    procedure PanDragToolAfterMouseMove(ATool: TChartTool; APoint: TPoint);
    procedure pmChartPopup(Sender: TObject);
    procedure SourceGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure UpdateTimerTimer(Sender: TObject);
  private
    { private declarations }
    srcFreq, srcAmpl: TAxisSource;

    procedure BeforeStart(Sender: tLogModule);
    procedure Start(Sender: tLogModule);
    procedure Pause(Sender: tLogModule);
    procedure Continue(Sender: tLogModule);
    procedure Stop(Sender: tLogModule);
    procedure CreateFile(Sender: tLogModule);
    function SaveLog(Sender: tLogModule): integer;
    procedure StateChange(Sender: tLogModule);
    procedure ProcessBuffers(Sender: tLogModule);
  public
    { public declarations }
    Log: tLogModule;

    LogTime, LogFreq, LogAmpl, OnePointPerStep: boolean;

    procedure EnableControls(Enable: boolean); override;
    procedure GetDeviceParams; override;

    //function RecvSnap(p: array of shortint): PBuffer; //not really a snap since not guaranteed simult
  end;

var
  TempControlForm: TTempControlForm;

implementation

uses                                  //remove
  DeviceF, MainF, ReadingsF, OptionF, tlntsend;

{$R *.lfm}

{ TTempControlForm }

procedure TTempControlForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  WriteProgramLog('Creating TempControl form');

  Top:= MainForm.Top;
  Left:= ReadingsForm.Left + ReadingsForm.Width + 8;

  btQuery.Caption:= 'Запрос' + LineEnding + 'текущих' + LineEnding + 'значений';
  btReset.Caption:= 'Сбросить' + LineEnding + '‌настройки'+ LineEnding + 'прибора';
  btCustomCommand.Caption:= 'Польз.' + LineEnding + 'команда';
  btStatus.Caption:= 'Состояние' + LineEnding + ' прибора';

  //MaxSimultPars:= -1;
  DeviceKind:= dTempController;
  DeviceIndex:= iDefaultDevice;
  ConnectionKind:= cNone;
  //DrawnBuffers:= 0;
  //t:= 0;

  //LogFreq:= false;
  //LogTime:= true;
  //LogAmpl:= false;

  for i:= 0 to PortCount - 1 do
    cbPortSelect.AddItem(MainForm.cbPortSelect.Items[i], nil);

  if cbPortSelect.ItemIndex < 0 then cbPortSelect.ItemIndex:= 0;

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
  SerPort.Free;
  TelNetClient.Free;
  Log.Free;
  DoneCriticalSection(CommCS);
end;

procedure TTempControlForm.FormShow(Sender: TObject);
begin
  GetSupportedDevices(DeviceKind);
end;

procedure TTempControlForm.btQueryClick(Sender: TObject);
begin

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

procedure TTempControlForm.PanDragToolAfterMouseDown(ATool: TChartTool;   { TODO 2 -cImprovement : update tachart to get rid of this }
  APoint: TPoint);
begin
  pmChart.AutoPopup:= true;
end;

procedure TTempControlForm.PanDragToolAfterMouseMove(ATool: TChartTool;
  APoint: TPoint);
begin
  pmChart.AutoPopup:= false;
end;

procedure TTempControlForm.pmChartPopup(Sender: TObject);
var
  i: integer;
  cb: tCombobox;
begin
  cb:= cbChartShow;

  with cb do
  begin
    if ItemIndex >= 0 then
    begin
      for i:= 0 to pmChart.Items.Count - 1 do
        pmChart.Items[i].Checked:= false;
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
  try                          { TODO -cImprovement : Get rid of try and optimize in general }
  if Src <> nil then
    AItem.Y:= Src.Values[Aindex]
  else ASource.PointsNumber:= 0;
  except
     on e:exception do writeprogramlog(e.message +' '+ strf(aindex)+' ' + strf(ASource.PointsNumber) +' '+ strf(src.count))
  end;
end;

procedure TTempControlForm.UpdateTimerTimer(Sender: TObject);
begin
  Log.ProcessBuffers;
end;

procedure TTempControlForm.BeforeStart(Sender: tLogModule);
begin

end;

procedure TTempControlForm.Start(Sender: tLogModule);
begin

end;

procedure TTempControlForm.Pause(Sender: tLogModule);
begin
  WriteProgramLog('Data collection pause');

  UpdateTimer.Enabled:= false;
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

end;

procedure TTempControlForm.Stop(Sender: tLogModule);
begin

end;

procedure TTempControlForm.CreateFile(Sender: tLogModule);
begin

end;

function TTempControlForm.SaveLog(Sender: tLogModule): integer;
begin

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
      cbUseGenFreq.Enabled:=      false;
      btClear.Enabled:=           false;
      btApply.Enabled:=           false;
      pnConnection.Enabled:=      false;
    end
    else
    begin
      btReset.Enabled:=           true;
      cbReadingsMode.Enabled:=    true;
      cbSampleRate.Enabled:=      true;
      cgTransfer.Enabled:=        true;
      cbUseGenFreq.Enabled:=      true;
      btApply.Enabled:=           true;
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
begin

end;

procedure TTempControlForm.EnableControls(Enable: boolean);
begin
  cgTransfer.Enabled:=      Enable;
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
  i: integer;
begin
  cgTransfer.Items.Clear;
  cbChartShow.Items.Clear;

  for i:= 4 to pmChart.Items.Count - 1 do
    pmChart.Items.Delete(4);

  with DeviceForm.sgTempCommands do
  begin
    cgTransfer.Items.AddText(Cells[DeviceIndex, integer(hChannelList)]);

    if cgTransfer.Width < 90 then
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

    MinDelay:= valf(Cells[DeviceIndex, integer(hMinDelay)]);
    eDelay.MinValue:= MinDelay;

    {TotalPars:= cgTransfer.Items.Count;
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
    end;}
  end;

  {if OffsetForm.GetParams > 0 then
    btOffset.Show
  else
    btOffset.Hide;  }
end;

procedure TTempControlForm.btnConnectClick(Sender: TObject);
var
  i : word;
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

  OptionForm.TabControl.TabIndex:= 1;
  OptionForm.DevicePage.TabIndex:= 2;

  inherited btnConnectClick(Sender);

  if Debug then
  if DeviceIndex = 0 then
  begin
   deviceindex:= 1;
   connectionkind:= ctelnet;
   telnetclient:= tTelNetSend.create;
  end;

  if DeviceIndex = iDefaultDevice then
    exit;

  Params.DetectorPort:= ReadingsForm.CurrentDevice^.Port;
  Params.LastDetector:= ReadingsForm.CurrentDevice^.Model;

  OptionForm.eDevice2.ItemIndex:= DeviceIndex - 1;
end;

procedure TTempControlForm.btClearClick(Sender: TObject);
begin
  Log.Clear;
end;

procedure TTempControlForm.btStartPauseLogClick(Sender: TObject);
begin
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
  Source.PointsNumber:= ProcessedPoints;
  Source.Reset;}
end;

procedure TTempControlForm.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
begin
  Log.Stop;
  MainForm.miShowTempControlF.Checked:= false;
end;

end.

