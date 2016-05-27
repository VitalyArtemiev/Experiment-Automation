unit TempControlF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TASources, TATools, TAGraph, TASeries, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, Buttons, StdCtrls, Spin, Menus,
  ComCtrls, SerConF, Types;

type

  { TTempControlForm }

  TTempControlForm = class(TSerConnectForm)
    btClear: TButton;
    btStartPauseLog: TButton;
    btStopLog: TButton;
    cbChart1Show: TComboBox;
    cbReadingsMode: TComboBox;
    cbSampleRate: TComboBox;
    cbShowPoints: TCheckBox;
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
    Source1: TUserDefinedChartSource;
    Source2: TUserDefinedChartSource;
    UpdateTimer: TTimer;
    ZoomDragTool: TZoomDragTool;
    ZoomIn: TMenuItem;
    ZoomOut: TMenuItem;

    procedure btnConnectClick(Sender: TObject); override;
    procedure cbChart1ShowChange(Sender: TObject);
    procedure cgTransferClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ChartMenuItemClick(Sender: TObject);
    procedure PanDragToolAfterMouseDown(ATool: TChartTool; APoint: TPoint);
    procedure PanDragToolAfterMouseMove(ATool: TChartTool; APoint: TPoint);
    procedure pmChartPopup(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure EnableControls(Enable: boolean); override;
  end;

var
  TempControlForm: TTempControlForm;

implementation

uses
  DeviceF, MainF, ReadingsF, OptionF;

{$R *.lfm}

{ TTempControlForm }

procedure TTempControlForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  WriteProgramLog('Creating TempControl form');

  Top:= MainForm.Top;
  Left:= ReadingsForm.Left + ReadingsForm.Width;

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

  //Threadlist:= TThreadList.Create;
  InitCriticalSection(CommCS);
  {InitCriticalSection(RNCS);
  InitCriticalSection(TimeCS);
  InitCriticalSection(OPSCS);
  InitCriticalSection(RTDCS);}
end;

procedure TTempControlForm.FormShow(Sender: TObject);
begin
  GetSupportedDevices(DeviceKind);
end;

procedure TTempControlForm.ChartMenuItemClick(Sender: TObject);
var
  cb: tComboBox;
  i: integer;
begin
  cb:= cbChart1Show;

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

procedure TTempControlForm.PanDragToolAfterMouseDown(ATool: TChartTool;
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
  cb:= cbChart1Show;

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

procedure TTempControlForm.EnableControls(Enable: boolean);
begin

end;

procedure TTempControlForm.btnConnectClick(Sender: TObject);
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

  OptionForm.TabControl.TabIndex:= 1;
  OptionForm.DevicePage.TabIndex:= 2;

  inherited btnConnectClick(Sender);

  {$IFOPT D+}
  if DeviceIndex = 0 then
  begin
   deviceindex:= 1;
   connectionkind:= cserial;
  end;
  {$ENDIF}


  if DeviceIndex = iDefaultDevice then exit;

  Params.DetectorPort:= ReadingsForm.CurrentDevice^.Port;
  Params.LastDetector:= ReadingsForm.CurrentDevice^.Model;

  OptionForm.eDevice2.ItemIndex:= DeviceIndex - 1;

  {cgTransfer.Items.Clear;
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
  cbInputRange.Items.Clear;  }
  for i:= 4 to pmChart.Items.Count - 1 do
    pmChart.Items.Delete(4);

  with DeviceForm.sgTempCommands do
  begin
    {cgTransfer.Items.AddText(Cells[DeviceIndex, integer(hTransferParams)]);
    cbChart1Show.Items:= cgTransfer.Items;
    cbChart2Show.Items:= cgTransfer.Items; }
    for i:= 0 to cgTransfer.Items.Count - 1 do
    with pmChart do
    begin
      Items.Add(tMenuItem.Create(pmChart));
      Items[Items.Count - 1].Caption:= cgTransfer.Items[i];
      Items[Items.Count - 1].OnClick:= @ChartMenuItemClick;
    end;
    {cbCh1.Items.AddText(Cells[DeviceIndex, integer(hCH1Options)]);
    cbCh2.Items.AddText(Cells[DeviceIndex, integer(hCH2Options)]);
    cbRatio1.Items.AddText(Cells[DeviceIndex, integer(hRatio1Options)]);
    cbRatio2.Items.AddText(Cells[DeviceIndex, integer(hRatio2Options)]);
    cbSensitivity.Items.AddText(Cells[DeviceIndex, integer(hSensitivityOptions)]);
    cbTimeConstant.Items.AddText(Cells[DeviceIndex, integer(hTimeConstOptions)]);
    cbReserve1.Items.AddText(Cells[DeviceIndex, integer(hCloseReserveOptions)]);
    cbReserve2.Items.AddText(Cells[DeviceIndex, integer(hWideReserveOptions)]);
    cbInputRange.Items.AddText(Cells[DeviceIndex, integer(hRangeOptions)]);  }

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

  with Params do
  begin
    {cbSensitivity.ItemIndex:= Sensitivity;
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
      cgTransfer.Checked[i]:= TransferPars[i]; }
  end;

 { if cbRatio1.ItemIndex < 0 then
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
  end;    }
end;

procedure TTempControlForm.cbChart1ShowChange(Sender: TObject);
begin
  {if (LogState = lActive) and (ReadingMode = rBuffer) then
  with cbChart1Show do
  begin
    ItemIndex:= Items.IndexOf(cbCH1.Text);
    if ItemIndex < 0 then
      ItemIndex:= CH1Index;
  end;
  Source1.PointsNumber:= ProcessedPoints;
  Source1.Reset;}
end;

procedure TTempControlForm.cgTransferClick(Sender: TObject);
begin

end;



end.

