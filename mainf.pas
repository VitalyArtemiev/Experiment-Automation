unit MainF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ComCtrls, DbCtrls, Spin, ExtCtrls, Buttons, Synaser, SerConF;

type
  { TMainForm }

  TMainForm = class(TSerConnectForm)
    btProgram: TSpeedButton;

    btTrigger: TButton;
    cbFuncSelect: TComboBox;
    cbImpedance: TComboBox;
    cbACEnable: TCheckBox;

    cbSweepRate: TCheckBox;
    cbSweepType: TComboBox;
    cbSweepDirection: TComboBox;
    cbPointPerStep: TCheckBox;
    cbAmplUnit: TComboBox;
    DebugBox: TMemo;
    eAmplitude: TFloatSpinEdit;
    eSweepStartF: TFloatSpinEdit;
    eSweepStopF: TFloatSpinEdit;
    eOffset: TFloatSpinEdit;
    eFrequency: TFloatSpinEdit;
    eSweepRate: TFloatSpinEdit;
    eStepStartF: TFloatSpinEdit;
    eStepStopF: TFloatSpinEdit;
    eFStep: TFloatSpinEdit;
    eStepStartA: TFloatSpinEdit;
    eStepStopA: TFloatSpinEdit;
    eAStep: TFloatSpinEdit;
    eTimeStep: TSpinEdit;
    FrequencyTab: TPageControl;
    Label10: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label28: TLabel;
    Label29: TLabel;

    Label30: TLabel;

    About: TMenuItem;

    miShowReadingsF: TMenuItem;

    miSavePar: TMenuItem;
    miLoadPar: TMenuItem;
    miOptions: TMenuItem;
    OpenDialog: TOpenDialog;
    pnConnection: TPanel;
    SaveDialog: TSaveDialog;
    ReadingTimer: TTimer;
    eDelay: TSpinEdit;


    TotalTime: TLabel;
    NewReport: TMenuItem;
    miExportParams: TMenuItem;
    SweepStartFReading: TLabel;
    SweepStopFReading: TLabel;
    Label1: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;

    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;

    Label9: TLabel;
    miSaveCfg: TMenuItem;
    miLoadCfg: TMenuItem;
    OffsetReading: TLabel;
    FrequencyReading: TLabel;

    AmplitudeReading: TLabel;

    MainMenu1: TMainMenu;
    miFile: TMenuItem;
    miAbout: TMenuItem;
    miView: TMenuItem;
    SweepRateReading: TLabel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure AboutClick(Sender: TObject);
    procedure cbPointPerStepChange(Sender: TObject);
    procedure cbAmplUnitChange(Sender: TObject);
    procedure DebugBoxClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure btnConnectClick(Sender: TObject); override;

    procedure EnableControls(Enable: boolean); override;

    procedure btApplyClick(Sender: TObject);
    procedure btQueryClick(Sender: TObject); override;
    procedure btProgramClick(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure btTriggerClick(Sender: TObject);
    procedure cbACEnableChange(Sender: TObject);
    procedure cbFuncSelectChange(Sender: TObject);
    procedure cbImpedanceChange(Sender: TObject);
    procedure cbSweepTypeChange(Sender: TObject);
   // procedure DebugBoxClick(Sender: TObject);
    procedure eOffsetChange(Sender: TObject);
    procedure eAmplitudeChange(Sender: TObject);
    procedure eStepChange(Sender: TObject);
    procedure eSweepStartFChange(Sender: TObject);
    procedure eSweepStopFChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miLoadParClick(Sender: TObject);
    procedure miOptionsClick(Sender: TObject);
    procedure miSaveParClick(Sender: TObject);
    procedure miShowReadingsFClick(Sender: TObject);
    procedure NewReportClick(Sender: TObject);
    procedure miExportParamsClick(Sender: TObject);
    procedure miLoadCfgClick(Sender: TObject);
    procedure miSaveCfgClick(Sender: TObject);
    procedure ReadingTimerStartTimer(Sender: TObject);
    procedure ReadingTimerTimer(Sender: TObject);

    //procedure ListBox1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }

    function SaveParams(FileName: ansistring): word;
    function LoadParams(FileName: ansistring): word;
    function SaveConfig(FileName: ansistring): word;
    function LoadConfig(FileName: ansistring): word;
    function ExportParams(Manual: boolean; Header: boolean = false): word;
  end;

  FunctionType = (Sine, Square, Triangle, Ramp, Noise);

const

  iDefaultDevice = 0;
  iDS345 = 1;
  iDS335 = 2;
  iSR844 = 3;
  iSR830 = 4;

  SupportedDevices: array [0..4] of tDevice =
    (
     (Manufacturer: '';                          Model: '';      Commands: nil; ParSeparator: ','; CommSeparator: ';'; Terminator: CR),
     (Manufacturer: 'StanfordResearchSystems';   Model: 'DS345'; Commands: nil; ParSeparator: ','; CommSeparator: ';'; Terminator: CR),
     (Manufacturer: 'StanfordResearchSystems';   Model: 'DS335'; Commands: nil; ParSeparator: ','; CommSeparator: ';'; Terminator: CR),
     (Manufacturer: 'Stanford_Research_Systems'; Model: 'SR844'; Commands: nil; ParSeparator: ','; CommSeparator: ';'; Terminator: CR),
     (Manufacturer: 'Stanford_Research_Systems'; Model: 'SR830'; Commands: nil; ParSeparator: ','; CommSeparator: ';'; Terminator: CR)
     );
    { TODO 3 -cFeature : Custom device support }
  PointsInSR844Buffer = 16383;

  TestTimeOut = 2000;

  DefaultConfig = 'Default.cfg';
  DefaultParams = 'Last.prm';

  HT = #09;

var
  CurrFunction: FunctionType;
  AmplitudeUnit: tUnits;
  MainForm: TMainForm;
  ExperimentLog: TFileStream;
  PortList: string;
  PortCount: integer = 0;
  ReportNumber: integer = 0;
  ExperimentNumber: integer = 1;

  Config: RConfig;
  Params: RParams;

  ParamArr: array of longint;

implementation

uses
  Math, MemoF, StepF, OptionF, ReadingsF, AboutF, Variants, GenConst;

{$R *.lfm}

{ TMainForm }

function TMainForm.LoadParams(FileName: ansistring): word;
var
  f: file;
  i: longint;
  s: shortstring;
  LastPortExists: boolean = false;
begin
  system.assign(f, FileName);
  {$I-}
  reset(f, sizeof(RParams));
  blockread(f, Params, 1);
  LoadParams := IOResult;
  if LoadParams = 0 then
  begin
    with Params do
    begin
      cbPortSelect.Text:= GeneratorCP.Port;
      seRecvTimeout.Value:= GeneratorCP.Timeout;
      PresumedDevice:= GeneratorCP.Device;

      cbImpedance.ItemIndex:= Impedance;
      cbFuncSelect.ItemIndex:= CurrFunc;

      cbAmplUnit.ItemIndex:= AmpUnit;
      cbAmplUnitChange(Self);
      eAmplitude.Value:= Amplitude;
      eOffset.Value:= Offset;
      cbACEnable.Checked:= ACOn;

      eFrequency.Value:= Frequency;

      eSweepStartF.Value:= SweepStartF;
      eSweepStopF.Value:= SweepStopF;
      eSweepRate.Value:= SweepRate;
      cbSweepRate.Checked:= AutoSweep;
      cbSweepType.ItemIndex:= SweepType;
      cbSweepDirection.ItemIndex:= SweepDir;

      eFStep.Value:= StepF;
      eStepStartF.Value:= StepStartF;
      eStepStopF.Value:= StepStopF;
      eAStep.Value:= StepA;
      eStepStartA.Value:= StepStartA;
      eStepStopA.Value:= StepStopA;
      eTimeStep.Value:= TimeStep;
      eDelay.Value:= Delay;


      with ReadingsForm, DetectorCP do
      begin
        cbPortSelect.Text:= Port;
        seRecvTimeout.Value:= Timeout;
        PresumedDevice:= Device;

        cbReadingsMode.ItemIndex:= ReadingsMode;
        cbUseGenFreq.Checked:= GenFreq;

        cbSampleRate.ItemIndex:= SampleRate;
        cbTimeConstant.ItemIndex:= TimeConstant;
        cbSensitivity.ItemIndex:= Sensitivity;

        cbCh1.ItemIndex:= Display1;
        cbCh2.ItemIndex:= Display2;
        cbRatio1.ItemIndex:= Ratio1;
        cbRatio2.ItemIndex:= Ratio2;
        cbChart1Show.ItemIndex:= Show1;
        cbChart2Show.ItemIndex:= Show2;

        eUpdateInterval.Value:= UpdateInterval;
        eAxisLimit.Value:= AxisLimit;
        cbXAxis.ItemIndex:= XAxis;
      end;

      cbPointPerStep.Checked:= OnePoint;

      for i:= 0 to PortCount - 1 do
        if cbPortSelect.Items.Strings[i] = cbPortSelect.Text then LastPortExists:= true;
      if not LastPortExists then
      begin
        if cbPortSelect.Text <> '' then ShowMessage('Сохраненный порт недоступен');
        if PortList <> '' then cbPortSelect.Text:= cbPortSelect.Items.Strings[0]
        else cbPortSelect.Text:= 'Нет';
      end;
    end
  end
  else
  begin
    str(LoadParams, s);
    ShowMessage('Ошибка загрузки параметров. Код ошибки ' + s);
    with Params do
    begin
      GeneratorCP.Port    := cbPortSelect.Text;
      GeneratorCP.Timeout := seRecvTimeout.Value;
      GeneratorCP.Device  := PresumedDevice;
      Impedance           := cbImpedance.ItemIndex;
      CurrFunc            := cbFuncSelect.ItemIndex;
      Amplitude           := eAmplitude.Value;
      Offset              := eOffset.Value;
      ACOn                := cbACEnable.Checked;
      Frequency           := eFrequency.Value;
      SweepStartF         := eSweepStartF.Value;
      SweepStopF          := eSweepStopF.Value;
      SweepRate           := eSweepRate.Value;
      AutoSweep           := cbSweepRate.Checked;
      SweepType           := cbSweepType.ItemIndex;
      SweepDir            := cbSweepDirection.ItemIndex;
      StepF               := eFStep.Value;
      StepStartF          := eStepStartF.Value;
      StepStopF           := eStepStopF.Value;
      StepA               := eAStep.Value;
      StepStartA          := eStepStartA.Value;
      StepStopA           := eStepStopA.Value;
      TimeStep            := eTimeStep.Value;

      with ReadingsForm, DetectorCP do
      begin
        Port                := cbPortSelect.Text;
        Timeout             := seRecvTimeout.Value;
        Device              := PresumedDevice;
        ReadingsMode        := cbReadingsMode.ItemIndex;
        GenFreq             := cbUseGenFreq.Checked;
        SampleRate          := cbSampleRate.ItemIndex;
        TimeConstant        := cbTimeConstant.ItemIndex;
        Sensitivity         := cbSensitivity.ItemIndex;
        Display1            := cbCh1.ItemIndex;
        Display2            := cbCh2.ItemIndex;
        Ratio1              := cbRatio1.ItemIndex;
        Ratio2              := cbRatio2.ItemIndex;
        Show1               := cbChart1Show.ItemIndex;
        Show2               := cbChart2Show.ItemIndex;
        UpdateInterval      := eUpdateInterval.Value;
        AxisLimit           := eAxisLimit.Value;
        XAxis               := cbXAxis.ItemIndex;
      end;
    end;
  end;

  MainForm.ConnectParams:= Params.GeneratorCP;
  ReadingsForm.ConnectParams:= Params.DetectorCP;
  MainForm.PresumedDevice:= Params.GeneratorCP.Device;
  ReadingsForm.PresumedDevice:= Params.DetectorCP.Device;

  system.close(f);
  {$I+}
end;

function TMainForm.SaveParams(FileName: ansistring): word;
var
  f: file;
begin
  Params.GeneratorCP:= MainForm.ConnectParams;
  Params.DetectorCP:= ReadingsForm.ConnectParams;
  system.assign(f, FileName);
  {$I-}
  rewrite(f, sizeof(RParams));
  blockwrite(f, Params, 1);

  SaveParams:= IOResult;
  system.close(f);
  {$I+}
  begin
    if SaveParams <> 0 then ShowMessage('Ошибка сохранения параметров. Код ошибки ' + strf(SaveParams));
  end;
end;

function TMainForm.SaveConfig(FileName: ansistring): word;
var
  f: file;
  s: string;
begin
  system.assign(f, FileName);
  {$I-}
  rewrite(f, sizeof(RConfig));

  blockwrite(f, Config, 1);

  system.close(f);
  {$I+}
  SaveConfig:= IOResult;
  str(SaveConfig, s);
  if SaveConfig <> 0 then ShowMessage('Ошибка сохранения конфигурации. Код ошибки ' + s);
end;

function TMainForm.LoadConfig(FileName: ansistring): word;
var
  f: file;
  s: string;
begin
  if FileExists(FileName) then
  begin
    system.assign(f, FileName);
    {$I-}
    reset(f, sizeof(RConfig));

    blockread(f, Config, 1);

    system.close(f);
    {$I+}
    LoadConfig:= IOResult;
  end
  else LoadConfig:= 2;
  str(LoadConfig, s);
  if LoadConfig <> 0 then ShowMessage('Ошибка загрузки конфигурации. Код ошибки ' + s);
end;

function TMainForm.ExportParams(Manual: boolean; Header: boolean = false): word;
var
  i: integer;
  f: system.text;
  FileName, s: string;
begin
  DateTimeToString(FileName, 'yyyy_mm_dd', Now);

  if ReportNumber = 0 then
  begin
    inc(ReportNumber);
    str(ReportNumber, s);
    while FileExists(FileName + '_' + s + '.txt') {or
          FileExists(FileName + '_' + s + '_1' + '.log')} do        //to cfg?
    begin
      inc(ReportNumber);
      str(ReportNumber, s);
    end;
    FileName+= '_' + s + '.txt';

    Header:= true;
    {$I-}
    system.assign(f, FileName);
    rewrite(f);

    {$I+}
  end
  else
  begin
    str(ReportNumber, s);
    FileName+= '_' + s + '.txt';

    {$I-}
    system.assign(f, FileName);
    if FileExists(Filename) then append(f)
    else rewrite(f);
    {$I+}
  end;

  {$I-}
  if Header then
  begin
    writeln(f, 'МФТИ');
    writeln(f, 'Кафедра фотоники');
    writeln(f, '======================================================================');
  end;

  DateTimeToString(s, 'dd/mm/yyyy  hh:mm:ss', Now);
  writeln(f, s);
  str(ExperimentNumber, s);

  writeln(f, 'Эксперимент №' + s);
  writeln(f, '======================================================================');
  writeln(f, '               Параметры:');
  writeln(f, 'Сопротивление:        ' + cbImpedance.Text);
  writeln(f, 'Функция:              ' + cbFuncSelect.Text);
  writeln(f, 'Амплитуда:            ' + AmplitudeReading.Caption + ' В');
  writeln(f, 'Смещение:             ' + OffsetReading.Caption + ' В');
  if FrequencyTab.TabIndex = 0 then
  begin
    writeln(f);
    writeln(f, 'Постоянная частота');
    writeln(f, 'Значение:             ' + FrequencyReading.Caption + ' Гц');
  end
  else
  if FrequencyTab.TabIndex = 1 then
  begin
    writeln(f);
    writeln(f, 'Сканирование');
    writeln(f, 'Тип:                  ' + cbSweepType.Text + ', ' + lowercase(cbSweepDirection.Text));
    writeln(f, 'Начальная частота:    ' + SweepStartFReading.Caption + ' Гц');
    writeln(f, 'Конечная частота:     ' + SweepStopFReading.Caption + ' Гц');
    if cbSweepRate.Checked then
    begin
      writeln(f, 'Частота сканирования: ' + SweepRateReading.Caption + ' Гц');
    end
    else writeln(f, 'Сканирование по триггеру');
  end
  else
  begin
    writeln(f);
    writeln(f, 'Пошагово');
    str(eTimeStep.Value, s);
    writeln(f, 'Интервал:             ' + s + ' мс');
    str(eStepStartF.Value:10:6, s);
    writeln(f, 'Начальная частота:    ' + s + ' Гц');
    str(eStepStopF.Value:10:6, s);
    writeln(f, 'Конечная частота:     ' + s + ' Гц');
    str(eFStep.Value:10:6, s);
    writeln(f, 'Шаг:                  ' + s + ' Гц');
    writeln(f);
    str(eStepStartA.Value:0:2, s);
    writeln(f, 'Начальная амплитуда:  ' + s + ' В');
    str(eStepStopA.Value:0:2, s);
    writeln(f, 'Конечная амплитуда:   ' + s + ' В');
    str(eAStep.Value:0:2, s);
    writeln(f, 'Шаг:                  ' + s + ' В');
    writeln(f, 'Данные в файле: ', ReadingsForm.CurrLogFileName);
  end;
  writeln(f, '======================================================================');
  writeln(f, '               Комментарий:');

  if Config.AutoComment or Manual then
  MemoForm.ShowModal;
  for i:= 0 to MemoForm.mComment.Lines.Count - 1 do
    writeln(f, MemoForm.mComment.Lines[i]);
  MemoForm.mComment.Lines.Clear;

  writeln(f, '======================================================================');
  writeln(f, '======================================================================');
  system.close(f);
  {$I+}

  ExportParams:= IOResult;
  if ExportParams <> 0 then ShowMessage('Ошибка сохранения отчета');
end;

{function TMainForm.SaveParams: word;
begin

end;}

procedure TMainForm.FormCreate(Sender: TObject);
var
  p: integer;
begin
  Top:= Screen.Height div 2 - Height div 2;
  Left:= Screen.Width div 2 - Width - 8;
  btQuery.Caption:= 'Запрос' + LineEnding + 'текущих' + LineEnding + 'значений';
  btProgram.Caption:= 'Включить' + LineEnding + 'управление';
  btReset.Caption:= 'Сбросить' + LineEnding + '‌настройки'+ LineEnding + 'прибора';
  btCustomCommand.Caption:= 'Польз.' + LineEnding + 'команда';

  SupportedDevices[iDefaultDevice].Commands:= CommonCommand;
  SupportedDevices[iDS335].Commands:= DS335Command;
  SupportedDevices[iDS345].Commands:= DS345Command;
  config.defaultparams:= defaultparams;

  //FrequencyTab.Color:= ;
  //:= TFileStream. Create;
  //ProgramLog:= TFileStream.Create('log.txt', fmCreate);
  if FileExists('ProgramLog.txt') then DeleteFile('ProgramLog.txt');
  ProgramLog:= TFileStream.Create('ProgramLog.txt', fmCreate);
  InitCriticalSection(LogCS);
  WriteProgramLog('Создание главного окна');

  SerPort:= TBlockSerial.Create;
  SerPort.RaiseExcept:= true;
  SerPort.ConvertLineEnd:= true;
  SerPort.DeadLockTimeOut:= 10000;
  DeviceIndex:= iDefaultDevice;
  PortList:= //'COM1,COM2,COM3,COM4';
             GetSerialPortNames;
  PortCount:= 0;

    if Portlist = '' then StatusBar1.Caption:= 'Нет доступных портов'
    else
      begin
        p:= pos(',', PortList);
        if p = 0 then cbPortSelect.AddItem(PortList, nil)
        else
          begin
            while p<>0 do
            begin
              cbPortSelect.AddItem(copy(PortList, 1, p-1), nil);
              delete(PortList, 1, p);
              inc(PortCount);
              p:= pos(',', PortList);
            end;
            cbPortSelect.AddItem(PortList, nil);
            inc(PortCount);
          end;
      end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Config.SaveParamsOnExit then
    SaveParams(Config.DefaultParams);
  SaveConfig(DefaultConfig);
  SerPort.CloseSocket;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SerPort.Destroy;
  DoneCriticalSection(LogCS);
  //ExperimentLog.Destroy;
  ProgramLog.Free;
end;

procedure TMainForm.NewReportClick(Sender: TObject);
begin
  ReportNumber:= 0;
  ExperimentNumber:= 1;
  ExportParams(true);
end;

procedure TMainForm.miExportParamsClick(Sender: TObject);
begin
  ExportParams(true);
end;

procedure TMainForm.eAmplitudeChange(Sender: TObject);
begin
  if eAmplitude.Value > 0 then
  begin
    eAmplitude.MinValue:= 0.05 + cbImpedance.ItemIndex * 0.05;
    cbACEnable.Checked:= true;
  end;
  if cbACEnable.Checked and (eAmplitude.Value / 2 + abs(eOffset.Value) > eOffset.MaxValue) then eOffset.Value:= (eOffset.MaxValue - eAmplitude.Value/2) * sign(eOffset.Value);
end;

procedure TMainForm.eStepChange(Sender: TObject);
begin
  if ((eStepStartF.Value <> eStepStopF.Value) and (eFStep.Value <> 0)) or
    ((eStepStartA.Value <> eStepStopA.Value) and (eAStep.Value <> 0)) then
    StepForm.CalcSteps
    else TotalTime.Caption:= '0 ч 0 м 0 с, 0 шагов';
  if eFStep.Value = 0 then eStepStopF.Value:= eStepStartF.Value;
  if eAStep.Value = 0 then eStepStopA.Value:= eStepStartA.Value;
  eDelay.MinValue:= eTimeStep.Value;
end;

procedure TMainForm.eSweepStartFChange(Sender: TObject);
begin
  if cbSweepType.ItemIndex = 1 then
  begin
      if (eSweepStartF.Value < eSweepStopF.Value) and (eSweepStartF.Value * 1000000 < eSweepStopF.Value) then
      eSweepStartF.Value:= eSweepStopF.Value / 1000000;
      if (eSweepStartF.Value > eSweepStopF.Value) and (eSweepStartF.Value  > eSweepStopF.Value * 1000000) then
      eSweepStartF.Value:= eSweepStopF.Value * 1000000;
  end
end;

procedure TMainForm.eSweepStopFChange(Sender: TObject);
begin
  if cbSweepType.ItemIndex = 1 then
  begin
      if (eSweepStartF.Value < eSweepStopF.Value) and (eSweepStartF.Value * 1000000 < eSweepStopF.Value) then
      eSweepStopF.Value:= eSweepStartF.Value * 1000000;
      if (eSweepStartF.Value > eSweepStopF.Value) and (eSweepStartF.Value  > eSweepStopF.Value * 1000000) then
      eSweepStopF.Value:= eSweepStartF.Value / 1000000;
  end
end;

procedure TMainForm.DebugBoxClick(Sender: TObject);
begin
  DebugBox.Lines[0]:= SerPort.LastErrorDesc;
  DebugBox.Lines[1]:= ReadingsForm.SerPort.LastErrorDesc;
end;

procedure TMainForm.cbPointPerStepChange(Sender: TObject);
begin
  if cbPointPerStep.Checked and (ReadingsForm.cbReadingsMode.ItemIndex = 0) then
  begin
    ReadingsForm.cbReadingsMode.ItemIndex:= 1;
    ReadingsForm.cbReadingsModeChange(Self);
    ShowMessage('Доступно только в режиме "Одновременный запрос".' + LineEnding +
    'Режим снятия переключен');
  end;
end;

procedure TMainForm.AboutClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.cbAmplUnitChange(Sender: TObject);
begin
  if cbAmplUnit.ItemIndex = 0 then AmplitudeUnit:= VP
  else AmplitudeUnit:= VR;
end;

procedure TMainForm.eOffsetChange(Sender: TObject);
begin
  if cbACEnable.Checked and (abs(eOffset.Value) > 2* eAmplitude.Value) then eOffset.Value:= 2* eAmplitude.Value * sign(eOffset.Value);
  if cbACEnable.Checked and (eAmplitude.Value/2 + abs(eOffset.Value) > eOffset.MaxValue) then eAmplitude.Value:=2* (eOffset.MaxValue - abs(eOffset.Value));
end;

procedure TMainForm.miOptionsClick(Sender: TObject);
begin
  OptionForm.ShowModal;
end;

procedure TMainForm.miLoadCfgClick(Sender: TObject);
var
  s: ansistring;
begin
  OpenDialog.FileName:= '';
  OpenDialog.Title:= 'Загрузить файл конфигурации';
  OpenDialog.Filter:= 'Файлы конфигурации|*.cfg|Все файлы|*.*';
  if OpenDialog.Execute then
  begin
    s:= UTF8toANSI(OpenDialog.FileName); //  проверить на доп символы
    if s <> '' then LoadConfig(s);
  end;
end;

procedure TMainForm.miSaveCfgClick(Sender: TObject);
var
  s: ansistring;
begin
  SaveDialog.FileName:= '';
  SaveDialog.Title:= 'Сохранить файл конфигурации как';
  SaveDialog.Filter:= 'Файлы конфигурации|*.cfg|Все файлы|*.*';
  if SaveDialog.Execute then
  begin
    s:= UTF8toANSI(SaveDialog.FileName);
    if s <> '' then SaveConfig(s);
  end;
end;

procedure TMainForm.ReadingTimerStartTimer(Sender: TObject);
begin
  ReadingTimer.Interval:= Params.ReadingTime * 1000;
end;

procedure TMainForm.ReadingTimerTimer(Sender: TObject);
begin
  ReadingsForm.StopLog;
  ReadingTimer.Enabled:= false;
end;

procedure TMainForm.miLoadParClick(Sender: TObject);
var
  s: ansistring;
begin
  OpenDialog.FileName:= '';
  OpenDialog.Title:= 'Загрузить файл параметров';
  OpenDialog.Filter:= 'Файлы параметров|*.prm|Все файлы|*.*';
  if OpenDialog.Execute then
  begin
    s:= UTF8toANSI(OpenDialog.FileName);
    if s <> '' then LoadParams(s);
  end;
end;

procedure TMainForm.miSaveParClick(Sender: TObject);
var
  s: ansistring;
begin
  SaveDialog.FileName:= '';
  SaveDialog.Title:= 'Сохранить файл параметров как';
  SaveDialog.Filter:= 'Файлы параметров|*.prm|Все файлы|*.*';
  if SaveDialog.Execute then
  begin
    s:= UTF8toANSI(SaveDialog.FileName);
    if s <> '' then SaveParams(s);
  end;
end;

procedure TMainForm.miShowReadingsFClick(Sender: TObject);
begin
  if not miShowReadingsF.Checked then
  begin
    ReadingsForm.Show;
    miShowReadingsF.Checked:= true;
  end
  else
  begin
    ReadingsForm.Hide;
    miShowReadingsF.Checked:= false;
  end;
end;

procedure TMainForm.cbFuncSelectChange(Sender: TObject);
var
  max: double;
begin
  CurrFunction:= FunctionType(cbFuncSelect.ItemIndex);
  case CurrFunction of
    Sine, Square: max:=  3100000;                                       // переделать в мас конст
    Triangle, Ramp: max:= 10000;
    Noise: max:= 3500000;
  end;
  eFrequency.MaxValue:= max;
  eSweepStartF.MaxValue:= max;
  eSweepStopF.MaxValue:= max;
  eStepStartF.MaxValue:= max;
  eStepStopF.MaxValue:= max;
end;

procedure TMainForm.cbImpedanceChange(Sender: TObject);
begin
  if cbImpedance.ItemIndex = 1 then
  begin
    eAmplitude.MaxValue:= 20;
    eStepStartA.MaxValue:= 20;
    eStepStopA.MaxValue:= 20;
    eAmplitude.MinValue:= 0.1;
    eStepStartA.MinValue:= 0.1;
    eStepStopA.MinValue:= 0.1;
    eOffset.MaxValue:= 10;
    eOffset.MinValue:= -10;
  end
  else
  begin
    eAmplitude.MaxValue:= 10;
    eStepStartA.MaxValue:= 10;
    eStepStopA.MaxValue:= 10;
    eAmplitude.MinValue:= 0.05;
    eStepStartA.MinValue:= 0.05;
    eStepStopA.MinValue:= 0.05;
    eOffset.MaxValue:= 5;
    eOffset.MinValue:= -5;
  end;
end;

procedure TMainForm.cbSweepTypeChange(Sender: TObject);
begin
  if cbSweepType.ItemIndex = 1 then
  begin
    eSweepStartF.MinValue:= 0.000001;
    eSweepStopF.MinValue:= 0.000001;
    if eSweepStartF.Value < eSweepStopF.Value then
    begin
      if eSweepStartF.Value * 1000000 < eSweepStopF.Value then eSweepStopF.Value:= eSweepStartF.Value * 1000000;
    end
    else
    if eSweepStopF.Value * 1000000 < eSweepStartF.Value then eSweepStartF.Value:= eSweepStopF.Value * 1000000;
  end
  else
  begin
    eSweepStartF.MinValue:= 0;
    eSweepStopF.MinValue:= 0;
  end;
end;

procedure TMainForm.btApplyClick(Sender: TObject);
var
  s: string;
begin
  with Params do
  begin
    Impedance:= cbImpedance.ItemIndex;
    CurrFunc:= longint(CurrFunction);
    Offset:= eOffset.Value;
    ACOn:= cbACEnable.Checked;
    AmpUnit:= cbAmplUnit.ItemIndex;

    if DeviceIndex <> iDS345 then AddCommand(TERM, false, Impedance);
    AddCommand(FUNC, false, CurrFunc);
    AddCommand(OFFS, false, Offset, NOUNIT);

    if FrequencyTab.TabIndex = 2 then
    begin
      if ((eStepStartF.Value <> eStepStopF.Value) and (eFStep.Value <> 0)) or
      ((eStepStartA.Value <> eStepStopA.Value) and (eAStep.Value <> 0)) then
      begin
        EnableControls(false);

        AddCommand(SWEN, false, 0);

        AutoSweep:= false;
        StepStartF:= eStepStartF.Value;
        StepStopF:=  eStepStopF.Value;
        StepF:=      eFStep.Value;
        StepStartA:= eStepStartA.Value;
        StepStopA:=  eStepStopA.Value;
        StepA:=      eAStep.Value;
        TimeStep:=   eTimeStep.Value;
        Delay:=      eDelay.Value;

        StepForm.Show;
        if Config.AutoExportParams and not Config.AutoReadingStep then
        begin
          ExportParams(false);
          inc(ExperimentNumber);
        end;
      end;
    end
    else
    begin
      AddCommand(AMPL, false, eAmplitude.Value, AmplitudeUnit);
      if FrequencyTab.TabIndex = 1 then
      begin
        ReadingsForm.LogFreq:= true;
        ReadingsForm.cbUseGenFreq.Checked:= false;
        ReadingsForm.LogAmpl:= false;
        if Config.AutoReadingSweep then
        begin
          ReadingsForm.BeginLog;
          if Params.ReadingTime > 0 then
            ReadingTimer.Enabled:= true;
        end;

        AddCommand(TSRC, false, integer(cbSweepRate.Checked));
        AddCommand(MTYP, false, cbSweepType.ItemIndex);
        AddCommand(SDIR, false, cbSweepDirection.ItemIndex);
        AddCommand(TRAT, false, eSweepRate.Value, NOUNIT);
        AddCommand(STFR, false, eSweepStartF.Value, NOUNIT);
        AddCommand(SPFR, false, eSweepStopF.Value, NOUNIT);
        AddCommand(SWEN, false, 1);

        AutoSweep:= cbSweepRate.Checked;
        SweepStartF:= eSweepStartF.Value;
        SweepStopF:= eSweepStopF.Value;
        SweepRate:= eSweepRate.Value;
        SweepType:= cbSweepType.ItemIndex;
        SweepDir:= cbSweepDirection.ItemIndex;
        if Config.AutoExportParams and not Config.AutoReadingSweep then
        begin
          ExportParams(false);
          inc(ExperimentNumber);
        end;
      end
      else
      begin
        ReadingsForm.LogFreq:= false;
        ReadingsForm.LogAmpl:= false;
        if Config.AutoReadingConst then
        begin
          ReadingsForm.BeginLog;
          if Params.ReadingTime > 0 then
            ReadingTimer.Enabled:= true;
        end;
        Frequency:= eFrequency.Value;

        AddCommand(FREQ, false, Frequency, NOUNIT);
        AddCommand(SWEN, false, 0);
        if Config.AutoExportParams and not Config.AutoReadingConst then
        begin
          ExportParams(false);
          inc(ExperimentNumber);
        end;
      end;
    end;
    PassCommands;
  end;

  str(eAmplitude.Value:0:2, s);
  AmplitudeReading.Caption:= s;
  str(eOffset.Value:0:2, s);
  OffsetReading.Caption:= s;
  str(eFrequency.Value:0:2, s);
  FrequencyReading.Caption:= s;
  str(eSweepStartF.Value:0:2, s);
  SweepStartFReading.Caption:= s;
  str(eSweepStopF.Value:0:2, s);
  SweepStopFReading.Caption:= s;
  str(eSweepRate.Value:0:2, s);
  SweepRateReading.Caption:= s;
end;

procedure TMainForm.btProgramClick(Sender: TObject);
begin
  enablecontrols(true);
  readingsform.enablecontrols(true);
  DeviceIndex:= iDS345;
  readingsform.DeviceIndex:= iSR830;             //--------------
  readingsform.btnConnectClick(self);
end;

procedure TMainForm.btQueryClick(Sender: TObject);
var
  s, cs: string;
  i, e: integer;
begin
  EnterCriticalSection(CommCS);
    AddCommand(TERM, true);
    AddCommand(FUNC, true);
    AddCommand(AMPL, true);
    AddCommand(OFFS, true);
    AddCommand(FREQ, true);

    AddCommand(TSRC, true);
    AddCommand(MTYP, true);
    AddCommand(SDIR, true);
    AddCommand(TRAT, true);
    AddCommand(STFR, true);
    AddCommand(SPFR, true);
    //AddCommand(SWEN, true);         ?индикатор

    PassCommands;

    s:= RecvString;
  LeaveCriticalSection(CommCS);
  statusbar1.SimpleText:= s;

  cs:= SupportedDevices[DeviceIndex].CommSeparator;

  val(copy(s, 1, pos(cs, s) - 1), i, e);
  if e <> 0 then exit;
  cbImpedance.ItemIndex:= i;
  delete(s, 1, pos(cs, s));

  val(copy(s, 1, pos(cs, s) - 1), i);
  cbFuncSelect.ItemIndex:= i;
  delete(s, 1, pos(cs, s));

  AmplitudeReading.Caption:= copy(s, 1, pos(cs, s) - 1);   //VP!
  delete(s, 1, pos(cs, s));

  OffsetReading.Caption:= copy(s, 1, pos(cs, s) - 1);
  delete(s, 1, pos(cs, s));

  FrequencyReading.Caption:= copy(s, 1, pos(cs, s) - 1);
  delete(s, 1, pos(cs, s));

  val(copy(s, 1, pos(cs, s) - 1), i);
  cbSweepRate.Checked:= boolean(i);
  delete(s, 1, pos(cs, s));

  val(copy(s, 1, pos(cs, s) - 1), i);
  cbSweepType.ItemIndex:= i;
  delete(s, 1, pos(cs, s));

  val(copy(s, 1, pos(cs, s) - 1), i);
  cbSweepDirection.ItemIndex:= i;
  delete(s, 1, pos(cs, s));

  SweepRateReading.Caption:= copy(s, 1, pos(cs, s) - 1);
  delete(s, 1, pos(cs, s));

  SweepStartFReading.Caption:= copy(s, 1, pos(cs, s) - 1);
  delete(s, 1, pos(cs, s));

  SweepStopFReading.Caption:= s; {copy(s, 1, pos(cs, s) - 1);
  delete(s, 1, pos(cs, s));}
end;

procedure TMainForm.btStopClick(Sender: TObject);
begin
  AddCommand(SWEN, false, 0);
  AddCommand(FREQ, false, 0, NOUNIT);
  AddCommand(AMPL, false, 0, VP);
  PassCommands;
end;

procedure TMainForm.btTriggerClick(Sender: TObject);
begin
  AddCommand(TRG);
  PassCommands;
end;

procedure TMainForm.cbACEnableChange(Sender: TObject);
begin
  if not cbACEnable.Checked then
  begin
    eAmplitude.MinValue:= 0;
    eAmplitude.Value:= 0;
  end
  else
  eAmplitude.MinValue:= 0.05 + cbImpedance.ItemIndex * 0.05;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  LoadConfig(DefaultConfig);

  if not FileExists(Config.DefaultParams) then Config.DefaultParams:= DefaultParams; //strange filenames saved in config prolly on crash but maybe on recompile???
  if Config.LoadParamsOnStart then LoadParams(Config.DefaultParams);

  cbImpedanceChange(Self);
  cbFuncSelectChange(Self);
  eStepChange(Self);
  ReadingsForm.cbReadingsModeChange(ReadingsForm);

end;

procedure TMainForm.btnConnectClick(Sender: TObject);
begin
  if (ReadingsForm.Serport.InstanceActive) and
    (ReadingsForm.cbPortSelect.ItemIndex = cbPortSelect.ItemIndex) then
    begin
      showmessage('К данному порту уже осуществляется подключение');
      exit
    end;
  OptionForm.TabControl.TabIndex:= 0;
  inherited btnConnectClick(Sender);

  if DeviceIndex = iDefaultDevice then Exit;
  cbFuncSelect.Items.Clear;
  case DeviceIndex of
    iDS345:
    begin
      Label9.Hide;
      cbImpedance.Hide;
      cbFuncSelect.Items.AddStrings(DS345WaveForm);
    end;
    iDS335:
    begin
      Label9.Show;
      cbImpedance.Show;
      cbFuncSelect.Items.AddStrings(DS335WaveForm);
    end;
  end;
end;

procedure TMainForm.EnableControls(Enable: boolean);
begin
  cbImpedance.Enabled:=      Enable;
  cbFuncSelect.Enabled:=     Enable;
  cbAmplUnit.Enabled:=       Enable;
  eAmplitude.Enabled:=       Enable;
  eOffset.Enabled:=          Enable;
  eFrequency.Enabled:=       Enable;
  cbSweepRate.Enabled:=      Enable;
  eSweepRate.Enabled:=       Enable;
  SweepRateReading.Enabled:= Enable;
  cbSweepType.Enabled:=      Enable;
  cbSweepDirection.Enabled:= Enable;
  eSweepStartF.Enabled:=     Enable;
  eSweepStopF.Enabled:=      Enable;
  btApply.Enabled:=          Enable;
  btTrigger.Enabled:=        Enable;
  btQuery.Enabled:=          Enable;
  btCustomCommand.Enabled:=  Enable;
  btStop.Enabled:=           Enable;
  btReset.Enabled:=          Enable;
  btStatus.Enabled:=         Enable;
  cbACEnable.Enabled:=       Enable;
  eStepStartF.Enabled:=      Enable;
  eStepStopF.Enabled:=       Enable;
  eFStep.Enabled:=           Enable;
  eStepStartA.Enabled:=      Enable;
  eStepStopA.Enabled:=       Enable;
  eAStep.Enabled:=           Enable;
  eTimeStep.Enabled:=        Enable;
  eDelay.Enabled:=           Enable;
  cbPointPerStep.Enabled:=   Enable;
  NewReport.Enabled:=        Enable;
  miExportParams.Enabled:=   Enable;
end;

end.
