unit MainF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, IDEWindowIntf, StrUtils, Forms,
  Controls, Graphics, Dialogs, Menus, StdCtrls, ComCtrls, DbCtrls, Spin,
  ExtCtrls, Buttons, ActnList, Synaser, SerConF, DeviceF;

type
  { TMainForm }

  tMainForm = class(TSerConnectForm)
    btProgram: TSpeedButton;
    btAutoConnect: TSpeedButton;

    btTrigger: TButton;
    cbFuncSelect: TComboBox;
    cbImpedance: TComboBox;
    cbACEnable: TCheckBox;
    cbPointPerStepTemp: TCheckBox;

    cbSweepRate: TCheckBox;
    cbSweepType: TComboBox;
    cbSweepDirection: TComboBox;
    cbPointPerStepDet: TCheckBox;
    cbAmplUnit: TComboBox;
    cbModulation: TComboBox;
    DividerBevel1: TDividerBevel;
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
    Label1: TLabel;
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
    Label27: TLabel;
    Label28: TLabel;

    About: TMenuItem;
    Label29: TLabel;
    miShowTempControlF: TMenuItem;
    pnConnection: TPanel;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;

    miShowReadingsF: TMenuItem;

    miSaveProfile: TMenuItem;
    miLoadProfile: TMenuItem;
    miOptions: TMenuItem;
    OpenDialog: TOpenDialog;
    pnBaseParams: TPanel;
    SaveDialog: TSaveDialog;
    ReadingTimer: TTimer;

    TotalTime: TLabel;
    miNewReport: TMenuItem;
    miExportParams: TMenuItem;
    SweepStartFReading: TLabel;
    SweepStopFReading: TLabel;
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

    MainMenu: TMainMenu;
    miFile: TMenuItem;
    miAbout: TMenuItem;
    miView: TMenuItem;
    SweepRateReading: TLabel;
    tsConstantFrequency: TTabSheet;
    tsSweep: TTabSheet;
    tsStep: TTabSheet;
    procedure AboutClick(Sender: TObject);
    procedure btAutoConnectClick(Sender: TObject);
    procedure cbPointPerStepDetChange(Sender: TObject);
    procedure cbAmplUnitChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);

    procedure btnConnectClick(Sender: TObject); override;
    procedure btApplyClick(Sender: TObject);
    procedure btQueryClick(Sender: TObject); override;
    procedure btProgramClick(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure btTriggerClick(Sender: TObject);
    procedure cbFuncSelectChange(Sender: TObject);
    procedure cbImpedanceChange(Sender: TObject);
    procedure cbSweepTypeChange(Sender: TObject);
    procedure eOffsetChange(Sender: TObject);
    procedure eAmplitudeChange(Sender: TObject);
    procedure eStepChange(Sender: TObject);
    procedure eSweepStartFChange(Sender: TObject);
    procedure eSweepStopFChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FrequencyTabChange(Sender: TObject);
    procedure miLoadProfileClick(Sender: TObject);
    procedure miOptionsClick(Sender: TObject);
    procedure miSaveProfileClick(Sender: TObject);
    procedure miShowReadingsFClick(Sender: TObject);
    procedure miNewReportClick(Sender: TObject);
    procedure miExportParamsClick(Sender: TObject);
    procedure miLoadCfgClick(Sender: TObject);
    procedure miSaveCfgClick(Sender: TObject);
    procedure miShowTempControlFClick(Sender: TObject);
    procedure ReadingTimerStartTimer(Sender: TObject);
    procedure ReadingTimerTimer(Sender: TObject);
    procedure StatusBarHint(Sender: TObject);
  private
    { private declarations }
    iIm, iFu, iST, iSD, iMF: tStringArray;
    FrequencyLimits, MinAmplitudeLimits, MaxAmplitudeLimits: array of double;
  public
    { public declarations }
    FileResult: integer;
    ReportFolder: string;

    function FullCfgDir: string; inline;
    function SaveProfile(FileName: ansistring): integer;
    function LoadProfile(FileName: ansistring): integer;
    function SaveConfig(FileName: ansistring): integer;
    function LoadConfig(FileName: ansistring): integer;

    function SaveReport(Manual: boolean; Header: boolean = false): integer;

    procedure EnableControls(Enable: boolean); override;
    procedure GetDeviceParams; override;
  end;


const

  iDefaultDevice = 0;

  ConstFTab = 0;
  SweepTab = 1;
  StepTab = 2;

  TestTimeOut = 2000;
  DefaultCfgFolder = 'Config';
  DefaultConfig = 'Default.cfg';
  DefaultParams = 'Last.prm';

  HT = #09;

var
  AmplitudeUnit: eUnits;
  MainForm: TMainForm;
  PortList: string;
  PortCount: integer = 0;
  ReportNumber: integer = 0;
  ExperimentNumber: integer;
  Debug: boolean;

  Config: RConfig;
  Params: RParams;

implementation

uses
  Math, Variants, MemoF, StepF, OptionF, DetControlF, TempControlF, LogModule, OffsetF, AboutF;

{$R *.lfm}

{ TMainForm }

function tMainForm.LoadProfile(FileName: ansistring): integer;
var
  FileStream: TFileStream;
  StringStream: TStringStream;
  s: string;
  p: integer;
begin
  if FileExists(FileName) then
  begin
    try
      FileStream:= TFilestream.Create(FileName, fmOpenRead);
      Result:= RestoreState(FileStream);
      Result+= DetControlForm.RestoreState(FileStream);
      Result+= TempControlForm.RestoreState(FileStream);
      StringStream:= tStringStream.Create('');

      StringStream.CopyFrom(FileStream, 0);    //0 = whole

      p:= pos('Internal', StringStream.DataString);
      s:= CopyFromTo(StringStream.DataString, p, '(', ')');

      p:= pos('DetLogDir', s);
      with DetControlForm do
        if p = 0 then
          DataFolder:= DefaultLogFolder
        else
          DataFolder:= CopyFromTo(s, p, '=', LineEnding);

      p:= pos('TempLogDir', s);
      with TempControlForm do
        if p = 0 then
          DataFolder:= DefaultLogFolder
        else
          DataFolder:= CopyFromTo(s, p, '=', LineEnding);

      StringStream.Destroy;
      FileStream.Destroy;
      if Result < 0 then s:= 'неверный параметр ' + strf(Result);
    except
      on e:Exception do
      begin
        Result:= 1;
        s:= e.Message;
      end;
    end;
  end
  else
  begin
    Result:= 2;
    s:= 'не найден файл ' + FileName;
  end;

  if Result <> 0 then
    ShowMessage('Ошибка загрузки параметров: ' + s);

  if cbPortSelect.ItemIndex < 0 then
  begin
    ShowMessage('Сохраненный порт недоступен');
    cbPortSelect.ItemIndex:= 0;
  end;
end;

function tMainForm.SaveProfile(FileName: ansistring): integer;
var
  FileStream: TFileStream;
  s: string;
begin
  Result:= 0;
  try
    FileStream:= TFilestream.Create(FileName, fmCreate);
  except
    on e: Exception do
    begin
      Result:= 1;
      s:= e.Message;
    end;
  end;
  SaveState(FileStream);
  DetControlForm.SaveState(FileStream);
  TempControlForm.SaveState(FileStream);
  s:= 'Internal' + '(' + LineEnding;
  FileStream.Write(s[1], length(s));

  s:= 'DetLogDir' + '=' + DetControlForm.DataFolder + LineEnding;
  FileStream.Write(s[1], length(s));

  s:= 'TempLogDir' + '=' + TempControlForm.DataFolder + LineEnding;
  FileStream.Write(s[1], length(s));

  s:= ')' + LineEnding;
  FileStream.Write(s[1], length(s));
  FileStream.Free;

  if Result <> 0 then
      ShowMessage('Ошибка сохранения параметров: ' + s);
end;

function tMainForm.SaveConfig(FileName: ansistring): integer;
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
  Result:= IOResult;

  str(Result, s);
  if Result <> 0 then
    ShowMessage('Ошибка сохранения конфигурации. Код ошибки ' + s);
end;

function tMainForm.LoadConfig(FileName: ansistring): integer;
var
  f: file;
  s: string;
  cfg: rConfig;
begin
  if FileExists(FileName) then
  begin
    system.assign(f, FileName);
    {$I-}
    reset(f, sizeof(RConfig));

    blockread(f, cfg, 1);

    system.close(f);
    {$I+}
    Result:= IOResult;
  end
  else
    Result:= 2;

  str(Result, s);
  if Result = 0 then
    Config:= cfg
  else
    ShowMessage('Ошибка загрузки конфигурации. Код ошибки ' + s);
end;

function tMainForm.SaveReport(Manual: boolean; Header: boolean): integer;
var
  i: integer;
  f: system.text;
  FileName, s: string;
begin
  DateTimeToString(FileName, 'yyyy_mm_dd', Now);
if ReportFolder <> '' then
  begin
    if CreateDir(ReportFolder) then
      FileName:= ReportFolder + '\' + FileName
    else
      WriteProgramLog('Error creating folder ' + ReportFolder);
  end;

  if ReportNumber = 0 then
  begin
    inc(ReportNumber);
    str(ReportNumber, s);
    while FileExists(FileName + '_' + s + '.txt') do        //to cfg?
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
    if FileExists(Filename) then
      append(f)
    else
      rewrite(f);
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

  if MainForm.ConnectionKind <> cNone then
  begin
    writeln(f);
    writeln(CurrentDevice^.Manufacturer + ' ' + CurrentDevice^.Model);
    writeln(f);
    if cbImpedance.Visible then
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
      writeln(f, 'Тип сканирования:     ' + cbSweepType.Caption);
      writeln(f, 'Направление:          ' + cbSweepDirection.Caption);
      if cbModulation.Visible then
        writeln(f, 'Модуляция:            ' + cbModulation.Caption);
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
      writeln(f, 'Данные в файле: ', DetControlForm.Log.FileName);  { TODO : test }
    end;
  end;

  with DetControlForm do
  if ConnectionKind <> cNone then
  begin
    writeln(f);
    writeln(CurrentDevice^.Manufacturer + ' ' + CurrentDevice^.Model);
    writeln(f);
    writeln(f, Label6.Caption, ' ', cbTimeConstant.Text);
    writeln(f, Label7.Caption, ' ', cbSensitivity.Text);
    if cbReserve1.Visible then
      writeln(f, Label16.Caption, ' ', cbReserve1.Text);
    if cbReserve2.Visible then
      writeln(f, Label17.Caption, ' ', cbReserve2.Text);
    if cbInputRange.Visible then
      writeln(f, Label18.Caption, ' ', cbInputRange.Text);
    if cbRatio1.Visible then
      writeln(f, Label14.Caption, ' ', cbRatio1.Text);
    if cbRatio2.Visible then
      writeln(f, Label15.Caption, ' ', cbRatio2.Text);
    if OffsetTracked then
    with OffsetForm do
    begin              { TODO 3 -cImprovement : total query???? }
      writeln(f, 'Смещения:');
      for i:= 0 to cbParams.Items.Count - 1 do
        writeln(f, cbParams.Items[i],' Смещение: ', Offsets[i],' Множитель: ', Expands[i]);
    end;
    writeln(f);
  end;

  with TempControlForm do
  if ConnectionKind <> cNone then
  begin
    writeln(f);
    writeln(CurrentDevice^.Manufacturer + ' ' + CurrentDevice^.Model);
    writeln(f);
    writeln(f, Label3.Caption, ' ', cbSamplerate.Text);
    writeln(f);
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

  Result:= IOResult;
  if Result <> 0 then
    ShowMessage('Ошибка сохранения отчета')
  else
    StatusBar.Panels[spStatus].Text:= 'Cохранено в ' + FileName;
end;

procedure tMainForm.FormCreate(Sender: TObject);
var
  p: integer;
begin
  WriteProgramLog('Создание главного окна');
  {$IFOPT D+}
  btProgram.Visible:= true;
  {$ENDIF}

  Top:= Screen.Height div 2 - Height div 2;
  Left:= 8;
  btAutoConnect.Caption:= 'Автоматич.' + LineEnding + 'подключение';
  btQuery.Caption:= 'Запрос' + LineEnding + 'текущих' + LineEnding + 'значений';
  btProgram.Caption:= 'Включить' + LineEnding + 'управление';
  btReset.Caption:= 'Сбросить' + LineEnding + '‌настройки'+ LineEnding + 'прибора';
  btCustomCommand.Caption:= 'Польз.' + LineEnding + 'команда';
  btStatus.Caption:= 'Состояние' + LineEnding + ' прибора';

  if FileExists('ProgramLog.txt') then DeleteFile('ProgramLog.txt');
  ProgramLog:= TFileStream.Create('ProgramLog.txt', fmCreate);
  InitCriticalSection(LogCS);
  InitCriticalSection(CommCS);

  DisplayMessages:= true;
  ParamsApplied:= false;
  DeviceKind:= dGenerator;
  DeviceIndex:= iDefaultDevice;
  ConnectionKind:= cNone;
  PortList:= //'COM1,COM2,COM3,COM4';
             GetSerialPortNames;
  PortCount:= 0;

  if IsEmptyStr(PortList, [' ']) then
    StatusBar.Panels[spStatus].Text:= 'Нет доступных COM-портов'
  else
    begin
      p:= pos(',', PortList);
      if p = 0 then cbPortSelect.AddItem(PortList, nil)
      else
        begin
          while p <> 0 do
          begin
            cbPortSelect.AddItem(copy(PortList, 1, p - 1), nil);
            delete(PortList, 1, p);
            inc(PortCount);
            p:= pos(',', PortList);
          end;
          cbPortSelect.AddItem(PortList, nil);
          inc(PortCount);
        end;
    end;
  cbPortSelect.AddItem('Ethernet', nil);
  inc(PortCount);

  if cbPortSelect.ItemIndex < 0 then
    cbPortSelect.ItemIndex:= 0;

  Config.CfgFolder:= DefaultCfgFolder; //init config
  Config.ParamFile:= DefaultParams;
  Config.WorkConfig:= DefaultConfig;

  if DirectoryExists(FullCfgDir) then
  begin
    FileResult:= LoadConfig(FullCfgDir + DefaultConfig);  //load default config
    if FileResult = 0 then
      if Config.WorkConfig <> DefaultConfig then          //try to load config pointed to by defaultconfig
        FileResult:= LoadConfig(FullCfgDir + Config.WorkConfig);
  end
  else
  begin
    if not CreateDir(DefaultCfgFolder) then  //if folder can't be created
    begin
      Config.CfgFolder:= '';
      FileResult:= LoadConfig(FullCfgDir + DefaultConfig); // try again without additional folder
      if FileResult = 0 then
        if Config.WorkConfig <> DefaultConfig then
          FileResult:= LoadConfig(FullCfgDir + Config.WorkConfig);
    end
    else
    begin
      ShowMessage('Не найдены файлы кофигурации.' + LineEnding +
                  'Создана папка по умолчанию ' + DefaultCfgFolder + '.');
      FileResult:= -1; //to still load defaults from optionform oncreate
    end;
  end;
end;

procedure tMainForm.FormDestroy(Sender: TObject);
begin
  btnDisconnectClick(Self);

  DoneCriticalSection(LogCS);
  ProgramLog.Free;
  DoneCriticalSection(CommCS);
end;

procedure tMainForm.FrequencyTabChange(Sender: TObject);
begin
  if FrequencyTab.TabIndex = 2 then
    eAmplitude.Enabled:= false
  else
    eAmplitude.Enabled:= cbACEnable.Enabled;
end;

procedure tMainForm.miNewReportClick(Sender: TObject);
begin
  ReportNumber:= 0; //so that it checks for existing file internally?
  ExperimentNumber:= 1;
  if DetControlForm.ConnectionKind <> cNone then
    ReportFolder:= DetControlForm.Log.FilePath
  else
    if TempControlForm.ConnectionKind <> cNone then
      ReportFolder:= TempControlForm.Log.FilePath
  else
    ReportFolder:= '';
  SaveReport(true);
end;

procedure tMainForm.miExportParamsClick(Sender: TObject);
begin
  SaveReport(true);
end;

procedure tMainForm.eAmplitudeChange(Sender: TObject);
begin                                       { TODO 2 -cBug : might be bugged, might need to extend}
  if cbACEnable.Checked and (eAmplitude.Value / 2 + abs(eOffset.Value) > eOffset.MaxValue) then
    eOffset.Value:= (eOffset.MaxValue - eAmplitude.Value/2) * sign(eOffset.Value);
end;

procedure tMainForm.cbAmplUnitChange(Sender: TObject);
begin
  if AmplitudeUnits[cbAmplUnit.ItemIndex] <> '' then
    AmplitudeUnit:= eUnits(cbAmplUnit.ItemIndex)
  else
    if ConnectionKind <> cNone then
      ShowMessage('Единица ' + cbAmplUnit.Text + ' не поддерживается прибором');
end;

procedure tMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Config.SaveParamsOnExit then
  begin
    FileResult:= SaveProfile(FullCfgDir + Config.ParamFile);
    WriteProgramLog('Сохранение параметров в ' + FullCfgDir + Config.ParamFile +
                    '; Результат: ' + strf(FileResult));
  end;

  FileResult:= SaveConfig(FullCfgDir + Config.WorkConfig);
  WriteProgramLog('Сохранение конфигурации в ' + FullCfgDir + Config.WorkConfig +
                  '; Результат: ' + strf(FileResult));
end;

procedure tMainForm.eOffsetChange(Sender: TObject);
begin
  {if cbACEnable.Checked and (abs(eOffset.Value) > 2 * eAmplitude.Value) then
    eOffset.Value:= 2 * eAmplitude.Value * sign(eOffset.Value);}
  { TODO 2 -cImprovement : same }
  if cbACEnable.Checked and (eAmplitude.Value / 2 + abs(eOffset.Value) > eOffset.MaxValue) then
    eAmplitude.Value:= 2 * (eOffset.MaxValue - abs(eOffset.Value));
end;

procedure tMainForm.eStepChange(Sender: TObject);
begin
  if ((eStepStartF.Value <> eStepStopF.Value) and (eFStep.Value <> 0)) or
     ((eStepStartA.Value <> eStepStopA.Value) and (eAStep.Value <> 0)) then
    StepForm.CalcSteps
  else
    TotalTime.Caption:= '0 ч 0 м 0 с, 0 шагов';
  if eFStep.Value = 0 then
    eStepStopF.Value:= eStepStartF.Value;
  if eAStep.Value = 0 then
    eStepStopA.Value:= eStepStartA.Value;
end;

procedure tMainForm.eSweepStartFChange(Sender: TObject);
begin
  if cbSweepType.ItemIndex = 1 then
  begin
    if (eSweepStartF.Value < eSweepStopF.Value) and (eSweepStartF.Value * 1000000 < eSweepStopF.Value) then
      eSweepStartF.Value:= eSweepStopF.Value / 1000000;                                                           { TODO 2 -cImprovement : Check this out }
    if (eSweepStartF.Value > eSweepStopF.Value) and (eSweepStartF.Value  > eSweepStopF.Value * 1000000) then
      eSweepStartF.Value:= eSweepStopF.Value * 1000000;
  end
end;

procedure tMainForm.eSweepStopFChange(Sender: TObject);
begin
  if cbSweepType.ItemIndex = 1 then
  begin
    if (eSweepStartF.Value < eSweepStopF.Value) and (eSweepStartF.Value * 1000000 < eSweepStopF.Value) then
    eSweepStopF.Value:= eSweepStartF.Value * 1000000;
    if (eSweepStartF.Value > eSweepStopF.Value) and (eSweepStartF.Value  > eSweepStopF.Value * 1000000) then
    eSweepStopF.Value:= eSweepStartF.Value / 1000000;
  end
end;

procedure tMainForm.cbPointPerStepDetChange(Sender: TObject);
begin
  if cbPointPerStepDet.Checked and (DetControlForm.cbReadingsMode.ItemIndex = integer(rBuffer)) then
  begin
    DetControlForm.cbReadingsMode.ItemIndex:= integer(rSimultaneous);
    DetControlForm.cbReadingsModeChange(Self);
    ShowMessage('Доступно только в режиме "Одновременный запрос".' + LineEnding +
    'Режим снятия переключен');
  end;
end;

procedure tMainForm.AboutClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure tMainForm.btAutoConnectClick(Sender: TObject);
begin
                  SetCursorAll(crHourGlass);
  DetControlForm. SetCursorAll(crHourGlass);
  TempControlForm.SetCursorAll(crHourGlass);

  AutoConnect;
  DetControlForm.AutoConnect;
  TempControlForm.AutoConnect;

                  SetCursorAll(crDefault);
  DetControlForm. SetCursorAll(crDefault);
  TempControlForm.SetCursorAll(crDefault);
end;

procedure tMainForm.miOptionsClick(Sender: TObject);
begin
  OptionForm.ShowModal;
end;

procedure tMainForm.miLoadCfgClick(Sender: TObject);
var
  s: ansistring;
begin
  with OpenDialog do
  begin
    InitialDir:= FullCfgDir;
    DefaultExt:= CfgExt;
    FileName:= '';
    Title:= 'Загрузить файл конфигурации';
    Filter:= 'Файлы конфигурации|*' + CfgExt + '|Все файлы|*.*';
    if Execute then
    begin
      s:= UTF8toANSI(FileName); //  проверить на доп символы
      LoadConfig(s);
    end
  end;
end;

procedure tMainForm.miSaveCfgClick(Sender: TObject);
var
  s: ansistring;
begin
  with SaveDialog do
  begin
    InitialDir:= FullCfgDir;
    DefaultExt:= CfgExt;
    FileName:= '';
    Title:= 'Сохранить файл конфигурации как';
    Filter:= 'Файлы конфигурации|*' + CfgExt + '|Все файлы|*.*';
    if Execute then
    begin
      s:= UTF8toANSI(FileName);
      SaveConfig(s);
    end;
    Config.WorkConfig:= s;
    SaveConfig(FullCfgDir + DefaultConfig);
  end;
end;

procedure tMainForm.miShowTempControlFClick(Sender: TObject);
begin
  if not miShowTempControlF.Checked then
  begin
    TempControlForm.Show;
    miShowTempControlF.Checked:= true;
  end
  else
  begin
    TempControlForm.Hide;
    miShowTempControlF.Checked:= false;
  end;
end;

procedure tMainForm.ReadingTimerStartTimer(Sender: TObject);
begin
  ReadingTimer.Interval:= Config.ReadingTime * 1000;
end;

procedure tMainForm.ReadingTimerTimer(Sender: TObject);
begin
  with DetControlForm do
    if Log.State = lActive then
      Log.Stop;

  with TempControlForm do
    if Log.State = lActive then
      Log.Stop;

  ReadingTimer.Enabled:= false;
end;

procedure tMainForm.StatusBarHint(Sender: TObject);
begin

end;

function tMainForm.FullCfgDir: string;
begin
  if Config.CfgFolder <> '' then
  begin
    if pos('\', Config.CfgFolder) = 0 then
      Result:= GetCurrentDir + '\' + Config.CfgFolder + '\'
    else
      Result:= Config.CfgFolder + '\'
  end
  else
    Result:= GetCurrentDir + '\';
end;

procedure tMainForm.miLoadProfileClick(Sender: TObject);
var
  s: ansistring;
begin
  with OpenDialog do
  begin
    InitialDir:= FullCfgDir;
    DefaultExt:= PrExt;
    FileName:= '';
    Title:= 'Загрузить файл параметров';
    Filter:= 'Файлы параметров|*' + PrExt + '|Все файлы|*.*';
    if Execute then
    begin
      s:= UTF8toANSI(FileName);
      LoadProfile(s);
    end;
  end;
end;

procedure tMainForm.miSaveProfileClick(Sender: TObject);
var
  s: ansistring;
begin
  with SaveDialog do
  begin
    InitialDir:= FullCfgDir;
    DefaultExt:= PrExt;
    FileName:= '';
    Title:= 'Сохранить файл параметров как';
    Filter:= 'Файлы параметров|*' + PrExt + '|Все файлы|*.*';
    if Execute then
    begin
      s:= UTF8toANSI(FileName);
      SaveProfile(s);
    end;
  end;
end;

procedure tMainForm.miShowReadingsFClick(Sender: TObject);
begin
  if not miShowReadingsF.Checked then
  begin
    DetControlForm.Show;
    miShowReadingsF.Checked:= true;
  end
  else
  begin
    DetControlForm.Hide;
    miShowReadingsF.Checked:= false;
  end;
end;

procedure tMainForm.cbFuncSelectChange(Sender: TObject);
var
  max: double;
begin
  if (length(FrequencyLimits) > 0) and (cbFuncSelect.ItemIndex >= 0) then
    max:= FrequencyLimits[cbFuncSelect.ItemIndex]
  else
    max:= maxdouble;
  eFrequency.MaxValue:= max;
  eSweepStartF.MaxValue:= max;
  eSweepStopF.MaxValue:= max;
  eStepStartF.MaxValue:= max;
  eStepStopF.MaxValue:= max;
end;

procedure tMainForm.cbImpedanceChange(Sender: TObject);
begin
  with cbImpedance do
  if (ItemIndex >= 0) and (length(MaxAmplitudeLimits) > 0) then
  begin
    eAmplitude.MaxValue:=  MaxAmplitudeLimits[ItemIndex];
    eStepStartA.MaxValue:= MaxAmplitudeLimits[ItemIndex];
    eStepStopA.MaxValue:=  MaxAmplitudeLimits[ItemIndex];
    eAmplitude.MinValue:=  MinAmplitudeLimits[ItemIndex];
    eStepStartA.MinValue:= MinAmplitudeLimits[ItemIndex];
    eStepStopA.MinValue:=  MinAmplitudeLimits[ItemIndex];
    eOffset.MaxValue:=     MaxAmplitudeLimits[ItemIndex] / 2;
    eOffset.MinValue:=   - eOffset.MaxValue
  end;

  if length(MinAmplitudeLimits) = 1 then
  begin
    eAmplitude.MaxValue:=  MaxAmplitudeLimits[0];
    eStepStartA.MaxValue:= MaxAmplitudeLimits[0];
    eStepStopA.MaxValue:=  MaxAmplitudeLimits[0];
    eAmplitude.MinValue:=  MinAmplitudeLimits[0];
    eStepStartA.MinValue:= MinAmplitudeLimits[0];
    eStepStopA.MinValue:=  MinAmplitudeLimits[0];
    eOffset.MaxValue:=     MaxAmplitudeLimits[0] / 2;
    eOffset.MinValue:=   - eOffset.MaxValue
  end;
end;

procedure tMainForm.cbSweepTypeChange(Sender: TObject);
begin                                                  { TODO 2 -cImprovement : Also questionable }
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

procedure tMainForm.btApplyClick(Sender: TObject);
var
  s: string;
begin
  with Params do
  begin
    Impedance:= cbImpedance.ItemIndex;
    CurrFunc:= cbFuncSelect.ItemIndex;
    Offset:= eOffset.Value;
    ACOn:= cbACEnable.Checked;
    AmplUnit:= cbAmplUnit.ItemIndex;
    if not cbAmplUnit.Visible then
      AmplitudeUnit:= uNone;

    EnterCriticalSection(CommCS);
      if cbImpedance.Visible then
        AddCommand(gResistance, false, iIm[Impedance]);
      AddCommand(gFunction, false, iFu[CurrFunc]);
      PassCommands;
    LeaveCriticalSection(CommCS);

    if FrequencyTab.TabIndex = StepTab then
    begin
      if Config.AutoReadingStep then       { TODO 2 -cImprovement : stuff like this really should throw an exception }
        if (DetControlForm.ConnectionKind = cNone) and
           (TempControlForm.ConnectionKind = cNone) then
          begin
            ShowMessage('Ошибка: Включено автоматическое снятие показаний,' + LineEnding +
                        'но ни один из приборов не подключен');

            WriteProgramLog('Невозможно начать считывание: приборы не подключены');
            CommandString:= '';
            exit
          end;

      if ((eStepStartF.Value <> eStepStopF.Value) and (eFStep.Value <> 0)) or
      ((eStepStartA.Value <> eStepStopA.Value) and (eAStep.Value <> 0)) then
      begin
        //assignments in stepf
        DetControlForm.OnePointPerStep:= cbPointPerStepDet.Checked;
        TempControlForm.OnePointPerStep:= cbPointPerStepTemp.Checked;
        AutoSweep:= false;
        StepStartF:= eStepStartF.Value;
        StepStopF:=  eStepStopF.Value;
        StepF:=      eFStep.Value;
        StepStartA:= eStepStartA.Value;
        StepStopA:=  eStepStopA.Value;
        StepA:=      eAStep.Value;
        TimeStep:=   eTimeStep.Value;

        StepForm.Show;
        if Config.AutoExportParams and not Config.AutoReadingStep then
        begin
          SaveReport(false);
        end;
      end
      else
        ShowMessage('Введены неверные параметры');
    end
    else
    begin
      EnterCriticalSection(CommCS);
        if ACOn then
          AddCommand(gAmplitude, false, eAmplitude.Value, AmplitudeUnit)
        else
          AddCommand(gAmplitude, false, '0' + AmplitudeUnits[integer(AmplitudeUnit)]);
        AddCommand(gOffset, false, Offset, uNone);
        PassCommands;
      LeaveCriticalSection(CommCS);

      if FrequencyTab.TabIndex = SweepTab then
      begin
        if Config.AutoReadingSweep then
          if (DetControlForm.ConnectionKind = cNone) and
             (TempControlForm.ConnectionKind = cNone) then
            begin
              ShowMessage('Ошибка: Включено автоматическое снятие показаний,' + LineEnding +
                          'но ни один из приборов не подключен');

              WriteProgramLog('Невозможно начать считывание: приборы не подключены');
              CommandString:= '';
              exit
            end;

        DetControlForm.LogFreq:= true;
        DetControlForm.cbUseGenFreq.Checked:= false;
        DetControlForm.LogAmpl:= false;
        if Config.AutoReadingSweep then
        begin
          SetCursorAll(crHourGlass);
          sleep(MinDelay);
          SetCursorAll(crDefault);
          DetControlForm.Log.Start;
          TempControlForm.Log.Start;
          if ReadingTime > 0 then
          begin
            ReadingTimer.Interval:= ReadingTime;
            ReadingTimer.Enabled:= true;
          end;
        end;

        EnterCriticalSection(CommCS);
          AddCommand(gSweepSource, false, integer(cbSweepRate.Checked));
          AddCommand(gSweepType, false, iST[cbSweepType.ItemIndex]);
          AddCommand(gSweepDirection, false, iSD[cbSweepDirection.ItemIndex]);
          AddCommand(gModulationWaveform , false, iMF[cbModulation.ItemIndex]);
          AddCommand(gSweepRate, false, eSweepRate.Value, uNone);
          AddCommand(gSweepStartFrequency, false, eSweepStartF.Value, uNone);
          AddCommand(gSweepStopFrequency, false, eSweepStopF.Value, uNone);
          AddCommand(gSweepEnable, false, 1);
          PassCommands;
        LeaveCriticalSection(CommCS);

        AutoSweep:= cbSweepRate.Checked;
        SweepStartF:= eSweepStartF.Value;
        SweepStopF:= eSweepStopF.Value;
        SweepRate:= eSweepRate.Value;
        SweepType:= cbSweepType.ItemIndex;
        SweepDir:= cbSweepDirection.ItemIndex;
        if Config.AutoExportParams and not Config.AutoReadingSweep then
        begin
          SaveReport(false);
        end;
      end
      else  //ConstFTab
      begin
        if Config.AutoReadingConst then
          if (DetControlForm.ConnectionKind = cNone) and
             (TempControlForm.ConnectionKind = cNone) then
            begin
              ShowMessage('Ошибка: Включено автоматическое снятие показаний,' + LineEnding +
                          'но ни один из приборов не подключен');

              WriteProgramLog('Невозможно начать считывание: приборы не подключены');
              CommandString:= '';
              exit
            end;

        DetControlForm.LogFreq:= false;
        DetControlForm.LogAmpl:= false;
        if Config.AutoReadingConst then
        begin
          SetCursorAll(crHourGlass);
          sleep(MinDelay);
          SetCursorAll(crDefault);
          DetControlForm.Log.Start;
          TempControlForm.Log.Start;
          if ReadingTime > 0 then
          begin
            ReadingTimer.Interval:= ReadingTime;
            ReadingTimer.Enabled:= true;
          end;
        end;
        Frequency:= eFrequency.Value;

        EnterCriticalSection(CommCS);
          AddCommand(gFrequency, false, Frequency, uNone);
          AddCommand(gSweepEnable, false, 0);
          PassCommands;
        LeaveCriticalSection(CommCS);

        if Config.AutoExportParams and not Config.AutoReadingConst then
        begin
          SaveReport(false);
        end;
      end;
    end;
    ParamsApplied:= true;
  end;

  str(eAmplitude.Value:0:2, s);     { TODO 2 -cImprovement : To events? properties? }
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

procedure tMainForm.btProgramClick(Sender: TObject);
begin
  enablecontrols(true);
  DetControlForm.enablecontrols(true);
  tempcontrolform.enablecontrols(true);
  Debug:= true;
  //DetControlForm.btnConnectClick(self);
end;

procedure tMainForm.btQueryClick(Sender: TObject);
var
  s, os, cs: string;
  i, e, l: integer;
  d: double;
begin
  Purge;
  EnterCriticalSection(CommCS);
    if cbImpedance.Visible then
      AddCommand(gResistance, true);
    AddCommand(gFunction, true);
    AddCommand(gAmplitude, true);
    AddCommand(gOffset, true);
    AddCommand(gFrequency, true);
    AddCommand(gSweepSource, true);
    AddCommand(gSweepType, true);
    AddCommand(gSweepDirection, true);
    if cbModulation.Visible then
      AddCommand(gModulationWaveform, true);
    AddCommand(gSweepRate, true);
    AddCommand(gSweepStartFrequency, true);
    AddCommand(gSweepStopFrequency, true);

    PassCommands;

    s:= RecvString;
  LeaveCriticalSection(CommCS);
  StatusBar.Panels[spStatus].Text:= s;

  cs:= CurrentDevice^.CommSeparator;
  l:= length(cs) - 1;

  if cbImpedance.Visible then
  begin
    cbImpedance.ItemIndex:= AnsiIndexStr(copy(s, 1, pos(cs, s) - 1), iIm);
    delete(s, 1, pos(cs, s) + l);
  end;

  cbFuncSelect.ItemIndex:= AnsiIndexStr(copy(s, 1, pos(cs, s) - 1), iFu); ;
  delete(s, 1, pos(cs, s) + l);

  os:= copy(s, 1, pos(cs, s) - 1);   //VP!
  e:= pos(AmplitudeUnits[0], os);
  if e > 0 then
  begin
    delete(os, e, length(AmplitudeUnits[0]));
    cbAmplUnit.ItemIndex:= 0;
  end
  else
  begin
    e:= pos(AmplitudeUnits[1], os);
    if e > 0 then
    begin
      delete(os, e, length(AmplitudeUnits[1]));
      cbAmplUnit.ItemIndex:= 1;
    end;
  end;
  val(os, d);
  str(d:0:6, os);
  AmplitudeReading.Caption:= os;

  delete(s, 1, pos(cs, s) + l);

  os:= copy(s, 1, pos(cs, s) - 1);
  val(os, d);
  str(d:0:6, os);
  OffsetReading.Caption:= os;
  delete(s, 1, pos(cs, s) + l);

  os:= copy(s, 1, pos(cs, s) - 1);
  val(os, d);
  str(d:0:6, os);
  FrequencyReading.Caption:= os;
  delete(s, 1, pos(cs, s) + l);

  val(copy(s, 1, pos(cs, s) - 1), i);
  cbSweepRate.Checked:= boolean(i); { TODO 2 -cImprovement : lacks func for boolean }
  delete(s, 1, pos(cs, s) + l);

  cbSweepType.ItemIndex:= AnsiIndexStr(copy(s, 1, pos(cs, s) - 1), iST);
  delete(s, 1, pos(cs, s) + l);

  cbSweepDirection.ItemIndex:= AnsiIndexStr(copy(s, 1, pos(cs, s) - 1), iSD); ;
  delete(s, 1, pos(cs, s) + l);

  if cbModulation.Visible then
  begin
    cbModulation.ItemIndex:= AnsiIndexStr(copy(s, 1, pos(cs, s) - 1), iMF);
    delete(s, 1, pos(cs, s) + l);
  end;

  os:= copy(s, 1, pos(cs, s) - 1);
  val(os, d);
  str(d:0:6, os);
  SweepRateReading.Caption:= os;
  delete(s, 1, pos(cs, s) + l);

  os:= copy(s, 1, pos(cs, s) - 1);
  val(os, d);
  str(d:0:6, os);
  SweepStartFReading.Caption:= os;
  delete(s, 1, pos(cs, s) + l);

  os:= s;
  val(os, d);
  str(d:0:6, os);
  SweepStopFReading.Caption:= os;
end;

procedure tMainForm.btStopClick(Sender: TObject);
begin
  EnterCriticalSection(CommCS);
    AddCommand(gSweepEnable, false, 0);
    AddCommand(gFrequency, false, 0, uNone);
    AddCommand(gAmplitude, false, 0, uVPeak);
    PassCommands;
  LeaveCriticalSection(CommCS);
end;

procedure tMainForm.btTriggerClick(Sender: TObject);
begin
  EnterCriticalSection(CommCS);
    AddCommand(cTrigger);
    PassCommands;
  LeaveCriticalSection(CommCS);
end;

procedure tMainForm.FormShow(Sender: TObject);
begin
  GetSupportedDevices(DeviceKind);

  if not FileExists(FullCfgDir + Config.ParamFile) then
    Config.ParamFile:= DefaultParams;
  if (Config.LoadParamsOnStart) and (FileResult = 0) then
    LoadProfile(FullCfgDir + Config.ParamFile);

  cbImpedanceChange(Self);
  if cbImpedance.ItemIndex < 0 then
    cbImpedance.ItemIndex:= 0;
  if cbModulation.ItemIndex < 0 then
    cbModulation.ItemIndex:= 0;
  eStepChange(Self);
  FrequencyTabChange(Self);
  DetControlForm.cbReadingsModeChange(DetControlForm);

  DetControlForm.Show;
  TempControlForm.Show;
end;

procedure tMainForm.btnConnectClick(Sender: TObject);
begin
  with DetControlForm do
  if cbPortSelect.ItemIndex = MainForm.cbPortSelect.ItemIndex then
  begin
    case ConnectionKind of
      cSerial:
      begin
        showmessage('К данному порту уже осуществляется подключение');
        exit
      end;
      cTelNet:
        if (CurrentDevice^.Host = MainForm.CurrentDevice^.Host) and
           (CurrentDevice^.Port = MainForm.CurrentDevice^.Port) then
          begin
            showmessage('По данному адресу уже осуществляется подключение');
            exit
          end;
    end;
  end;

  with TempControlForm do
  if cbPortSelect.ItemIndex = MainForm.cbPortSelect.ItemIndex then
  begin
    case ConnectionKind of
      cSerial:
      begin
        showmessage('К данному порту уже осуществляется подключение');
        exit
      end;
      cTelNet:
        if (CurrentDevice^.Host = MainForm.CurrentDevice^.Host) and
           (CurrentDevice^.Port = MainForm.CurrentDevice^.Port) then
          begin
            showmessage('По данному адресу уже осуществляется подключение');
            exit
          end;
    end;
  end;

  OptionForm.TabControl.TabIndex:= 0;
  inherited btnConnectClick(Sender);

  if DeviceIndex = iDefaultDevice then
    exit;

  FrequencyTabChange(Self);

  Params.GeneratorPort:= MainForm.CurrentDevice^.Port;
  Params.LastGenerator:= MainForm.CurrentDevice^.Model;
end;

procedure tMainForm.EnableControls(Enable: boolean);
begin
  cbImpedance.Enabled:=        Enable;
  cbFuncSelect.Enabled:=       Enable;
  cbAmplUnit.Enabled:=         Enable;
  eAmplitude.Enabled:=         Enable;
  eOffset.Enabled:=            Enable;
  eFrequency.Enabled:=         Enable;
  cbSweepRate.Enabled:=        Enable;
  eSweepRate.Enabled:=         Enable;
  SweepRateReading.Enabled:=   Enable;
  cbSweepType.Enabled:=        Enable;
  cbSweepDirection.Enabled:=   Enable;
  cbModulation.Enabled:=       Enable;
  eSweepStartF.Enabled:=       Enable;
  eSweepStopF.Enabled:=        Enable;
  btApply.Enabled:=            Enable;
  btTrigger.Enabled:=          Enable;
  btQuery.Enabled:=            Enable;
  btCustomCommand.Enabled:=    Enable;
  btStop.Enabled:=             Enable;
  btReset.Enabled:=            Enable;
  btStatus.Enabled:=           Enable;
  cbACEnable.Enabled:=         Enable;
  eStepStartF.Enabled:=        Enable;
  eStepStopF.Enabled:=         Enable;
  eFStep.Enabled:=             Enable;
  eStepStartA.Enabled:=        Enable;
  eStepStopA.Enabled:=         Enable;
  eAStep.Enabled:=             Enable;
  eTimeStep.Enabled:=          Enable;
  cbPointPerStepDet.Enabled:=  Enable;
  cbPointPerStepTemp.Enabled:= Enable;
  miNewReport.Enabled:=        Enable;
  miExportParams.Enabled:=     Enable;
end;

procedure tMainForm.GetDeviceParams;
var
  i: integer;
  s: string;
begin
  if debug then
  if DeviceIndex = 0 then
  begin
   deviceindex:= 1;
   connectionkind:= cserial;
   serport:= tblockserial.create;
  end;

  OptionForm.eDevice.ItemIndex:= DeviceIndex - 1;
  {cbImpedance.Items.Clear;
  cbFuncSelect.Items.Clear;
  cbSweepType.Items.Clear;
  cbSweepDirection.Items.Clear;
  cbModulation.Items.Clear;   }

  with DeviceForm.sgGenCommands do
  begin
    //cbImpedance.Items.AddText(Cells[DeviceIndex, integer(hResistanceOptions)]);
    SeparateIndices(Cells[DeviceIndex, integer(hResistanceOptions)], cbImpedance.Items, iIm);
    //cbFuncSelect.Items.AddText(Cells[DeviceIndex, integer(hFunctionOptions)]);
    SeparateIndices(Cells[DeviceIndex, integer(hFunctionOptions)], cbFuncSelect.Items, iFu);
    //cbSweepType.Items.AddText(Cells[DeviceIndex, integer(hSweepTypeOptions)]);
    SeparateIndices(Cells[DeviceIndex, integer(hSweepTypeOptions)], cbSweepType.Items, iST);

    //cbSweepDirection.Items.AddText(Cells[DeviceIndex, integer(hSweepDirectionOptions)]);
    SeparateIndices(Cells[DeviceIndex, integer(hSweepDirectionOptions)], cbSweepDirection.Items, iSD);

    //cbModulation.Items.AddText(Cells[DeviceIndex, integer(hModulationOptions)]);
    SeparateIndices(Cells[DeviceIndex, integer(hModulationOptions)], cbModulation.Items, iMF);


    MinDelay:= valf(Cells[DeviceIndex, integer(hMinDelay)]);
    eTimeStep.MinValue:= MinDelay;

    s:= Cells[DeviceIndex, integer(hMaxFrequencies)];
    if s <> '' then
    with MemoForm do
    begin
      mComment.Text:= s;
      setlength(FrequencyLimits, mComment.Lines.Count);
      for i:= 0 to mComment.Lines.Count - 1 do
        FrequencyLimits[i]:= valf(mComment.Lines[i]);
      mComment.Clear;
    end;

    s:= Cells[DeviceIndex, integer(hMinMaxAmplitudes)];
    if s <> '' then
    with MemoForm do
    begin
      mComment.Text:= s;
      setlength(MinAmplitudeLimits, mComment.Lines.Count);
      setlength(MaxAmplitudeLimits, mComment.Lines.Count);
      for i:= 0 to mComment.Lines.Count - 1 do
      begin
        s:= mComment.Lines[i];

        MinAmplitudeLimits[i]:= vald(Copy2SymbDel(s, '-'));
        MaxAmplitudeLimits[i]:= vald(s);
      end;
      mComment.Clear;
    end
    else
    begin
      setlength(MinAmplitudeLimits, 1);
      setlength(MaxAmplitudeLimits, 1);
      MinAmplitudeLimits[0]:= 0;
      MaxAmplitudeLimits[0]:= 20;
    end;

    s:= Cells[DeviceIndex, integer(hAmplitudeUnits)];
    if s <> '' then
    with MemoForm.mComment do
    begin
      Text:= s;
      for i:= 0 to Lines.Count - 1 do
        AmplitudeUnits[i]:= Lines[i];
      Clear;
      cbAmplUnit.Show;
      Label29.Show;
    end
    else
    begin
      cbAmplUnit.Hide;
      Label29.Hide;
    end;

    cbFuncSelectChange(Self);
  end;

  if cbImpedance.Items.Count = 0 then
  begin
    Label9.Hide;
    cbImpedance.Hide;
  end
  else
  begin
    Label9.Show;
    cbImpedance.Show;
  end;

  if cbModulation.Items.Count = 0 then
  begin
    cbModulation.Hide;
    cbSweepType.Width:= eSweepStartF.Width;
    cbSweepDirection.Width:= eSweepStartF.Width;
    cbSweepDirection.BorderSpacing.Left:= 16;
  end
  else
  begin
    cbModulation.Show;
    cbSweepType.Width:= cbModulation.Width;
    cbSweepDirection.Width:= cbModulation.Width;
    cbSweepDirection.BorderSpacing.Left:= 4;
  end;
end;

end.
