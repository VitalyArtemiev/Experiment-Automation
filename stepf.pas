unit StepF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

type

  eStepMode = (sBoth, sFrequency, sAmplitude);

  { TStepForm }

  TStepForm = class(TForm)
    btCancel: TButton;
    btPause: TButton;
    btFinish: TButton;
    DividerBevel1: TDividerBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    AmplitudeReading: TLabel;
    FrequencyReading: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lTotalSteps: TLabel;
    lStep: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Elapsed: TLabel;
    ProgressBar: TProgressBar;
    Timer: TTimer;
    procedure btCancelClick(Sender: TObject);
    procedure btFinishClick(Sender: TObject);
    procedure btPauseClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { private declarations }
  public
    Finished: boolean;
    function CalcSteps: longint;
    { public declarations }
  end;

var
  Frequency, Amplitude, f, a, StartF, StartA, StopF, StopA, FStep, AStep: double;
  StartTime, ElapsedTime, PauseTime, PauseLength: TDateTime;
  StepNumber: longword;
  StepMode: eStepMode;
  StepForm: TStepForm;

implementation

uses
  math, DeviceF, MainF, DetControlF, TempControlF, LogModule, ReadingThreads, BaseConF;

{$R *.lfm}

{ TStepForm }

function TStepForm.CalcSteps: longint;
var
  t: qword;
  s: string;
begin
  StartF:= MainForm.eStepStartF.Value;
  StartA:= MainForm.eStepStartA.Value;
  FStep:= MainForm.eFStep.Value;
  AStep:= MainForm.eAStep.Value;
  StopF:= MainForm.eStepStopF.Value;
  StopA:= MainForm.eStepStopA.Value;
  if StartF > StopF then FStep:= - FStep;
  if StartA > StopA then AStep:= - AStep;

  if (Fstep = 0) then
  begin
    StepNumber:= abs(trunc((StopA - StartA) / AStep));
    StepMode:= sAmplitude;

    with DetControlForm do
    begin
      LogAmpl:= true;
      LogFreq:= false;
    end;
    with TempControlForm do
    begin
      LogAmpl:= true;
      LogFreq:= false;
    end;

    MainForm.eTimeStep.MinValue:= 150;
  end
  else
  if (AStep = 0) then
  begin
    StepNumber:= abs(trunc((StopF - StartF) / FStep));
    StepMode:= sFrequency;

    with DetControlForm do
    begin
      LogAmpl:= false;
      LogFreq:= true;
    end;
    with TempControlForm do
    begin
      LogAmpl:= false;
      LogFreq:= true;
    end;

    MainForm.eTimeStep.MinValue:= 150;
  end
  else
  begin
    StepMode:= sBoth;

    with DetControlForm do
    begin
      LogAmpl:= true;
      LogFreq:= true;
    end;
    with TempControlForm do
    begin
      LogAmpl:= true;
      LogFreq:= true;
    end;

    MainForm.eTimeStep.MinValue:= 300;
    StepNumber:= abs(trunc((StopA - StartA) / AStep));
    if abs(trunc(StopF - StartF) / FStep) > StepNumber then
      StepNumber:= abs(trunc((StopF - StartF) / FStep));
  end;
  CalcSteps:= StepNumber;
  t:= StepNumber * MainForm.eTimeStep.Value;

  str(t div 3600000, s);
  MainForm.TotalTime.Caption:= s + ' ч ';
  str((t mod 3600000) div 60000, s);
  MainForm.TotalTime.Caption:= MainForm.TotalTime.Caption + s + ' м ';
  str((t mod 60000) div 1000, s);
  MainForm.TotalTime.Caption:= MainForm.TotalTime.Caption + s + ' с, ';
  str(StepNumber, s);
  MainForm.TotalTime.Caption:= MainForm.TotalTime.Caption + s;

  case StepNumber mod 10 of
    1: MainForm.TotalTime.Caption:= MainForm.TotalTime.Caption + ' шаг';
    2, 3, 4: MainForm.TotalTime.Caption:= MainForm.TotalTime.Caption + ' шага';
    else MainForm.TotalTime.Caption:= MainForm.TotalTime.Caption + ' шагов';
  end;
end;

procedure TStepForm.FormShow(Sender: TObject);
var
  s: string;
begin
  CalcSteps;
  Frequency:= StartF;
  Amplitude:= StartA;

  case StepMode of
    sBoth: Label7.Caption:= 'Проход по частоте и амплитуде';
    sFrequency: Label7.Caption:= 'Проход по частоте';
    sAmplitude: Label7.Caption:= 'Проход по амплитуде';
  end;

  writeprogramlog(Label7.Caption);

  str(Amplitude:0:2, s);
  AmplitudeReading.Caption:= s;
  str(Frequency:19:6, s);
  FrequencyReading.Caption:= s;

  with MainForm do
  begin
    EnableControls(false);
    pnConnection.Enabled:= false;
    btQuery.Enabled:= true;
    btStatus.Enabled:= true;
    btCustomCommand.Enabled:= true;
    btStop.Enabled:= true;
    Cursor:= crAppStart;
    EnterCriticalSection(CommCS);
      AddCommand(gSweepEnable, false, 0);
      AddCommand(gFrequency, false, Frequency, uNone);
      AddCommand(gAmplitude, false, Amplitude, AmplitudeUnit);
      AddCommand(gOffset, false, Params.Offset, uNone);
      PassCommands;
    LeaveCriticalSection(CommCS);
  end;

  if Config.AutoReadingStep then
  begin
    with DetControlForm do
      if ConnectionKind <> cNone then
      begin
        btStartPauseLog.Enabled:= false;
        btStopLog.Enabled:= false;
      end;
    with TempControlForm do
      if ConnectionKind <> cNone then
      begin
        btStartPauseLog.Enabled:= false;
        btStopLog.Enabled:= false;
      end;
  end;

  btCancel.Enabled:= true;
  btPause.Enabled:= true;
  btFinish.Enabled:= false;
  Timer.Interval:= MainForm.eTimeStep.Value;

  ProgressBar.Max:= StepNumber;
  str(ProgressBar.max, s);
  lTotalSteps.Caption:= '/' + s;
  lStep.Caption:= '0';
  ProgressBar.Position:= 0;
  PauseTime:= 0;

  Finished:= false;

  Cursor:= crHourGlass;
  if Config.AutoReadingStep then
  begin
    sleep(MainForm.MinDelay);
    with DetControlForm do
      if ConnectionKind <> cNone then
      begin
        btApplyClick(Self);
        CreateFile(Log);
        Log.GetExperimentNumber;
      end;
    with TempControlForm do
      if ConnectionKind <> cNone then
      begin
        btApplyClick(Self);
        CreateFile(Log);
        Log.GetExperimentNumber;
      end;

    sleep(max(DetControlForm.eDelay.Value * integer( DetControlForm.ConnectionKind <> cNone),
             TempControlForm.eDelay.Value * integer(TempControlForm.ConnectionKind <> cNone)));

    with DetControlForm do
    begin
      AutoApply:= false;
      if ConnectionKind <> cNone then
        Log.Start;
    end;
    with TempControlForm do
    begin
      AutoApply:= false;
      if ConnectionKind <> cNone then
        Log.Start;
    end;
    {if MainForm.cbPointPerStepDet.Checked then
      DetControlForm.UpdateTimer.Enabled:= false; }
  end;

  sleep(Params.TimeStep);
  Cursor:= crDefault;

  Timer.Enabled:= true;
  StartTime:= Now;
end;

procedure TStepForm.TimerTimer(Sender: TObject);
var
  s: string;
begin
  if Finished then
  begin
    ProgressBar.StepIt;

    btCancel.Enabled:= false;
    btPause.Enabled:= false;
    btFinish.Enabled:= true;
    Timer.Enabled:= false;
    btPause.Caption:= 'Пауза';
    Sleep(Timer.Interval);
    if Config.AutoReadingStep then
    begin
      with DetControlForm do
      if ConnectionKind <> cNone then
      begin
        Log.ProcessBuffers;
        Log.Stop;
      end;
      with TempControlForm do
      if ConnectionKind <> cNone then
      begin
        Log.ProcessBuffers;
        Log.Stop;
      end;
    end;
  end
  else
  begin
    with DetControlForm do
      if OnePointPerStep and (ConnectionKind <> cNone) then
        Log.ReadingsThread:= tDetOnePerStepThread.Create;
    with TempControlForm do
      if OnePointPerStep and (ConnectionKind <> cNone) then
        Log.ReadingsThread:= tTempOnePerStepThread.Create;

    ElapsedTime:= Now - StartTime - PauseLength;
    DateTimeToString(s, 'hh:mm:ss', ElapsedTime);
    Elapsed.Caption:= s;
    ProgressBar.StepIt;

    str(Amplitude:0:2, s);
    AmplitudeReading.Caption:= s;
    str(Frequency:10:6, s);
    FrequencyReading.Caption:= s;
    str(ProgressBar.Position, s);
    lStep.Caption:= s;

    with DetControlForm do
      if OnePointPerStep and (ConnectionKind <> cNone) then
      begin
        Log.ReadingsThread.WaitFor;
        freeandnil(Log.ReadingsThread);
      end;
    with TempControlForm do
      if OnePointPerStep and (ConnectionKind <> cNone) then
      begin
        Log.ReadingsThread.WaitFor;
        freeandnil(Log.ReadingsThread);
      end;

    f:= Frequency;
    a:= Amplitude;

    if (Amplitude = StopA) and (Frequency = StopF) then Finished:= true
    else
    begin
      Frequency+= FStep;
      Amplitude+= AStep;

      if (AStep > 0) and (Amplitude > StopA) then Amplitude:= StopA
      else
      if (AStep < 0) and (Amplitude < StopA) then Amplitude:= StopA;

      if (FStep > 0) and (Frequency > StopF) then Frequency:= StopF
      else
      if (FStep < 0) and (Frequency < StopF) then Frequency:= StopF;

      case StepMode of
      sBoth:
        begin
          MainForm.AddCommand(gFrequency, false, Frequency, uNone);
          MainForm.AddCommand(gAmplitude, false, Amplitude, AmplitudeUnit);
        end;
      sFrequency:
        begin
          MainForm.AddCommand(gFrequency, false, Frequency, uNone);
          StopA:= Amplitude;
        end;
      sAmplitude:
        begin
          MainForm.AddCommand(gAmplitude, false, Amplitude, AmplitudeUnit);
          StopF:= Frequency;
        end;
      end;
    end;

    with DetControlForm do
      if OnePointPerStep and (ConnectionKind <> cNone) then
        Log.ProcessBuffers;
    with TempControlForm do
      if OnePointPerStep and (ConnectionKind <> cNone) then
        Log.ProcessBuffers;
    MainForm.PassCommands;
  end;
end;

procedure TStepForm.FormHide(Sender: TObject);
begin
  MainForm.CommandString:= '';
  Timer.Enabled:= false;
  MainForm.EnableControls(true);
  MainForm.pnConnection.Enabled:= true;
  MainForm.Cursor:= crDefault;
  DetControlForm.btStartPauseLog.Enabled:= true;
  DetControlForm.btStopLog.Enabled:= true;
  TempControlForm.btStartPauseLog.Enabled:= true;
  TempControlForm.btStopLog.Enabled:= true;
end;

procedure TStepForm.btPauseClick(Sender: TObject);
begin
  if Timer.Enabled then
  begin
    PauseTime:= Now;
    Timer.Enabled:= false;

    with DetControlForm do
      if Config.AutoReadingStep and (ConnectionKind <> cNone) then
        Log.Pause;
    with TempControlForm do
      if Config.AutoReadingStep and (ConnectionKind <> cNone) then
        Log.Pause;

    btPause.Caption:= 'Продолжить';
  end
  else
  begin
    PauseLength+= Now - PauseTime;
    Timer.Enabled:= true;

    with DetControlForm do
      if Config.AutoReadingStep and (ConnectionKind <> cNone) then
        Log.Continue;
    with TempControlForm do
      if Config.AutoReadingStep and (ConnectionKind <> cNone) then
        Log.Continue;

    btPause.Caption:= 'Пауза';
  end;
end;

procedure TStepForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if (DetControlForm.Log.State <> lInActive) and
     (TempControlForm.Log.State <> lInActive) then
    CanClose:= false
  else
    CanClose:= true;
end;

procedure TStepForm.FormCreate(Sender: TObject);
begin
  Finished:= true;
end;

procedure TStepForm.btCancelClick(Sender: TObject);
begin
  Finished:= true;
  Timer.Enabled:= true;

  with DetControlForm do
      if Config.AutoReadingStep and (ConnectionKind <> cNone) then
        Log.Stop;
  with TempControlForm do
    if Config.AutoReadingStep and (ConnectionKind <> cNone) then
      Log.Stop;
end;

procedure TStepForm.btFinishClick(Sender: TObject);
begin
  StepForm.Close;
end;

end.

