unit StepF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

type

  { TStepForm }

  TStepForm = class(TForm)
    btCancel: TButton;
    btPause: TButton;
    btFinish: TButton;
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
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    procedure btCancelClick(Sender: TObject);
    procedure btFinishClick(Sender: TObject);
    procedure btPauseClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    function CalcSteps: longint;
    { public declarations }
  end;

var
  Frequency, Amplitude, f, a, StartF, StartA, StopF, StopA, FStep, AStep: double;
  StartTime, ElapsedTime, PauseTime, PauseLength: TDateTime;
  StepNumber, StepMode: longword;
  Finished: boolean = false;
  StepForm: TStepForm;

implementation

uses DeviceF, MainF, ReadingsF, SerConF, GenConst;

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
    StepMode:= 2;
    ReadingsForm.LogAmpl:= true;
    ReadingsForm.LogFreq:= false;
    MainForm.eTimeStep.MinValue:= 150;
  end
  else
  if (AStep = 0) then
  begin
    StepNumber:= abs(trunc((StopF - StartF) / FStep));
    StepMode:= 1;
    ReadingsForm.LogAmpl:= false;
    ReadingsForm.LogFreq:= true;
    MainForm.eTimeStep.MinValue:= 150;
  end
  else
  begin
    StepMode:= 0;
    ReadingsForm.LogAmpl:= true;
    ReadingsForm.LogFreq:= true;
    MainForm.eTimeStep.MinValue:= 300;
    StepNumber:= abs(trunc((StopA - StartA) / AStep));
    if abs(trunc(StopF - StartF) / FStep) > StepNumber then StepNumber:= abs(trunc((StopF - StartF) / FStep));
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
  ReadingsForm.btStartPauseLog.Enabled:= false;
  ReadingsForm.btStopLog.Enabled:= false;

  CalcSteps;
  Frequency:= StartF;
  Amplitude:= StartA;

  case StepMode of
    0: Label7.Caption:= 'Проход по частоте и амплитуде';
    1: Label7.Caption:= 'Проход по частоте';
    2: Label7.Caption:= 'Проход по амплитуде';
  end;

  writeprogramlog(Label7.Caption);

  str(Amplitude:0:2, s);
  AmplitudeReading.Caption:= s;
  str(Frequency:19:6, s);
  FrequencyReading.Caption:= s;

  MainForm.AddCommand(gFrequency, false, Frequency, NOUNIT);
  MainForm.AddCommand(gAmplitude, false, Amplitude, AmplitudeUnit);
  MainForm.PassCommands;

  Finished:= false;
  btCancel.Enabled:= true;
  btPause.Enabled:= true;
  btFinish.Enabled:= false;
  Timer1.Interval:= MainForm.eTimeStep.Value;

  ProgressBar1.Max:= StepNumber;
  //ProgressBar1.Step:= ProgressBar1.Max div StepNumber;
  str(ProgressBar1.max, s);
  lTotalSteps.Caption:= '/' + s;
  lStep.Caption:= '0';
  ProgressBar1.Position:= 0;
  //ProgressBar1.;
  PauseTime:= 0;

  sleep(Params.Delay - Params.TimeStep);

  if Config.AutoReadingStep then
  begin
    ReadingsForm.OnePointPerStep:= MainForm.cbPointPerStep.Checked;
    Params.OnePoint:= MainForm.cbPointPerStep.Checked;
    ReadingsForm.BeginLog;
    ReadingsForm.UpdateTimer.Enabled:= false;
  end;

  sleep(Params.TimeStep);
  Timer1.Enabled:= true;
  StartTime:= Now;
end;

procedure TStepForm.Timer1Timer(Sender: TObject);
var
  s: string;
begin
  if Finished then
  begin
    ProgressBar1.StepIt;

    btCancel.Enabled:= false;
    btPause.Enabled:= false;
    btFinish.Enabled:= true;
    Timer1.Enabled:= false;
    Sleep(Timer1.Interval);
    if Config.AutoReadingStep then
    begin
      ReadingsForm.ProcessBuffers;
      ReadingsForm.StopLog;
    end;
  end
  else
  begin
    if MainForm.cbPointPerStep.Checked then
      ReadingsForm.ReadingNeeded:= true;

    ElapsedTime:= Now - StartTime - PauseLength;
    DateTimeToString(s, 'hh:mm:ss', ElapsedTime);
    Elapsed.Caption:= s;
    ProgressBar1.StepIt;

    str(Amplitude:0:2, s);
    AmplitudeReading.Caption:= s;
    str(Frequency:10:6, s);
    FrequencyReading.Caption:= s;
    str(ProgressBar1.Position, s);
    lStep.Caption:= s;

    if MainForm.cbPointPerStep.Checked then
      repeat
      until ReadingsForm.ReadingsThreadDone;

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
      0:
        begin
          MainForm.AddCommand(gFrequency, false, Frequency, NOUNIT);
          MainForm.AddCommand(gAmplitude, false, Amplitude, AmplitudeUnit);
        end;
      1:
        begin
          MainForm.AddCommand(gFrequency, false, Frequency, NOUNIT);
          StopA:= Amplitude;
        end;
      2:
        begin
          MainForm.AddCommand(gAmplitude, false, Amplitude, AmplitudeUnit);
          StopF:= Frequency;
        end;
      end;
    end;

    ReadingsForm.ProcessBuffers;
    MainForm.PassCommands;
  end;
end;

procedure TStepForm.FormHide(Sender: TObject);
begin
  MainForm.CommandString:= '';
  Timer1.Enabled:= false;
  MainForm.EnableControls(true);
  ReadingsForm.btStartPauseLog.Enabled:= true;
  ReadingsForm.btStopLog.Enabled:= true;
end;

procedure TStepForm.btPauseClick(Sender: TObject);
begin
  if Timer1.Enabled then
  begin
    PauseTime:= Now;
    Timer1.Enabled:= false;
    if Config.AutoReadingStep then ReadingsForm.PauseLog;
    btPause.Caption:= 'Продолжить';
  end
  else
  begin
    PauseLength+= Now - PauseTime;
    Timer1.Enabled:= true;
    if Config.AutoReadingStep then ReadingsForm.ContinueLog;
    btPause.Caption:= 'Пауза';
  end;
end;

procedure TStepForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ReadingsForm.LogState <> lInActive then CanClose:= false
  else CanClose:= true;
end;

procedure TStepForm.btCancelClick(Sender: TObject);
begin
  Finished:= true;
  Timer1.Enabled:= true;
  if Config.AutoReadingStep then ReadingsForm.StopLog;
end;

procedure TStepForm.btFinishClick(Sender: TObject);
begin
  StepForm.Close;
end;

end.

