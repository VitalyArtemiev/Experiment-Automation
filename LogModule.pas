unit LogModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AxisSource;

type

  eLogState = (lInActive, lActive, lPaused);

  { tLogModule }

  tLogModule = class
  private
    FLogState: eLogState;
    FOnLogStateChange: tNotifyEvent;
    ProcessedPoints: longword;
    StartTime, PauseTime, PauseLength: TDateTime;
    srcTime: TAxisSource;
    TimeCS: tRTLCriticalSection;
    CoordinateSources: array of TAxisSource;

    function GetTime: TDateTime;
    procedure SetLogState(AValue: eLogState);
  public
    Filename: string;

    procedure CreateFile;
    procedure SaveFile;
    property OnLogStateChange: tNotifyEvent read FOnLogStateChange write FOnLogStateChange;
    //property OnLogStateChange: tNotifyEvent;
    property LogState: eLogState read FLogState write SetLogState;
    property ElapsedTime: TDateTime read GetTime;
  end;

implementation

{ tLogModule }

function tLogModule.GetTime: TDateTime;
begin
  EnterCriticalSection(TimeCS);
  try
    Result:= Now - StartTime - PauseLength;
  finally
    LeaveCriticalSection(TimeCS);
  end;
end;

procedure tLogModule.SetLogState(AValue: eLogState);
begin
  FLogState:= AValue;
  if Assigned(OnLogStateChange) then
    onLogStateChange(Self);
end;

procedure tLogModule.CreateFile;
begin

end;

procedure tLogModule.SaveFile;
begin

end;

end.

