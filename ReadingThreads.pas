unit ReadingThreads;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, serconf;

type

  { tBufferThread }

  tBufferThread = class(tThread)
    constructor Create(BufferLength: longword);
    procedure Execute; override;
  private
    BuffByte1, BuffByte2: array of byte;
    ParamArr: tIntegerArray;
  end;

 { tSimultaneousThread }

 tSimultaneousThread = class(tThread)
   DataList: TList;
   procedure Execute; override;
   constructor Create;
 end;

 { tOnePerStepThread }

 tOnePerStepThread = class(tThread)
   DataList: TList;
   procedure Execute; override;
   constructor Create;
 end;

implementation

uses math, DeviceF, ReadingsF;

{ tOnePerStepThread }

constructor tOnePerStepThread.Create;
begin
  inherited Create(false);
  FreeOnTerminate:= false;
end;

procedure tOnePerStepThread.Execute;
var
  pd: PBuffer;
  pt: ^TDateTime;
begin
  with ReadingsForm do
  begin
    try
      pd:= RecvSnap(ParToRead);

    except on E:Exception do
      writeprogramlog(E.Message);
    end;

     if pd <> nil then
    begin
      new(pt);
      pt^:= ElapsedTime;
      DataList:= ThreadList.LockList;
        DataList.Add(pt);
        DataList.Add(pd);
      ThreadList.UnlockList;
      inc(ReadPoints);
    end;
  end;
end;

{ tSimultaneousThread }

constructor tSimultaneousThread.Create;
begin
  inherited Create(false);
  FreeOnTerminate:= false;
end;

procedure tSimultaneousThread.Execute;
var
  pd: PBuffer;
  pt: ^TDateTime;
begin
  with ReadingsForm do
  repeat
    try
      pd:= RecvSnap(ParToRead);

    except on E:Exception do
      writeprogramlog(E.Message);
    end;

    if pd <> nil then
    begin
      new(pt);
      pt^:= ElapsedTime;
      DataList:= ThreadList.LockList;
        DataList.Add(pt);
        DataList.Add(pd);
      ThreadList.UnlockList;
      inc(ReadPoints);
    end
  until Terminated;
end;

{ tBufferThread }

constructor tBufferThread.Create(BufferLength: longword);
begin
  setlength(BuffByte1, BufferLength * 4);
  setlength(BuffByte2, BufferLength * 4);
  setlength(ParamArr, 3);
  inherited Create(false);
  FreeOnTerminate:= false;
end;

procedure tBufferThread.Execute;
var
  StoredPoints: longint = 0;
  PointsToRead: longint;
  p1, p2: PBuffer;
  i: integer;
  s: string;
  o: ^smallint;
  DataList: TList;
begin
  with ReadingsForm do
  repeat
    try
      EnterCriticalSection(CommCS);
      AddCommand(dStoredPoints, true);               //paus+
      PassCommands;
      s:= RecvString;
    finally
      LeaveCriticalSection(CommCS);
    end;

    val(s, StoredPoints);
    //AddCommand(PAUS, false);
    //PassCommands;
    PointsToRead:= StoredPoints - ReadPoints;

    if PointsToRead > 0 then
    begin
      Serport.RaiseExcept:= false;

      ParamArr[0]:= 1;
      ParamArr[1]:= ReadPoints;
      ParamArr[2]:= PointsToRead;
      try
        EnterCriticalSection(CommCS);
          AddCommand(dReadPointsNative, true, ParamArr);
          PassCommands;
          SerPort.RecvBufferEx(@BuffByte1[0], PointsToRead * 4, CurrentDevice^.Timeout);

          ParamArr[0]:= 2;

          AddCommand(dReadPointsNative, true, ParamArr);
          PassCommands;

          SerPort.RecvBufferEx(@BuffByte2[0], PointsToRead * 4, CurrentDevice^.Timeout);
      finally
          Serport.RaiseExcept:= true;
        LeaveCriticalSection(CommCS);
      end;

      new(p1);
      new(p2);
      setlength(p1^, PointsToRead);
      setlength(p2^, PointsToRead);
      for i:= 0 to PointsToRead - 1 do
      begin
        o:= @BuffByte1[i*4];
        p1^[i]:= smallint(o^) * power(2, BuffByte1[i*4 + 2] - 124);    //конвертация из внутр. формата

        o:= @BuffByte2[i*4];
        p2^[i]:= smallint(o^) * power(2, BuffByte2[i*4 + 2] - 124);
      end;

      DataList:= ThreadList.LockList;
        DataList.Add(p1);
        DataList.Add(p2);
        i:= DataList.Count;
      ThreadList.UnlockList;
      ReadPoints:= StoredPoints;
    end;
    //AddCommand(STRT, false);
    //PassCommands;
    { TODO 1 -cBug : Continuos buffer }
  until Terminated;
end;

end.
