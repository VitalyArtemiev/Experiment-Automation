unit AxisSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { tAxisSource }

  tAxisSource = class
  private
    fCapacity: longword;
    procedure SetCapacity(AValue: longword); inline;
  public
    Values: array of double;
    Count: longword;
    property Capacity: longword read fCapacity write SetCapacity;

    constructor Create(InitialCap: longword = 4);
    destructor Destroy; override;

    procedure Add(b: array of double);
    procedure Add(v: double); inline;
    procedure Clear;
  end;

implementation

uses
  math;

{ tAxisSource }

procedure tAxisSource.SetCapacity(AValue: longword);
begin
  if fCapacity = AValue then Exit;
  fCapacity:= AValue;
  setlength(Values, fCapacity);
end;

constructor tAxisSource.Create(InitialCap: longword = 4);
begin
  Count:= 0;
  if InitialCap > 4 then
    Capacity:= InitialCap
  else
    Capacity:= 4;
end;

destructor tAxisSource.Destroy;
begin
  setlength(Values, 0);
end;

procedure tAxisSource.Add(b: array of double);
var
  i, c: integer;
begin
  c:= Count;
  Count+= length(b);
  if Count > Capacity then
    Capacity:= Capacity + max(Capacity shr 2, length(b));
  for i:= 0 to high(b) do
    Values[c + i]:= b[i];
end;

procedure tAxisSource.Add(v: double);
begin
  inc(Count);
  if Count > Capacity then
    Capacity:= Capacity + Capacity shr 2;
  Values[Count - 1]:= v;
end;

procedure tAxisSource.Clear;
begin
  Count:= 0;
  Capacity:= 4;
end;

end.

