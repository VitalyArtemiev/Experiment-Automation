unit DevSession;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Visa, Session;

type

  { tDeviceSession }

  tDeviceSession = class(tVisaSession)
    function GetDeviceList(IPAddress: string): string;
  end;

implementation

{ tDeviceSession }

function tDeviceSession.GetDeviceList(IPAddress: string): string;
begin
  //viFindRsrc(
end;

end.

