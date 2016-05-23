unit menuh;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Controls, ComCtrls, Forms;

type

  tHintRedirStatusBar = class(tStatusBar)
    function DoSetApplicationHint(AHintStr: String): Boolean; override;
  end;

implementation

uses MainF;

function THintRedirStatusBar.DoSetApplicationHint(AHintStr: String): Boolean;
var
  HintInfo: THintInfoAtMouse{ = record
    MousePos: TPoint;
    Control: TControl;
    ControlHasHint: boolean;
  end};
begin
  {if AHintStr <> '' then} //showmessage(AHintStr);

  Application.ShowHint:= true;
  Application.Hint:= AHintStr;
  MainForm.Statusbar.Hint:= AHintStr;
  HintInfo.Control:= MainForm.Statusbar;
  HintInfo.ControlHasHint:= true;
  HintInfo.MousePos:= Mouse.CursorPos;

  Application.ShowHintWindow(HintInfo);

  {with MainForm.PopupNotifier do
  begin
    Text:= AHintStr;
    if Text <> '' then
      ShowAtPos(Mouse.CursorPos.x + 50, Mouse.CursorPos.y);
  end;}
  //Application. ActivateHint(Mouse.CursorPos, true);
  //Application.Hint:= '';
  Result:= true;
  {Result := DoHint;
  if Result then
    Exit;
  if SimplePanel then
    SimpleText := AHintStr
  else
  if Panels.Count > 0 then
    Panels[0].Text := AHintStr;
  Result := True;}
end;

initialization
begin
  RegisterClass(THintRedirStatusBar);
end;

end.

