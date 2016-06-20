unit OffsetF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  Spin, StdCtrls, ComCtrls;

type

  { TOffsetForm }

  TOffsetForm = class(TForm)
    btApply: TButton;
    btAutoOffset: TButton;
    btQuery: TButton;
    cbExpand: TComboBox;
    cbParams: TComboBox;
    DividerBevel1: TDividerBevel;
    eOffset: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure btApplyClick(Sender: TObject);
    procedure btAutoOffsetClick(Sender: TObject);
    procedure btQueryClick(Sender: TObject);
    procedure cbParamsChange(Sender: TObject);
  private
    { private declarations }

    ParamIndices: array of string;
  public
    Offsets: array of double;
    Expands: array of byte;
    { public declarations }
    function GetParams: integer;
  end;

var
  OffsetForm: TOffsetForm;

implementation

{$R *.lfm}

uses StrUtils, DeviceF, SerConF, DetControlF;

{ TOffsetForm }

procedure TOffsetForm.cbParamsChange(Sender: TObject);
begin
  eOffset.Value:= Offsets[cbParams.ItemIndex];
  cbExpand.ItemIndex:= Expands[cbParams.ItemIndex];
end;

function TOffsetForm.GetParams: integer;
var
  s: string;
  i: integer;
begin
  with DeviceForm.sgDetCommands do
    s:= Cells[DetControlForm.DeviceIndex, integer(hOffsetParams)];
  with cbParams do
  begin
    Items.Clear;
    Items.AddText(s);
    setlength(ParamIndices, Items.Count);
    setlength(Offsets, Items.Count);
    setlength(Expands, Items.Count);
    Result:= Items.Count;
    for i:= 0 to Items.Count - 1 do
    begin
      s:= Items[i];
      ParamIndices[i]:= Copy2SymbDel(s, ' ');
      Items[i]:= s;
    end;
  end;
end;

procedure TOffsetForm.btQueryClick(Sender: TObject);
var
  s, cs: string;
  i, l, e: integer;
  d: double;
begin
  with DetControlForm, cbParams do
  begin
    Purge;
    if CommandSupported(dExpand) then
    begin
      EnterCriticalSection(CommCS);
        AddCommand(dOffset, true, ParamIndices[ItemIndex]);
        PassCommands;
        s:= RecvString;
      LeaveCriticalSection(CommCS);

      val(s, d, e);
      if e <> 0 then exit;
      eOffset.Value:= d;

      EnterCriticalSection(CommCS);
        AddCommand(dExpand, true, ParamIndices[ItemIndex]);
        PassCommands;
        s:= RecvString;
      LeaveCriticalSection(CommCS);

      val(s, i, e);
      if e <> 0 then exit;
      cbExpand.ItemIndex:= i;
    end
    else
    begin
      EnterCriticalSection(CommCS);
        AddCommand(dOffset, true,
          tVariantArray.Create(ParamIndices[ItemIndex], Offsets[ItemIndex])
                   );
        PassCommands;
        s:= RecvString;
      LeaveCriticalSection(CommCS);

      StatusBar.Panels[spStatus].Text:= s;

      cs:= CurrentDevice^.ParSeparator;
      l:= length(cs) - 1;

      val(copy(s, 1, pos(cs, s) - 1), d, e);
      if e <> 0 then exit;
      eOffset.Value:= d;
      delete(s, 1, pos(cs, s) + l);

      val(s, i, e);
      if e <> 0 then exit;
      cbExpand.ItemIndex:= i;
    end;

    Offsets[ItemIndex]:= eOffset.Value;
    Expands[ItemIndex]:= cbExpand.ItemIndex;
  end;
end;

procedure TOffsetForm.btAutoOffsetClick(Sender: TObject);
begin
  with DetControlForm, cbParams do
  begin
    EnterCriticalSection(CommCS);
      AddCommand(dAutoOffset, false, ParamIndices[ItemIndex]);
      PassCommands;
    LeaveCriticalSection(CommCS);
  end;
  btQueryClick(Self);
end;

procedure TOffsetForm.btApplyClick(Sender: TObject);
begin
  with DetControlForm, cbParams do
  begin
    Offsets[ItemIndex]:= eOffset.Value;
    Expands[ItemIndex]:= cbExpand.ItemIndex;

    if CommandSupported(dExpand) then
    begin
      EnterCriticalSection(CommCS);
        AddCommand(dOffset, false,
          tVariantArray.Create(ParamIndices[ItemIndex], Offsets[ItemIndex])
                   );
        AddCommand(dExpand, false,
          tVariantArray.Create(ParamIndices[ItemIndex], Expands[ItemIndex])
                   );
        PassCommands;
      LeaveCriticalSection(CommCS);
    end
    else
    begin
      EnterCriticalSection(CommCS);
        AddCommand(dOffset, false,
          tVariantArray.Create(ParamIndices[ItemIndex], Offsets[ItemIndex], Expands[ItemIndex])
                   );
        PassCommands;
      LeaveCriticalSection(CommCS);
    end;
  end;
end;

end.

