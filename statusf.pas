unit StatusF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TStatusForm }

  TStatusForm = class(TForm)
    btApply: TButton;
    btPoll: TButton;
    btLastPoll: TButton;
    eDDS: TEdit;
    eESB: TEdit;
    eLIAS: TEdit;
    eSESB: TEdit;
    eSPSB: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label10: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    lDDSB: TLabel;
    lESB: TLabel;
    lLIASB: TLabel;
    lSESB: TLabel;
    lSPSB: TLabel;
    Panel1: TPanel;

    procedure btApplyClick(Sender: TObject);
    procedure btLastPollClick(Sender: TObject);
    procedure btPollClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    SerPollSB, StdEventSB, LiaSB, ErrSB, DDSB,
      eSerPollSB, eStdEventSB, eLiaSB, eErrSB, eDDSB,
      pSerPollSB, pStdEventSB, pLiaSB, pErrSB, pDDSB: byte;
    { private declarations }
  public
    { public declarations }
    Form: pointer;
    procedure GetStatus;
  end;

var
  StatusForm: TStatusForm;

implementation

Uses DeviceF, MainF, GenConst, DetConst, SerConF;

{$R *.lfm}

{ TStatusForm }

procedure TStatusForm.btPollClick(Sender: TObject);
begin
  GetStatus;
end;

procedure TStatusForm.btLastPollClick(Sender: TObject);
begin
  lSPSB.Caption:=  BinStr(pSerPollSb, 8);
  lSESB.Caption:=  BinStr(pStdEventSB, 8);
  lLIASB.Caption:= BinStr(pLiaSB, 8);
  lESB.Caption:=   BinStr(pErrSB, 8);
  lDDSB.Caption:=  BinStr(pDDSB, 8);
end;

procedure TStatusForm.btApplyClick(Sender: TObject);
begin
  val('%' + eSPSB.Text,  eSerPollSB);
  val('%' + eSESB.Text,  eStdEventSB);
  val('%' + eLIAS.Text, eLiaSB);
  val('%' + eESB.Text,   eErrSB);
  val('%' + eDDS.Text,   eDDSB);

  with TSerConnectForm(Form) do
  begin
    EnterCriticalSection(CommCS);
      AddCommand(cSerialPollEnable, false, eSerPollSB);
      AddCommand(cStdEventEnable, false, eStdEventSB);

      if DeviceKind = dDetector  then
      begin
        AddCommand(dLIAEnable, false, eLiaSB);
        AddCommand(dErrorEnable, false, eErrSB);
      end
      else
      begin
        AddCommand(gDDSEnable, false, eDDSB);
      end;

      PassCommands;
    LeaveCriticalSection(CommCS);
  end;
end;

procedure TStatusForm.FormShow(Sender: TObject);
begin
  Label5.Caption:= TSerConnectForm(Form).CurrentDevice^.Model;
  if Label5.Caption = '' then Label5.Caption:= 'Устройство не опознано';

  lSPSB.Caption:=  '?';
  lSESB.Caption:=  '?';
  lLIASB.Caption:= '?';
  lESB.Caption:=   '?';
  lDDSB.Caption:=  '?';
  with TSerConnectForm(Form) do
  begin
    if DeviceKind = dDetector then
    begin
      Label10.Show;
      eLIAS.Show;
      lLIASb.Show;
      Label4.Show;
      eESB.Show;
      lESB.Show;

      label9.Hide;
      eDDS.Hide;
      lDDSB.Hide;
    end
    else
    if DeviceKind = dGenerator then
    begin
      Label10.Hide;
      eLIAS.Hide;
      lLIASb.Hide;
      Label4.Hide;
      eESB.Hide;
      lESB.Hide;

      label9.Show;
      eDDS.Show;
      lDDSB.Show;
    end;

    if DeviceIndex = iDefaultDevice then
    begin
      Label10.Hide;
      eLIAS.Hide;
      lLIASb.Hide;
      Label4.Hide;
      eESB.Hide;
      lESB.Hide;

      Label8.Hide;
      eDDS.Hide;
      lDDSB.Hide;
    end;
  end;
end;

procedure TStatusForm.GetStatus;
var
  s{, cs}: string;
begin
  with TSerConnectForm(Form) do
  begin
    pSerPollSB:= SerPollSB;
    pStdEventSB:= StdEventSB;
    pLiaSB:= LiaSB;
    pErrSB:= ErrSB;
    pDDSB:= DDSB;

    SerPollSB:= 0;
    StdEventSB:= 0;
    LiaSB:= 0;
    ErrSB:= 0;
    DDSB:=  0;

    eSerPollSB:= 0;
    eStdEventSB:= 0;
    eLiaSB:= 0;
    eErrSB:= 0;
    eDDSB:=  0;

    EnterCriticalSection(CommCS);
      AddCommand(cSerialPollEnable, true);
      PassCommands;
      s:= Recvstring;
      val(s, eSerPollSB);

      AddCommand(cStdEventEnable, true);
      PassCommands;
      s:= Recvstring;
      val(s, eStdEventSB);


      AddCommand(cSerialPoll, true);
      PassCommands;
      s:= Recvstring;
      val(s, SerPollSB);

      AddCommand(cStdEvent, true);
      PassCommands;
      s:= Recvstring;
      val(s, StdEventSB);

      if DeviceKind = dDetector then
      begin
        AddCommand(dLIAEnable, true);
        PassCommands;
        s:= Recvstring;
        val(s, eLiaSB);

        AddCommand(dErrorEnable, true);
        PassCommands;
        s:= Recvstring;
        val(s, eErrSB);


        AddCommand(dLIA, true);
        PassCommands;
        s:= Recvstring;
        val(s, LiaSB);

        AddCommand(dError, true);
        PassCommands;
        s:= Recvstring;
        val(s, ErrSB);
      end
      else
      if DeviceKind = dGenerator then
      begin
        AddCommand(gDDSEnable, true);
        PassCommands;
        s:= Recvstring;
        val(s, eDDSB);

        AddCommand(gDDS, true);
        PassCommands;
        s:= Recvstring;
        val(s, DDSB);
      end;
    LeaveCriticalSection(CommCS);

    {cs:= SupportedDevices[DeviceIndex].CommSeparator;

    val(copy(s, 1, pos(cs, s) - 1), i);
    SerPollSB:= i;
    delete(s, 1, pos(cs, s));

    val(copy(s, 1, pos(cs, s) - 1), i);
    StdEventSB:= i;
    delete(s, 1, pos(cs, s));

    val(copy(s, 1, pos(cs, s) - 1), i);
    LiaSB:= i;
    delete(s, 1, pos(cs, s));

    val(copy(s, 1, pos(cs, s) - 1), i);
    ErrSB:= i;
    delete(s, 1, pos(cs, s));}

    lSPSB.Caption:=  BinStr(SerPollSB, 8);
    lSESB.Caption:=  BinStr(StdEventSB, 8);
    lLIASB.Caption:= BinStr(LiaSB, 8);
    lESB.Caption:=   BinStr(ErrSB, 8);
    lDDSB.Caption:=   BinStr(DDSB, 8);

    eSPSB.Text:=  BinStr(eSerPollSB, 8);
    eSESB.Text:=  BinStr(eStdEventSB, 8);
    eLIAS.Text:= BinStr(eLiaSB, 8);
    eESB.Text:=   BinStr(eErrSB, 8);
    eDDS.Text:=   BinStr(eDDSB, 8);
  end;
end;

end.

