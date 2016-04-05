unit DeviceF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  Buttons, StdCtrls, ComCtrls, ExtCtrls;

type
  eHeaderRow = (
    hModel, hManufacturer, hInterface, hBaudRate, hDataBits, hStopBits, hParity,
    hHandShake, hIPAdress, hPort, hInitString, hTimeOut, hMinDelay,
    hParSeparator, hCommSeparator, hTerminator,
    hTimeConstOptions, hSensitivityOptions, hTransferParams, hFirstIndex,
    hIndices, hMaxSimultPars, hCH1Options, hCH2Options, hRatio1Options,
    hRatio2Options,
    hResistanceOptions = integer(hTimeConstOptions), hFunctionOptions,
    hSweepTypeOptions, hSweepDirectionOptions, hModulationOptions
                );

  eCommonCommand = (
    cReset, cReboot, cIdentify, cRecall, cSave, cTest, cCalibrate, cClearStatus,
    cSerialPoll, cSerialPollEnable, cStdEvent, cStdEventEnable, cPowerClear,
    cTrigger
                    );

  eGenCommand = (
    gDDS = integer(cTrigger) + 1, gDDSEnable, gResistance, gFunction,
    gAmplitude, gOffset, gFrequency, gSweepSource, gSweepEnable,
    gSweepRate, gSweepStartFrequency, gSweepStopFrequency, gSweepType,
    gSweepDirection, gModulationWaveform
                 );

  eDetCommand = (
    dError = integer(cTrigger) + 1, dErrorEnable, dLIA, dLIAEnable,
    dRange, dSensitivity, dTimeConstant, dCloseReserve, dWideReserve,
    dAutoRange, dAutoSensitivity, dAutoCloseReserve, dAutoWideReserve,
    dAutoPhase, dAutoOffset, dReferenceSource,  dSampleRate, dStartStorage,
    dPauseStorage, dResetStorage, dStoredPoints, dReadPointsNative,
    dStorageMode, dReadSimultaneous, dDisplaySelect
                 );

  {rCommand = record
    case integer of
      0..integer(cTrigger): (Value: eCommonCommand);
      integer(gDDS)..integer(gSweepDirection): (Value: eGenCommand);
      integer(dError)..integer(dDisplaySelect): (Value: eDetCommand);
    end;
  end;}

  {type
  TSR844Command = (
    PRST = longint(TRG) + 1, LOCL, OVRM,
    FMOD, HARM, FREQ, FRAQ, FRIQ, PHAS, APHS, REFZ, WRSV, AWRS, INPZ,
    SENS, AGAN, CRSV, ACRS, OFLT, OFSL, SETL,
    DDEF, DRAT, FPOP, DOFF, AOFF, DEXP,
    AUXI, AUXO,
    OUTX, KCLK, ALRM, SSET, RSET, KNOB, KEYP,
    SSTR, SFIN, SSTP, SMOD, RSTO, RRDY, RCLR, RMOD,
    SRAT, SEND, TSTR, STRT, PAUS, REST,
    OUTP, OUTR, SNAP, SPTS, TRCA, TRCB, TRCL, FAST, STRD,
    ERRS, ERRE, LIAS, LIAE
                     );

  TSR830Command = (
    RST, IDN, LOCL = 3, OVRM,
    FMOD, HARM, FREQ, PHAS = 10, APHS,
    SENS = 16, AGAN, OFLT = 20, OFSL,
    DDEF = 23, FPOP = 25, AOFF = 27,
    OAUX = 29, AUXV,
    OUTX, KCLK, ALRM, SSET, RSET,
    RMOD = 45,
    SRAT, SEND, TRIG, TSTR, STRT, PAUS, REST,
    OUTP, OUTR, SNAP, SPTS, TRCA, TRCB, TRCL, FAST, STRD,
    CLS, STB, SRE, ESR, ESE, PSC, ERRS, ERRE, LIAS, LIAE,
    RSLP = longint(LIAE) + 1, SLVL, ISRC, IGND, ICPL, ILIN, SYNC, OEXP, ARSV
                                              );                               }

   {tDS345Command = (
    AECL = longint(TRG) + 1, AMPL, ATTL, FREQ, FSMP, FUNC, INVT, OFFS, PCLR, PHSE,
    BCNT, DPTH, FDEV, MDWF, MENA, MKSP, MRKF, MTYP, PDEV, RATE, SPAN, SPCF, SPFR, SPMK, STFR, TRAT, TSRC,
    AMRT, AMOD, LDWF,
    DENA, STAT
              );

  tDS335Command = (
    KEYS = longint(STAT) + 1,  SYNC, TERM,
    FSEN, SDIR, SWEN
                    );
const

  DS345Command: array [longint(RST)..longint(STAT)] of ansistring = (
    '*RST', '*IDN', '*RCL', '*SAV', '*TST', '*CAL', '*CLS', '*STB', '*SRE', '*ESR', '*ESE', '*PSC' , '*TRG',
    'AECL', 'AMPL', 'ATTL', 'FREQ', 'FSMP', 'FUNC', 'INVT', 'OFFS', 'PCLR', 'PHSE',
    'BCNT', 'DPTH', 'FDEV', 'MDWF', 'MENA', 'MKSP', 'MRKF', 'MTYP', 'PDEV', 'RATE', 'SPAN', 'SPCF', 'SPFR', 'SPMK', 'STFR', 'TRAT', 'TSRC',
    'AMRT', 'AMOD', 'LDWF',
    'DENA', 'STAT'
    );

  DS335Command: array [longint(RST)..longint(SWEN)] of ansistring = (
    '*RST', '*IDN', '*RCL', '*SAV', '*TST', '', '*CLS', '*STB', '*SRE', '*ESR', '*ESE', '*PSC' , '*TRG',
    'AECL', 'AMPL', 'ATTL', 'FREQ', '', 'FUNC', 'INVT', 'OFFS', '', '',
    '', '', '', '', '', '', '', 'STYP', '', '', '', '', 'SPFR', '', 'STFR', 'SRAT', 'TSRC',
    '', '', '',
    'DENA', 'STAT',
    'KEYS',  'SYNC', 'TERM',
    'FSEN', 'SDIR', 'SWEN'
     );   }

  { tDeviceForm }

  tDeviceForm = class(TForm)
    btAddDevice: TButton;
    btDeleteDevice: TButton;
    btApply: TButton;
    btSaveToFile: TButton;
    btLoadFromFile: TButton;
    btSetDefaultFile: TButton;
    btDone: TButton;
    cbHandshake: TComboBox;
    cbParity: TComboBox;
    cbStopBits: TComboBox;
    cbTerminator: TComboBox;
    cbInterface: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    OpenDialog: TOpenDialog;
    pcDevice: TPageControl;
    SaveDialog: TSaveDialog;
    sgGenCommands: TStringGrid;
    sgDetCommands: TStringGrid;
    tsGenerators: TTabSheet;
    tsDetectors: TTabSheet;
    procedure btAddDeviceClick(Sender: TObject);
    procedure btApplyClick(Sender: TObject);
    procedure btDeleteDeviceClick(Sender: TObject);
    procedure btDoneClick(Sender: TObject);
    procedure btLoadFromFileClick(Sender: TObject);
    procedure btSaveToFileClick(Sender: TObject);
    procedure btSetDefaultFileClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pcDeviceChange(Sender: TObject);
    procedure sgDetCommandsClick(Sender: TObject);
    procedure sgDetCommandsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sgGenCommandsClick(Sender: TObject);
    procedure sgGenCommandsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sgGenCommandsSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure sgDetCommandsSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure sgGenCommandsSelection(Sender: TObject; aCol, aRow: Integer);
    procedure EditOptionString(c, r: integer);
    function CheckConformance: integer;
  private
    { private declarations }
    CurrColumn: integer;  //stringgrid.row
    CurrGenFileName, CurrDetFileName: string;
    sg: tStringGrid;
  public
    { public declarations }
  end;

const
  SGHeaderLength = 28; //кол-во строк в tStringGrid, не относящ. к командам
  DefaultGen = 'DefaultGenCommands.xml';
  DefaultDet = 'DefaultDetCommands.xml';

var
  DeviceForm: TDeviceForm;

implementation

{$R *.lfm}

uses
  LCLType, SynaIP, MainF, ReadingsF, OptionF, serconf, MemoF;

{ TDeviceForm }

procedure tDeviceForm.btAddDeviceClick(Sender: TObject);
var
  Model: string;
begin
  Model:= InputBox('Новое устройство','Введите название модели','');
  if Model <> '' then
  with sg do
  begin
    Columns.Add;
    Cols[ColCount - 1].Add(Model);
  end;
end;

procedure tDeviceForm.btApplyClick(Sender: TObject);
begin
  if CurrColumn < 0 then exit;
  with sg do
  begin
    Cells[CurrColumn, integer(hInterface) ]:= cbInterface.Text;
    Cells[CurrColumn, integer(hStopBits)  ]:= cbStopBits.Text;
    Cells[CurrColumn, integer(hParity)    ]:= cbParity.Text;
    Cells[CurrColumn, integer(hHandShake) ]:= cbHandShake.Text;
    Cells[CurrColumn, integer(hTerminator)]:= cbTerminator.Text;
  end;
end;

procedure tDeviceForm.btDeleteDeviceClick(Sender: TObject);
begin
  if CurrColumn < 0 then exit;
  if Application.MessageBox(
                            pchar('Будет удалено устройство ' +
                            sg.Cells[CurrColumn, 0] +
                            '. Продолжить?'),
                            'Подтверждение', MB_YESNO
                            ) = IDYES then
    sg.DeleteCol(CurrColumn);
end;

procedure tDeviceForm.btDoneClick(Sender: TObject);

begin
  Close; //onclosequery checks
end;

procedure tDeviceForm.btLoadFromFileClick(Sender: TObject);
begin
  with OpenDialog do
  begin
    case pcDevice.TabIndex of
      0: FileName:= DefaultGen;
      1: FileName:= DefaultDet;
    end;
    Title:= 'Загрузить файл устройств';
    Filter:= 'Файлы устройств|*.xml|Все файлы|*.*';
    if Execute then
    begin
      case pcDevice.TabIndex of
        0: begin
             CurrGenFileName:= UTF8toANSI(FileName); //  проверить на доп символы
             if FileExists(CurrGenFileName) then
             with sg do
             begin
               sg.Clean;
               LoadFromFile(CurrGenFileName);
             end;
           end;
        1: begin
             CurrDetFileName:= UTF8toANSI(FileName);
             if FileExists(CurrDetFileName) then
             with sg do
             begin

               sg.Clean;
               LoadFromFile(CurrDetFileName);
             end;
           end;
      end;
    end;
  end;
end;

procedure tDeviceForm.btSaveToFileClick(Sender: TObject);
begin
  with SaveDialog do
  begin
    case pcDevice.TabIndex of
      0: FileName:= 'DefaultGenCommands';
      1: FileName:= 'DefaultDetCommands';
    end;

    Title:= 'Сохранить файл устройств как';
    Filter:= 'Файлы устройств|*.xml|Все файлы|*.*';
    if Execute then
    begin
      case pcDevice.TabIndex of
        0: begin
             CurrGenFileName:= UTF8toANSI(FileName); //  проверить на доп символы
             if CurrGenFileName <> '' then sg.SaveToFile(CurrGenFileName);
           end;
        1: begin
             CurrDetFileName:= UTF8toANSI(FileName);
             if CurrDetFileName <> '' then sg.SaveToFile(CurrDetFileName);
           end;
      end;
    end;
  end;
end;

procedure tDeviceForm.btSetDefaultFileClick(Sender: TObject);
begin
  case pcDevice.TabIndex of
    0: if CurrGenFileName <> '' then
         Config.DefaultGens:= CurrGenFileName;
    1: if CurrDetFileName <> '' then
         Config.DefaultDets:= CurrDetFileName;
  end;
end;

procedure tDeviceForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  o: integer;
begin
  o:= CheckConformance;

  if o = 0 then
  begin
    MainForm.GetSupportedDevices(dGenerator);
    ReadingsForm.GetSupportedDevices(dDetector);
    OptionForm.ReloadDeviceList;

    CanClose:= true;
  end
  else
    CanClose:= false;
end;

procedure tDeviceForm.FormCreate(Sender: TObject);
begin
  if FileExists(Config.DefaultGens) then
    sgGenCommands.LoadFromFile(Config.DefaultGens)
  else
  if Config.DefaultGens <> DefaultGen then
    ShowMessage('Не найден файл ' + Config.DefaultGens);

  if FileExists(Config.DefaultDets) then
    sgDetCommands.LoadFromFile(Config.DefaultDets)
  else
  if Config.DefaultDets <> DefaultDet then
    ShowMessage('Не найден файл ' + Config.DefaultDets);
end;

procedure tDeviceForm.FormShow(Sender: TObject);
begin
  CurrGenFileName:= '';
  CurrDetFileName:= '';
  CurrColumn:= -1;
  pcDeviceChange(Self);
end;

procedure tDeviceForm.pcDeviceChange(Sender: TObject);
begin
  CurrColumn:= -1;
  case pcDevice.TabIndex of
    0: sg:= sgGenCommands;
    1: sg:= sgDetCommands;
  end;
end;

procedure tDeviceForm.sgDetCommandsClick(Sender: TObject);
begin
  with sg do
    case Row of
     integer(hTimeConstOptions),
     integer(hSensitivityOptions),
     integer(hTransferParams),
     integer(hCH1Options),
     integer(hCH2Options),
     integer(hRatio1Options),
     integer(hRatio2Options):
        begin
          //ClearSelections;
          EditOptionString(Col, Row);
          //sgDetCommands.led:= false; //CRUTCH to avoid selection troublems
          //tCrutch.Enabled:= true;
        end;
    end;
end;

procedure tDeviceForm.sgDetCommandsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  key:= 0;
  sgDetCommandsClick(Self);
end;

procedure tDeviceForm.sgGenCommandsClick(Sender: TObject);
begin
  with sg do
    case Row of
      integer(hResistanceOptions),
      integer(hFunctionOptions),
      integer(hSweepTypeOptions),
      integer(hsWeepDirectionOptions),
      integer(hModulationOptions):
        begin
          //ClearSelections;
          EditOptionString(Col, Row);
          //sgDetCommands.led:= false; //CRUTCH to avoid selection troublems
          //tCrutch.Enabled:= true;
        end;
    end;
end;

procedure tDeviceForm.sgGenCommandsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  key:= 0;
  sgGenCommandsClick(Self);
end;

procedure tDeviceForm.sgGenCommandsSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  with sgGenCommands do
  begin
    AutoEdit:= true;
    with Columns.Items[aCol - 1] do
    case aRow of
      integer(hInterface):
        begin
          ButtonStyle:= cbsPickList;
          PickList:= cbInterface.Items;
        end;
      integer(hStopBits):
        begin
          ButtonStyle:= cbsPickList;
          PickList:= cbStopBits.Items;
        end;
      integer(hParity):
        begin
          ButtonStyle:= cbsPickList;
          PickList:= cbParity.Items;
        end;
      integer(hHandshake):
        begin
          ButtonStyle:= cbsPickList;
          PickList:= cbHandshake.Items;
        end;
      integer(hTerminator):
        begin
          ButtonStyle:= cbsPickList;
          PickList:= cbTerminator.Items;
        end;
      integer(hResistanceOptions),
      integer(hFunctionOptions),
      integer(hSweepTypeOptions),
      integer(hSweepDirectionOptions),
      integer(hModulationOptions):
        AutoEdit:= false;
      else
        begin
           ButtonStyle:= cbsAuto;
           PickList:= nil;
        end;
    end;
  end;
end;

procedure tDeviceForm.sgDetCommandsSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  with sgDetCommands do
  begin
    AutoEdit:= true;
    with Columns.Items[aCol - 1] do
    case aRow of
      integer(hInterface):
        begin
          ButtonStyle:= cbsPickList;
          PickList:= cbInterface.Items;
        end;
      integer(hStopBits):
        begin
          ButtonStyle:= cbsPickList;
          PickList:= cbStopBits.Items;
        end;
      integer(hParity):
        begin
          ButtonStyle:= cbsPickList;
          PickList:= cbParity.Items;
        end;
      integer(hHandshake):
        begin
          ButtonStyle:= cbsPickList;
          PickList:= cbHandshake.Items;
        end;
      integer(hTerminator):
        begin
          ButtonStyle:= cbsPickList;
          PickList:= cbTerminator.Items;
        end;
      integer(hTimeConstOptions),
      integer(hSensitivityOptions),
      integer(hTransferParams),
      integer(hCH1Options),
      integer(hCH2Options),
      integer(hRatio1Options),
      integer(hRatio2Options):
        AutoEdit:= false;
      else
        begin
           ButtonStyle:= cbsAuto;
           PickList:= nil;
        end;
    end;
  end;
end;

procedure tDeviceForm.sgGenCommandsSelection(Sender: TObject; aCol,
  aRow: Integer);
begin
  CurrColumn:= aCol;
end;

procedure tDeviceForm.EditOptionString(c, r: integer);
begin
  with sg, MemoForm  do
  begin

    //Col:= c;

    //Row:= r;

    mComment.Text := Cells[Col, Row];

    ShowModal;
    if (mComment.Text <> '') or (Cells[Col, Row] <> '') then
    begin
     // mComment.Lines.;
      Cells[Col, Row]:= mComment.Text;
    end;
  end;
end;

function tDeviceForm.CheckConformance: integer;
var
  s: string;
  i, o: integer;
{Exitcodes:
  0 - Success
  in gen
  -1 - Interface
  -2 - Baud rate
  -3 - Databits
  -4 - Stop bits
  -5 - Parity
  -6 - Handshake
  -7 - ip
  -8 - port
  -9 - terminator
  -10 - timeout
  in det
  -11 - Interface
  -12 - Baud rate
  -13 - Databits
  -14 - Stop bits
  -15 - Parity
  -16 - Handshake
  -17 - ip
  -18 - port
  -19 - terminator
  -20 - timeout
  -30 temp usb wip
  }
begin
  Result:= 0;
  with sgGenCommands do
    for i:= 1 to ColCount - 1 do
    begin
      s:= Cells[i, integer(hInterface)];
      o:= DeviceForm.cbInterface.Items.IndexOf(s);
      case o of
        0:
          begin
            o:= valf(Cells[i, integer(hBaudRate)]);
            if o < 50 then
            begin
              pcDevice.TabIndex:= 0;
              Row:= integer(hBaudRate);
              Col:= i;
              ShowMessage('Ошибка в поле "Бод-частота"');
              exit(-2);
            end;

            o:= valf(Cells[i, integer(hDataBits)]);
            if o < 1 then
            begin
              pcDevice.TabIndex:= 0;
              Row:= integer(hDataBits);
              Col:= i;
              ShowMessage('Ошибка в поле "Биты данных"');
              exit(-3);
            end;

            s:= Cells[i, integer(hStopBits)];
            o:= DeviceForm.cbStopBits.Items.IndexOf(s);
            if o < 0 then
            begin
              pcDevice.TabIndex:= 0;
              Row:= integer(hStopBits);
              Col:= i;
              ShowMessage('Ошибка в поле "Стоп-биты"');
              exit(-4);
            end;

            s:= Cells[i, integer(hParity)];
            o:= DeviceForm.cbParity.Items.IndexOf(s);
            if o < 0 then
            begin
              pcDevice.TabIndex:= 0;
              Row:= integer(hParity);
              Col:= i;
              ShowMessage('Ошибка в поле "Парность"');
              exit(-5);
            end;

            s:= Cells[i, integer(hHandshake)];
            o:= DeviceForm.cbHandshake.Items.IndexOf(s);
            if o < 0 then
            begin
              pcDevice.TabIndex:= 0;
              Row:= integer(hHandShake);
              Col:= i;
              ShowMessage('Ошибка в поле "Handshake"');
              exit(-6);
            end;
          end;
        1:
          begin
            s:= Cells[i, integer(hIPAdress)];
            if not IsIP(s) then
            begin
              pcDevice.TabIndex:= 0;
              Row:= integer(hIPAdress);
              Col:= i;
              ShowMessage('Ошибка в поле "IP-адрес"');
              exit(-7);
            end;

            o:= valf(Cells[i, integer(hPort)]);
            if o <= 0 then
            begin
              pcDevice.TabIndex:= 0;
              Row:= integer(hPort);
              Col:= i;
              ShowMessage('Ошибка в поле "Порт"');
              exit(-8);
            end;
            { TODO 3 -cImprovement : check if 0 is valid port }
          end;
        2:
          begin
            pcDevice.TabIndex:= 0;
            ShowMessage('USB не поддерживается');
            exit(-30);
          end;
        else
          begin
            pcDevice.TabIndex:= 0;
            Row:= integer(hInterface);
            Col:= i;
            ShowMessage('Ошибка в поле "Интерфейс"');
            exit(-1);
          end;
      end;

      s:= Cells[i, integer(hTerminator)];
      o:= DeviceForm.cbTerminator.Items.IndexOf(s);
      if o < 0 then
      begin
        pcDevice.TabIndex:= 0;
        Row:= integer(hTerminator);
        Col:= i;
        ShowMessage('Ошибка в поле "Терминатор"');
        exit(-9);
      end;

      o:= valf(Cells[i, integer(hTimeOut)]);
      if o <= 0 then
      begin
        pcDevice.TabIndex:= 0;
        Row:= integer(hTimeOut);
        Col:= i;
        ShowMessage('Ошибка в поле "Таймаут"');
        exit(-10);
      end;
    end;

  with sgDetCommands do
    for i:= 1 to ColCount - 1 do
    begin
      s:= Cells[i, integer(hInterface)];
      o:= DeviceForm.cbInterface.Items.IndexOf(s);
      case o of
        0:
          begin
            o:= valf(Cells[i, integer(hBaudRate)]);
            if o < 50 then
            begin
              pcDevice.TabIndex:= 1;
              Row:= integer(hBaudRate);
              Col:= i;
              ShowMessage('Ошибка в поле "Бод-частота"');
              exit(-12);
            end;

            o:= valf(Cells[i, integer(hDataBits)]);
            if o < 1 then
            begin
              pcDevice.TabIndex:= 1;
              Row:= integer(hDataBits);
              Col:= i;
              ShowMessage('Ошибка в поле "Биты данных"');
              exit(-13);
            end;

            s:= Cells[i, integer(hStopBits)];
            o:= DeviceForm.cbStopBits.Items.IndexOf(s);
            if o < 0 then
            begin
              pcDevice.TabIndex:= 1;
              Row:= integer(hStopBits);
              Col:= i;
              ShowMessage('Ошибка в поле "Стоп-биты"');
              exit(-14);
            end;

            s:= Cells[i, integer(hParity)];
            o:= DeviceForm.cbParity.Items.IndexOf(s);
            if o < 0 then
            begin
              pcDevice.TabIndex:= 1;
              Row:= integer(hParity);
              Col:= i;
              ShowMessage('Ошибка в поле "Парность"');
              exit(-15);
            end;

            s:= Cells[i, integer(hHandshake)];
            o:= DeviceForm.cbHandshake.Items.IndexOf(s);
            if o < 0 then
            begin
              pcDevice.TabIndex:= 1;
              Row:= integer(hHandShake);
              Col:= i;
              ShowMessage('Ошибка в поле "Handshake"');
              exit(-16);
            end;
          end;
        1:
          begin
            s:= Cells[i, integer(hIPAdress)];
            if not IsIP(s) then
            begin
              pcDevice.TabIndex:= 1;
              Row:= integer(hIPAdress);
              Col:= i;
              ShowMessage('Ошибка в поле "IP-адрес"');
              exit(-17);
            end;

            o:= valf(Cells[i, integer(hPort)]);
            if o <= 0 then
            begin
              pcDevice.TabIndex:= 1;
              Row:= integer(hPort);
              Col:= i;
              ShowMessage('Ошибка в поле "Порт"');
              exit(-18);
            end;
            { TODO 3 -cImprovement : check if 0 is valid port }
          end;
        2:
          begin
            pcDevice.TabIndex:= 1;
            ShowMessage('USB не поддерживается');
            exit(-30);
          end;
        else
          begin
            pcDevice.TabIndex:= 1;
            Row:= integer(hInterface);
            Col:= i;
            ShowMessage('Ошибка в поле "Интерфейс"');
            exit(-11);
          end;
      end;

      s:= Cells[i, integer(hTerminator)];
      o:= DeviceForm.cbTerminator.Items.IndexOf(s);
      if o < 0 then
      begin
        pcDevice.TabIndex:= 1;
        Row:= integer(hTerminator);
        Col:= i;
        ShowMessage('Ошибка в поле "Терминатор"');
        exit(-19);
      end;

      o:= valf(Cells[i, integer(hTimeOut)]);
      if o <= 0 then
      begin
        pcDevice.TabIndex:= 1;
        Row:= integer(hTimeOut);
        Col:= i;
        ShowMessage('Ошибка в поле "Таймаут"');
        exit(-20);
      end;
    end;
end;

end.
