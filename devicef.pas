unit DeviceF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  Buttons, StdCtrls, ComCtrls; { TODO 2 -cImprovement : check out valedit }

type
  eHeaderRow = (
    hModel, hManufacturer, hInterface, hParSeparator,
    hCommSeparator, hTerminator, hBaudRate, hDataBits, hStopBits, hParity,
    hHandShake, hInitString, hTimeOut, hMinDelay
                );

  { TDeviceForm }

  TDeviceForm = class(TForm)
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
    Label1: TLabel;
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
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pcDeviceChange(Sender: TObject);
    procedure sgGenCommandsSelection(Sender: TObject; aCol, aRow: Integer);
  private
    { private declarations }
    CurrColumn: integer;  //stringgrid.row
    CurrGenFileName, CurrDetFileName: string;
    sg: tStringGrid;
  public
    { public declarations }
  end;

const
  DefaultGen = 'DefaultGenCommands.xml';
  DefaultDet = 'DefaultDetCommands.xml';

var
  DeviceForm: TDeviceForm;

implementation

{$R *.lfm}

uses
  LCLType, MainF, OptionF;

const
  StopBitsRow = 8;
  ParityRow = 9;
  HandShakeRow = 10;
  TerminatorRow = 5;

{ TDeviceForm }

procedure TDeviceForm.btAddDeviceClick(Sender: TObject);
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

procedure TDeviceForm.btApplyClick(Sender: TObject);
begin
  if CurrColumn < 0 then exit;
  with sg do
  begin
    Cells[CurrColumn, StopBitsRow]:= cbStopBits.Text;
    Cells[CurrColumn, ParityRow]:= cbParity.Text;
    Cells[CurrColumn, HandShakeRow]:= cbHandShake.Text;
    Cells[CurrColumn, TerminatorRow]:= cbTerminator.Text;
  end;
end;

procedure TDeviceForm.btDeleteDeviceClick(Sender: TObject);
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

procedure TDeviceForm.btDoneClick(Sender: TObject);
begin
  OptionForm.GetOptions;
  Close;
end;

procedure TDeviceForm.btLoadFromFileClick(Sender: TObject);
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
                { TODO 3 -cBug : how to clear }
                //sg.Clean;
               LoadFromFile(CurrGenFileName);
             end;
           end;
        1: begin
             CurrDetFileName:= UTF8toANSI(FileName);
             if FileExists(CurrDetFileName) then
             with sg do
             begin
                { TODO 3 -cBug : how to clear }
                //sg.Clean;
               LoadFromFile(CurrDetFileName);
             end;
           end;
      end;
    end;
  end;
end;

procedure TDeviceForm.btSaveToFileClick(Sender: TObject);
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

procedure TDeviceForm.btSetDefaultFileClick(Sender: TObject);
begin
  case pcDevice.TabIndex of
    0: if CurrGenFileName <> '' then
         Config.DefaultGens:= CurrGenFileName;
    1: if CurrDetFileName <> '' then
         Config.DefaultDets:= CurrDetFileName;
  end;
end;

procedure TDeviceForm.FormCreate(Sender: TObject);
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

procedure TDeviceForm.FormShow(Sender: TObject);
begin
  CurrGenFileName:= '';
  CurrDetFileName:= '';
  CurrColumn:= -1;
  pcDeviceChange(Self);
end;

procedure TDeviceForm.pcDeviceChange(Sender: TObject);
begin
  CurrColumn:= -1;
  case pcDevice.TabIndex of
      0: sg:= sgGenCommands;
      1: sg:= sgDetCommands;
  end;
end;

procedure TDeviceForm.sgGenCommandsSelection(Sender: TObject; aCol,
  aRow: Integer);
begin
  CurrColumn:= aCol;
end;

end.

