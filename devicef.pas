unit DeviceF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ValEdit, Buttons, StdCtrls, ComCtrls, Spin;

type

  { TDeviceForm }

  TDeviceForm = class(TForm)
    btAddDevice: TButton;
    btDeleteDevice: TButton;
    btApply: TButton;
    btSaveToFile: TButton;
    btLoadFromFile: TButton;
    btSetDefaultFile: TButton;
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
    procedure btLoadFromFileClick(Sender: TObject);
    procedure btSaveToFileClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pcDeviceChange(Sender: TObject);
    procedure sgGenCommandsSelection(Sender: TObject; aCol, aRow: Integer);
  private
    { private declarations }
    CurrColumn: integer;  //stringgrid.row
    sg: tStringGrid;
  public
    { public declarations }
  end;

var
  DeviceForm: TDeviceForm;

implementation

{$R *.lfm}

uses
  LCLType;

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

procedure TDeviceForm.btLoadFromFileClick(Sender: TObject);
var
  s: string;
begin
  with OpenDialog do
  begin
    case pcDevice.TabIndex of
        0: FileName:= 'DefaultGenCommands';
        1: FileName:= 'DefaultDetCommands';
    end;
    Title:= 'Загрузить файл устройств';
    Filter:= 'Файлы устройств|*.xml|Все файлы|*.*';
    if Execute then
    begin
      s:= UTF8toANSI(FileName); //  проверить на доп символы
      if s <> '' then sg.LoadFromFile(s);
    end;
  end;
end;

procedure TDeviceForm.btSaveToFileClick(Sender: TObject);
var
  s: string;
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
      s:= UTF8toANSI(FileName);
      if s <> '' then sg.SaveToFile(s);
    end;
  end;
end;

procedure TDeviceForm.FormShow(Sender: TObject);
begin
  CurrColumn:= -1;
  pcDeviceChange(Self);
end;

procedure TDeviceForm.pcDeviceChange(Sender: TObject);
begin
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

