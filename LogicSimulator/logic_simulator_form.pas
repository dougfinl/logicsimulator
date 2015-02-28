unit logic_simulator_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, ExtCtrls, StdCtrls, LCLType;

type

  { TfrmLogicSim }

  TfrmLogicSim = class(TForm)
    btnNew: TToolButton;
    btnOpen: TToolButton;
    btnRun: TToolButton;
    btnSave: TToolButton;
    btnStop: TToolButton;
    ilMenuBarIcons: TImageList;
    ilComponents: TImageList;
    lblComponents: TLabel;
    diaOpen: TOpenDialog;
    menuSeparator5: TToolButton;
    pnlSchema: TPanel;
    diaSave: TSaveDialog;
    sbStatus: TStatusBar;
    scrollComponents: TScrollBox;
    tbMenuBar: TToolBar;
    tbComponents: TToolBar;
    menuSeparator1: TToolButton;
    componentSeparator4: TToolButton;
    componentSeparator5: TToolButton;
    btnMouse: TToolButton;
    btnWire: TToolButton;
    menuSeparator3: TToolButton;
    menuSeparator4: TToolButton;
    btnGateXNOR: TToolButton;
    btnGateXOR: TToolButton;
    btnGateNOR: TToolButton;
    componentSeparator6: TToolButton;
    componentSeparator7: TToolButton;
    componentSeparator8: TToolButton;
    btnGateNOT: TToolButton;
    menuSeparator2: TToolButton;
    btnGateAND: TToolButton;
    btnGateOR: TToolButton;
    componentSeparator2: TToolButton;
    componentSeparator1: TToolButton;
    componentSeparator3: TToolButton;
    btnGateNAND: TToolButton;
    btnOutputIndicator: TToolButton;
    componentSeparator9: TToolButton;
    btnInputSwitch: TToolButton;
    componentSeparator10: TToolButton;
    btnInputButton: TToolButton;
    btnSnapToGrid: TToolButton;
    SimulationTimer: TTimer;
    procedure btnGateANDClick(Sender: TObject);
    procedure btnGateNANDClick(Sender: TObject);
    procedure btnGateNORClick(Sender: TObject);
    procedure btnGateNOTClick(Sender: TObject);
    procedure btnGateORClick(Sender: TObject);
    procedure btnGateXNORClick(Sender: TObject);
    procedure btnGateXORClick(Sender: TObject);
    procedure btnInputButtonClick(Sender: TObject);
    procedure btnInputSwitchClick(Sender: TObject);
    procedure btnMouseClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnOutputIndicatorClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSnapToGridClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnWireClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; {%H-}Shift: TShiftState);
    procedure pnlSchemaMouseDown({%H-}Sender: TObject; {%H-}Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure pnlSchemaMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X,
    {%H-}Y: integer);
    procedure SimulationTimerTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmLogicSim: TfrmLogicSim;


implementation

uses
  logic_simulator_common, logic_simulator_globals, logic_simulator_simulation;

{$R *.lfm}

{ TfrmLogicSim }

// Form events.
procedure TfrmLogicSim.FormCreate(Sender: TObject);
begin
  InitialiseProgram;
  btnStop.Enabled := False;
  SchematicChanged := False;
  SnapToGrid := False;
  ApplicationState := EDITING;
end;

procedure TfrmLogicSim.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_BACK, VK_DELETE:
    begin
      RemoveCurrentlySelected;
      RedrawAllWires;
    end;
    VK_ESCAPE:
    begin
      DeselectCurrentlySelected;
      RemoveTmpWire;
    end;
  end;
end;

procedure TfrmLogicSim.pnlSchemaMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  DeselectCurrentlySelected;
  RemoveTmpWire;
end;

procedure TfrmLogicSim.pnlSchemaMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  UpdateTmpWire;
end;

procedure TfrmLogicSim.SimulationTimerTimer(Sender: TObject);
var
  SimulationElapsedTime: TDateTime;
  Tmp: Pointer;
begin
  SimulationElapsedTime := Now - SimulationStartTime;
  UpdateStatusBar('Elapsed time: ' +
                  FormatDateTime('nn:ss', SimulationElapsedTime));

  // Update the input devices - important for buttons which automatically turn
  // off.
  for Tmp in InputDevicesList do
  begin
    TInputDevice(Tmp).UpdateDevice;
  end;

  if InputDevicesChanged then
  begin
    SimulateOnce;
  end;
end;

procedure TfrmLogicSim.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  UninitialiseProgram;
end;

procedure TfrmLogicSim.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if SchematicChanged then
  begin
    case AskToSave of
      ID_CANCEL: CanClose := False;
      ID_YES: btnSaveClick(nil);
    end;
  end;
end;
// END form events.

// Components toolbar buttons.
procedure TfrmLogicSim.btnGateNOTClick(Sender: TObject);
begin
  AddSchematicDevice(GATE_NOT, 10, 10);
end;

procedure TfrmLogicSim.btnGateORClick(Sender: TObject);
begin
  AddSchematicDevice(GATE_OR, 10, 10);
end;

procedure TfrmLogicSim.btnGateXNORClick(Sender: TObject);
begin
  AddSchematicDevice(GATE_XNOR, 10, 10);
end;

procedure TfrmLogicSim.btnGateXORClick(Sender: TObject);
begin
  AddSchematicDevice(GATE_XOR, 10, 10);
end;

procedure TfrmLogicSim.btnGateANDClick(Sender: TObject);
begin
  AddSchematicDevice(GATE_AND, 10, 10);
end;

procedure TfrmLogicSim.btnGateNANDClick(Sender: TObject);
begin
  AddSchematicDevice(GATE_NAND, 10, 10);
end;

procedure TfrmLogicSim.btnGateNORClick(Sender: TObject);
begin
  AddSchematicDevice(GATE_NOR, 10, 10);
end;

procedure TfrmLogicSim.btnOutputIndicatorClick(Sender: TObject);
begin
  AddSchematicDevice(OD_INDICATOR, 10, 10);
end;

procedure TfrmLogicSim.btnInputButtonClick(Sender: TObject);
begin
  AddSchematicDevice(ID_BUTTON, 10, 10);
end;

procedure TfrmLogicSim.btnInputSwitchClick(Sender: TObject);
begin
  AddSchematicDevice(ID_SWITCH, 10, 10);
end;
// END components toolbar buttons.

// Main toolbar buttons.
procedure TfrmLogicSim.btnNewClick(Sender: TObject);
begin
  if SchematicChanged then
  begin
    case AskToSave of
      ID_CANCEL: Exit;
      ID_YES: btnSaveClick(nil);
    end;
  end;

  ClearSchematic;
end;

procedure TfrmLogicSim.btnOpenClick(Sender: TObject);
var
  Location: string;
begin
  StopSimulation;

  if SchematicChanged then
  begin
    case AskToSave of
      ID_CANCEL: Exit;
      ID_YES: btnSaveClick(nil);
    end;
  end;

  // Show open file selection dialog
  // diaOpen handles non-existent files
  if diaOpen.Execute then
  begin
    Location := diaOpen.FileName;

    // Cancel button has been pressed.
    if Location = '' then
    begin
      Exit;
    end;
  end;

  ClearSchematic;
  if LoadSchematicFromFile(Location) then
  begin
    // Mark the schematic as unchanged since the last save.
    SchematicChanged := False;
    DeselectCurrentlySelected;
  end
  else
  begin
    DisplayError('Could not load the schematic.');
  end;
end;

procedure TfrmLogicSim.btnSaveClick(Sender: TObject);
var
  Location: string;
begin
  StopSimulation;

  // Show save file selection dialog.
  if diaSave.Execute then
  begin
    Location := diaSave.FileName;
  end;

  if FileExists(Location) then
  begin
    // Exit the procedure if the user does not wish to overwrite existing file.
    if AskToOverwrite = ID_CANCEL then
    begin
      Exit;
    end;
  end;

  if WriteSchematicToFile(Location) then
  begin
    // Mark the schematic as unchanged since the last save.
    SchematicChanged := False;
  end
  else
  begin
    DisplayError('Could not save the schematic.');
  end;
end;

procedure TfrmLogicSim.btnSnapToGridClick(Sender: TObject);
begin
  SnapToGrid := not SnapToGrid;
end;

procedure TfrmLogicSim.btnRunClick(Sender: TObject);
begin
  StartSimulation;
end;

procedure TfrmLogicSim.btnStopClick(Sender: TObject);
begin
  StopSimulation;
end;

procedure TfrmLogicSim.btnMouseClick(Sender: TObject);
begin
  SetToolType(TOOL_MOUSE);
end;

procedure TfrmLogicSim.btnWireClick(Sender: TObject);
begin
  SetToolType(TOOL_WIRE);
end;
// END main toolbar.

end.
