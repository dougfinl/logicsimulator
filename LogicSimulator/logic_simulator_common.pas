unit logic_simulator_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, LCLType, TypInfo, DateUtils;

type
  TToolType = (TOOL_MOUSE, TOOL_WIRE);

  TGateType = (GATE_NOT, GATE_OR, GATE_AND, GATE_NOR, GATE_NAND, GATE_XOR,
    GATE_XNOR);

  TOutputDeviceType = (OD_INDICATOR);

  TInputDeviceType = (ID_SWITCH, ID_BUTTON);

  TSchematicDevice = class;

  TNodeType = (NODE_UNUSED, NODE_INPUT, NODE_OUTPUT);

  { TNode }
  TNode = record
    NodeType: TNodeType;
    PositionRelativeToDevice: TPoint;
    State: Boolean;
    LastUpdate: Integer;
    ConnectedWires: TList;
    ParentSchematicDevice: TSchematicDevice;
  end;

  { TNodePtr }
  TNodePtr = ^TNode;

  { TSchematicDevice }
  TSchematicDevice = class(TCustomImage)
  private
    Selected: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DrawNode(ANode: TNode);
    procedure Reset; virtual;
    procedure UpdateImage; virtual;
    procedure UpdateDevice; virtual;

    { Event handlers }
    procedure OnPaintHandler(Sender: TObject); virtual;
    procedure OnMouseDownHandler(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X: longint; {%H-}Y: longint); virtual;
    procedure OnMouseMoveHandler(Sender: TObject; Shift: TShiftState;
      {%H-}X: longint; {%H-}Y: longint); virtual;
  end;

  { TWire }
  TWire = class(TCustomImage)
  private
    Selected: Boolean;
    PStartNode: ^TNode;
    PEndNode: ^TNode;
    EndPoint: TPoint;
    FlippedX: Boolean;
    FlippedY: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetStart(var ANode: TNode);
    procedure SetEnd(const ANode: TNode); overload;
    procedure SetEnd(const APoint: TPoint); overload;
    procedure SetEnd(const X: Integer; const Y: Integer); overload;
    procedure CalculateNewSize;
    procedure AffectInputNode;

    { Event handlers }
    procedure OnPaintHandler(Sender: TObject);
    procedure OnMouseDownHandler(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X: longint; {%H-}Y: longint);
    procedure OnMouseMoveHandler(Sender: TObject; Shift: TShiftState;
      {%H-}X: longint; {%H-}Y: longint);
  end;

  { TLogicGate }
  TLogicGate = class(TSchematicDevice)
  private
    GateType: TGateType;
    InputNode1: TNode;
    InputNode2: TNode;
    OutputNode: TNode;
  public
    constructor Create(AOwner: TComponent; NewGateType: TGateType); reintroduce;
    destructor Destroy; override;
    procedure Reset; override;
    procedure UpdateDevice; override;
    function GetOutputNodeState: Boolean;

    { Event handlers }
    procedure OnPaintHandler(Sender: TObject); override;
    procedure OnMouseDownHandler(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X: longint; Y: longint); override;
    procedure OnMouseMoveHandler(Sender: TObject; Shift: TShiftState;
      {%H-}X: longint; {%H-}Y: longint); override;
  end;

  { TOutputDevice }
  TOutputDevice = class(TSchematicDevice)
  private
    OutputDeviceType: TOutputDeviceType;
    ImageIDOn: Integer;
    ImageIDOff: Integer;
    InputNode: TNode;
  public
    constructor Create(AOwner: TComponent;
                       NewOutputDeviceType: TOutputDeviceType);
      reintroduce;
    destructor Destroy; override;
    procedure Reset; override;
    procedure UpdateImage; override;
    procedure UpdateDevice; override;

    { Event handlers }
    procedure OnPaintHandler(Sender: TObject); override;
    procedure OnMouseDownHandler(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X: longint; Y: longint); override;
    procedure OnMouseMoveHandler(Sender: TObject; Shift: TShiftState;
      {%H-}X: longint; {%H-}Y: longint); override;
  end;

  { TInputDevice }
  TInputDevice = class(TSchematicDevice)
  private
    InputDeviceType: TInputDeviceType;
    OutputNode: TNode;
    ImageIDOn: Integer;
    ImageIDOff: Integer;
    ButtonPressedTime: TDateTime;
  public
    constructor Create(AOwner: TComponent;
                       NewInputDeviceType: TInputDeviceType); reintroduce;
    destructor Destroy; override;
    procedure Reset; override;
    procedure UpdateDevice; override;
    procedure UpdateImage; override;
    function GetInputDeviceType: TInputDeviceType;

    { Event handlers }
    procedure OnPaintHandler(Sender: TObject); override;
    procedure OnMouseDownHandler(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X: longint; Y: longint); override;
    procedure OnMouseMoveHandler(Sender: TObject; Shift: TShiftState;
      {%H-}X: longint; {%H-}Y: longint); override;
  end;


procedure InitialiseProgram;
procedure UninitialiseProgram;
procedure ClearSchematic;
procedure SetToolType(const NewToolType: TToolType);
function WriteSchematicToFile(const Location: string): Boolean; experimental;
function LoadSchematicFromFile(const Location: string): Boolean; experimental;
procedure DisplayError(const Text: string);
procedure DisplayWarning(const Text: string);
procedure UpdateStatusBar(const Text: string);
procedure AddSchematicDevice(const GateType: TGateType; X, Y: Integer); overload;
procedure AddSchematicDevice(const InputDeviceType: TInputDeviceType; X, Y: Integer); overload;
procedure AddSchematicDevice(const OutputDeviceType: TOutputDeviceType; X, Y: Integer); overload;
procedure RemoveCurrentlySelected;
procedure DeselectCurrentlySelected;
procedure RepaintAllSchematicDevices;
function AskToSave: Integer;
function AskToOverwrite: Integer;
procedure UpdateTmpWire;
procedure RemoveTmpWire;
procedure RedrawAllWires;
procedure UpdateNodeState(PANode: TNodePtr; const NewState: Boolean);


implementation

uses
  logic_simulator_form, logic_simulator_globals, logic_simulator_simulation;

type
  { TSaveData }
  TSaveData = record
    ObjectType: String[20];
    ObjectSpecificType: Integer;
    Top: Integer;
    Left: Integer;
  end;


var
  CurrentTool: TToolType;
  CurrentlySelectedDevice: TSchematicDevice;
  CurrentlySelectedWire: TWire;
  PLastNodeClicked: ^TNode;
  TmpWire: TWire;


// InitialiseProgram
//
// Allocates memory to lists, sets up basic user interface variables and enters
// edit mode.
procedure InitialiseProgram;
begin
  GatesList := TList.Create;
  InputDevicesList := TList.Create;
  OutputDevicesList := TList.Create;
  WiresList := TList.Create;

  frmLogicSim.SimulationTimer.Enabled := False;
  frmLogicSim.SimulationTimer.Interval := SIMULATION_INTERVAL;

  // There is no device currently selected/wire, as none exist!
  CurrentlySelectedDevice := Nil;
  CurrentlySelectedWire := Nil;
  PLastNodeClicked := Nil;
  TmpWire := Nil;
  SetToolType(TOOL_MOUSE);
  UpdateStatusBar('Edit mode');
end;

// UnitialiseProgram
//
// Frees any allocated memory back to the OS.
procedure UninitialiseProgram;
begin
  StopSimulation;
  ClearSchematic;

  GatesList.Free;
  InputDevicesList.Free;
  OutputDevicesList.Free;
  WiresList.Free;
end;

// ClearSchematic
//
// Removes any schematic devices from the schematic, and frees any schematic
// devices.
procedure ClearSchematic;
var
  Tmp: Pointer;
begin
  DeselectCurrentlySelected;

  for Tmp in GatesList do
  begin
    TLogicGate(Tmp).Free;
  end;
  GatesList.Clear;

  for Tmp in InputDevicesList do
  begin
    TInputDevice(Tmp).Free;
  end;
  InputDevicesList.Clear;

  for Tmp in OutputDevicesList do
  begin
    TOutputDevice(Tmp).Free;
  end;
  OutputDevicesList.Clear;

  for Tmp in WiresList do
  begin
    TWire(Tmp).Free;
  end;
  WiresList.Clear;
end;

// SetToolType
procedure SetToolType(const NewToolType: TToolType);
begin
  CurrentTool := NewToolType;
  DeselectCurrentlySelected;
  RemoveTmpWire;
  RepaintAllSchematicDevices;

  case NewToolType of
    TOOL_WIRE:
    begin
      frmLogicSim.btnWire.Enabled := False;
      frmLogicSim.btnMouse.Enabled := True;
    end;
    TOOL_MOUSE:
    begin
      frmLogicSim.btnWire.Enabled := True;
      frmLogicSim.btnMouse.Enabled := False;
    end;
  end;
end;

// WriteSchematicToFile
function WriteSchematicToFile(const Location: string): Boolean;
var
  SaveFile: file of TSaveData;
  TmpSaveRecord: TSaveData;
  Tmp: Pointer;
begin
  Result := True;
  AssignFile(SaveFile, Location);

  try
    // Open the file for writing.
    Rewrite(SaveFile);

    for Tmp in GatesList do
    begin
      TmpSaveRecord.ObjectType := 'TLogicGate';
      TmpSaveRecord.ObjectSpecificType := Ord(TLogicGate(Tmp).GateType);
      TmpSaveRecord.Top := TLogicGate(Tmp).Top;
      TmpSaveRecord.Left := TLogicGate(Tmp).Left;
      Write(SaveFile, TmpSaveRecord);
    end;

    for Tmp in InputDevicesList do
    begin
      TmpSaveRecord.ObjectType := 'TInputDevice';
      TmpSaveRecord.ObjectSpecificType := Ord(TInputDevice(Tmp).InputDeviceType);
      TmpSaveRecord.Top := TInputDevice(Tmp).Top;
      TmpSaveRecord.Left := TInputDevice(Tmp).Left;
      Write(SaveFile, TmpSaveRecord);
    end;

    for Tmp in OutputDevicesList do
    begin
      TmpSaveRecord.ObjectType := 'TOutputDevice';
      TmpSaveRecord.ObjectSpecificType := Ord(TOutputDevice(Tmp).OutputDeviceType);
      TmpSaveRecord.Top := TOutputDevice(Tmp).Top;
      TmpSaveRecord.Left := TOutputDevice(Tmp).Left;
      Write(SaveFile, TmpSaveRecord);
    end;

    for Tmp in WiresList do
    begin
      TmpSaveRecord.ObjectType := 'TWire';
      TmpSaveRecord.ObjectSpecificType := -1;
    end;
  except
    on E: EInOutError do
    begin
      Result := False;
    end;
  end;

  CloseFile(SaveFile);
end;

// LoadSchematicFromFile
function LoadSchematicFromFile(const Location: string): Boolean;
var
  LoadFile: file of TSaveData;
  TmpLoadRecord: TSaveData;
begin
  Result := True;

  AssignFile(LoadFile, Location);

  try
    // Open the file for reading.
    Reset(LoadFile);

    repeat
      Read(LoadFile, TmpLoadRecord);

      case TmpLoadRecord.ObjectType of
        'TLogicGate':    begin
                           AddSchematicDevice(TGateType(TmpLoadRecord.ObjectSpecificType), TmpLoadRecord.Left, TmpLoadRecord.Top);
                         end;
        'TInputDevice':  begin
                           AddSchematicDevice(TInputDeviceType(TmpLoadRecord.ObjectSpecificType), TmpLoadRecord.Left, TmpLoadRecord.Top);
                         end;
        'TOutputDevice': begin
                           AddSchematicDevice(TOutputDeviceType(TmpLoadRecord.ObjectSpecificType), TmpLoadRecord.Left, TmpLoadRecord.Top);
                         end;
      end;
    until EOF(LoadFile);
  except
    on E: EInOutError do
    begin
      Result := False;
    end;
  end;

  CloseFile(LoadFile);
end;

// DisplayError
//
// Displays Text in a pop-up message with an error icon.
procedure DisplayError(const Text: string);
begin
  MessageDlg(Text, mtError, [mbOK], 0);
end;

// DisplayWarning
//
// Displays Text in a pop-up message with a warning icon.
procedure DisplayWarning(const Text: string);
begin
  MessageDlg(Text, mtWarning, [mbOK], 0);
end;

// UpdateStatusBar
//
// Updates the status bar with Text.
procedure UpdateStatusBar(const Text: string);
begin
  frmLogicSim.sbStatus.Panels[0].Text := Text;
end;

// AddSchematicDevice
//
// Overloaded procedure that will add a specific type of device to the schematic
// at the specified position. Adds the schematic device to the corresponding
// list (GatesList, InputDevicesList, OutputDevicesList).
procedure AddSchematicDevice(const GateType: TGateType; X, Y: Integer); overload;
var
  Tmp: TLogicGate;
begin
  Tmp := TLogicGate.Create(frmLogicSim.pnlSchema, GateType);
  Tmp.Left := X;
  Tmp.Top := Y;
  GatesList.Add(Tmp);
end;

procedure AddSchematicDevice(const InputDeviceType: TInputDeviceType; X, Y: Integer); overload;
var
  Tmp: TInputDevice;
begin
  Tmp := TInputDevice.Create(frmLogicSim.pnlSchema, InputDeviceType);
  Tmp.Left := X;
  Tmp.Top := Y;
  InputDevicesList.Add(tmp);
end;

procedure AddSchematicDevice(const OutputDeviceType: TOutputDeviceType; X, Y: Integer); overload;
var
  Tmp: TOutputDevice;
begin
  Tmp := TOutputDevice.Create(frmLogicSim.pnlSchema, OutputDeviceType);
  Tmp.Left := X;
  Tmp.Top := Y;
  OutputDevicesList.Add(Tmp);
end;
// END AddSchematicDevice (overloaded)

// RemoveWire
//
// Removes AWire from the schematic, updates ConnectedWires of the affected
// nodes and frees the AWire's memory. Also removes the wire from WiresList.
procedure RemoveWire(AWire: TWire);
begin
  AWire.PStartNode^.ConnectedWires.Remove(AWire);
  AWire.PEndNode^.ConnectedWires.Remove(AWire);

  WiresList.Remove(AWire);
  AWire.Free;
end;

// RemoveSchematicDevice
//
// Removes ADevice from the schematic, along with any connected wires. Also
// removes ADevice from the corresponding list (GatesList, InputDevicesList,
// OutputDevicesList) and frees memory used by ADevice.
procedure RemoveSchematicDevice(ADevice: TSchematicDevice);
begin
  case ADevice.ClassName of
    'TLogicGate':
    begin
      GatesList.Remove(ADevice);

      while TLogicGate(ADevice).OutputNode.ConnectedWires.Count <> 0 do
      begin
        RemoveWire(TWire(TLogicGate(ADevice).OutputNode.ConnectedWires[0]));
      end;

      while TLogicGate(ADevice).InputNode1.ConnectedWires.Count <> 0 do
      begin
        RemoveWire(TWire(TLogicGate(ADevice).InputNode1.ConnectedWires[0]));
      end;

      while TLogicGate(ADevice).InputNode2.ConnectedWires.Count <> 0 do
      begin
        RemoveWire(TWire(TLogicGate(ADevice).InputNode2.ConnectedWires[0]));
      end;
    end;
    'TInputDevice':
    begin
      InputDevicesList.Remove(ADevice);

      while TInputDevice(ADevice).OutputNode.ConnectedWires.Count <> 0 do
      begin
        RemoveWire(TWire(TInputDevice(ADevice).OutputNode.ConnectedWires[0]));
      end;
    end;
    'TOutputDevice':
    begin
      OutputDevicesList.Remove(ADevice);

      while TOutputDevice(ADevice).InputNode.ConnectedWires.Count <> 0 do
      begin
        RemoveWire(TWire(TOutputDevice(ADevice).InputNode.ConnectedWires[0]));
      end;
    end;
  end;
  ADevice.Free;
end;

// SelectDevice
//
// Overloaded procedure that will mark AGate or AWire as the
// CurrentlySelectedDevice. Sets the Selected property of the device to TRUE and
// causes the device to be redrawn.
procedure SelectDevice(AGate: TSchematicDevice); overload;
begin
  DeselectCurrentlySelected;
  CurrentlySelectedDevice := AGate;
  CurrentlySelectedDevice.Selected := True;
  CurrentlySelectedDevice.Invalidate;
end;

procedure SelectDevice(AWire: TWire); overload;
begin
  DeselectCurrentlySelected;
  CurrentlySelectedWire := AWire;
  CurrentlySelectedWire.Selected := True;
  CurrentlySelectedWire.Invalidate;
end;
// END SelectDevice (overloaded)

// RemoveCurrentlySelected
//
// Removes the currently selected device or wire from the schematic.
procedure RemoveCurrentlySelected;
var
  TmpDevice: TSchematicDevice;
  TmpWire: TWire;
begin
  if CurrentlySelectedDevice <> Nil then
  begin
    TmpDevice := CurrentlySelectedDevice;
    DeselectCurrentlySelected;
    RemoveSchematicDevice(TmpDevice);
  end;

  if CurrentlySelectedWire <> Nil then
  begin
    TmpWire := CurrentlySelectedWire;
    DeselectCurrentlySelected;
    RemoveWire(TmpWire);
  end;
end;

// DeselectCurrentlySelected
//
// Deselects the currently selected device or wire.
procedure DeselectCurrentlySelected;
begin
  if CurrentlySelectedDevice <> Nil then
  begin
    CurrentlySelectedDevice.Selected := False;
    CurrentlySelectedDevice.Invalidate;
    CurrentlySelectedDevice := Nil;
  end;

  if CurrentlySelectedWire <> Nil then
  begin
    CurrentlySelectedWire.Selected := False;
    CurrentlySelectedWire.Invalidate;
    CurrentlySelectedWire := Nil;
  end;
end;

// RepaintAllSchematicDevices
//
// Calls Invalidate on every device in the schematic, causing them to be
// redrawn.
procedure RepaintAllSchematicDevices;
var
  Tmp: Pointer;
begin
  for Tmp in GatesList do
  begin
    TLogicGate(Tmp).Invalidate;
  end;

  for Tmp in InputDevicesList do
  begin
    TInputDevice(Tmp).Invalidate;
  end;

  for Tmp in OutputDevicesList do
  begin
    TOutputDevice(Tmp).Invalidate;
  end;
end;

// AskToSave
//
// Prompts the user with a message dialog box asking if they want to save the
// schematic to a file, with yes/no/cancel as response options.
function AskToSave: integer;
begin
  Result := MessageDlg('Would you like to save the current schematic?',
    mtConfirmation, mbYesNoCancel, 0);
end;

// AskToOverwrite
//
// Prompts the user with a message dialog box asking if they want to overwrite
// an existing file, with yes/no/cancel as response options.
function AskToOverwrite: Integer;
begin
  Result := MessageDlg('File exists. Overwrite?', mtConfirmation,
    mbYesNoCancel, 0);
end;

// UpdateTmpWire
//
// Causes the temporary wire to be redrawn. TmpWire connects the last node
// clicked to the current mouse position.
procedure UpdateTmpWire;
begin
  if (ApplicationState = EDITING) and (CurrentTool = TOOL_WIRE) and
     (PLastNodeClicked <> Nil) then
  begin
    // Create an instance of TWire if it doesn't already exist.
    if TmpWire = Nil then
    begin
      TmpWire := TWire.Create(frmLogicSim.pnlSchema);
      // Set the start position of TmpWire.
      TmpWire.SetStart(PLastNodeClicked^);
    end;
    // Set the end position of TmpWire as the cursor position.
    TmpWire.SetEnd(frmLogicSim.pnlSchema.ScreenToClient(Mouse.CursorPos));

    TmpWire.Invalidate;
  end
  else
  begin
    // Destroy the old TmpWire if it exists.
    if TmpWire <> Nil then
    begin
      TmpWire.Free;
      TmpWire := Nil;
    end;
  end;
end;

// RemoveTmpWire
procedure RemoveTmpWire;
begin
  PLastNodeClicked := Nil;
  UpdateTmpWire;
end;

// RedrawAllWires
//
// Causes all wires to recalculate their size and redraws them.
procedure RedrawAllWires;
var
  Tmp: Pointer;
begin
  for Tmp in WiresList do
  begin
    TWire(Tmp).CalculateNewSize;
    TWire(Tmp).Invalidate;
  end;
end;

// UpdateNodeState
//
// Updates the state of a node and sets LastUpdate to the current time.
procedure UpdateNodeState(PANode: TNodePtr; const NewState: Boolean);
begin
  if PANode^.State <> NewState then
  begin
    PANode^.State :=  NewState;
    PANode^.LastUpdate := Round(Now * 24 * 60 * 60 * 1000);
  end;
end;

// InitialiseNewNode
//
// Sets up basic initial values for a new, unitialised node, and allocate
// memory for the ConnectedWires list.
procedure InitialiseNewNode(var ANode: TNode; const Parent: TSchematicDevice;
  const NewNodeType: TNodeType; const X: Integer; const Y: Integer);
begin
  ANode.ParentSchematicDevice := Parent;

  ANode.PositionRelativeToDevice.x := X;
  ANode.PositionRelativeToDevice.y := Y;

  ANode.NodeType := NewNodeType;
  ANode.State := False;
  ANode.LastUpdate := 0;

  ANode.ConnectedWires := TList.Create;
end;

// UnitialiseNode
// Free the memory used for ConnectedWires list.
procedure UninitialiseNode(var ANode: TNode);
begin
  ANode.ConnectedWires.Free;
end;

// IsMouseInNodeArea
//
// Returns TRUE if the mouse is currently sitting over a node. Otherwise,
// returns FALSE.
function IsMouseInNodeArea(const MouseX, MouseY: Integer; const ANode: TNode):
  Boolean;
var
  TopLeftX, TopLeftY, BottomRightX, BottomRightY: Integer;
begin
  TopLeftX := ANode.PositionRelativeToDevice.x;// - (NODE_WIDTH div 2);
  TopLeftY := ANode.PositionRelativeToDevice.y - (NODE_HEIGHT div 2);
  BottomRightX := TopLeftX + NODE_WIDTH;
  BottomRightY := TopLeftY + NODE_HEIGHT;

  if (MouseX >= TopLeftX) and (MouseX <= BottomRightX) and
     (MouseY >= TopLeftY) and (MouseY <= BottomRightY) then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

// NodeClicked
//
// Processes the event of a node being clicked by the user. If a node has
// already been clicked, validation will be performed and the two nodes will be
// connected with a wire if passed. Useful error messages are displayed
// if validation fails.
procedure NodeClicked(PANode: TNodePtr);
var
  InvalidConnection: Boolean;
  TmpNewWire: TWire;
  Tmp: Pointer;
begin
  InvalidConnection := False;

  if PLastNodeClicked = Nil then
  begin
    // The first node has been clicked.
    PLastNodeClicked := PANode;
  end
  else
  begin
    // The second node has been clicked.

    // Check to see if the two nodes are the same.
    if PLastNodeClicked = PANode then
    begin
      DisplayError('Cannot connect a node to itself.');
      InvalidConnection := True;
    end
    else
    begin
      // Check to see if two input nodes are trying to be connected together.
      if (PANode^.NodeType = NODE_INPUT) and
         (PLastNodeClicked^.NodeType = NODE_INPUT) then
      begin
        DisplayError('Cannot connect two input nodes together.');
        InvalidConnection := True;
      end
      else
      begin
        // Check to see if two output nodes are trying to be connected together.
        if (PANode^.NodeType = NODE_OUTPUT) and
           (PLastNodeClicked^.NodeType = NODE_OUTPUT) then
        begin
          DisplayError('Cannot connect two output nodes together.');
          InvalidConnection := True;
        end;
      end;
    end;

    // Check to see if more than one output can't connect to the same input.
    for Tmp in WiresList do
    begin
      if (TWire(Tmp).PEndNode = PANode) then
      begin
        DisplayError('Only one wire can be connected to a single input.');
        InvalidConnection := True;
      end;
    end;

    // Check to see if the two nodes are already connected.
    for Tmp in WiresList do
    begin
      if ((TWire(Tmp).PStartNode = PANode) and
          (TWire(Tmp).PEndNode = PLastNodeClicked)) or
         ((TWire(Tmp).PStartNode = PLastNodeClicked) and
          (TWire(Tmp).PEndNode = PANode)) then
      begin
        DisplayError('Wire already exists.');
        InvalidConnection := True;
      end;
    end;

    if not InvalidConnection then
    begin
      // Connect the two nodes, since no error has been found.
      TmpNewWire := TWire.Create(frmLogicSim.pnlSchema);
      TmpNewWire.SetStart(PLastNodeClicked^);
      TmpNewWire.SetEnd(PANode^);

      // Add TmpNewWire to ConnectedWires list of both nodes.
      PANode^.ConnectedWires.Add(TmpNewWire);
      PLastNodeClicked^.ConnectedWires.Add(TmpNewWire);

      WiresList.Add(TmpNewWire);

      RemoveTmpWire;
    end;
  end;
end;

// TWire
// Create
constructor TWire.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := frmLogicSim.pnlSchema;

  Selected := False;
  PStartNode := Nil;
  PEndNode := Nil;

  // Should be able to see through the bounding box of a wire.
  Transparent := True;
  Picture.Bitmap.Transparent := True;
  Picture.Bitmap.TransparentColor := clFuchsia;
  Picture.Bitmap.TransparentMode := tmFixed;

  // Assign event handlers.
  OnPaint := @OnPaintHandler;
  OnMouseDown := @OnMouseDownHandler;
  OnMouseMove := @OnMouseMoveHandler;
end;

// SetStart
procedure TWire.SetStart(var ANode: TNode);
begin
  PStartNode := @ANode;
  CalculateNewSize;
end;

// SetEnd
//
// Overloaded function that can take either a TNode, TPoint or separate X/Y
// values (for setting the end position to the cursor position).
procedure TWire.SetEnd(const ANode: TNode);
begin
  PEndNode := @ANode;
  CalculateNewSize;
end;

procedure TWire.SetEnd(const APoint: TPoint);
begin
  PEndNode := Nil;
  EndPoint.x := APoint.x;
  EndPoint.y := APoint.y;
  CalculateNewSize;
end;

procedure TWire.SetEnd(const X: Integer; const Y: Integer);
begin
  PEndNode := Nil;
  EndPoint.x := X;
  EndPoint.y := Y;
  CalculateNewSize;
end;
// END SetEnd (overloaded)

// CalculateNewSize
//
// Calculates the new canvas size for the wire, based on the distance between
// the start node and end node.
procedure TWire.CalculateNewSize;
var
  StartCoord, EndCoord: TPoint;
begin
  StartCoord.x := PStartNode^.ParentSchematicDevice.Left +
                  PStartNode^.PositionRelativeToDevice.x;
  StartCoord.y := PStartNode^.ParentSchematicDevice.Top +
                  PStartNode^.PositionRelativeToDevice.y;

  if PEndNode = Nil then
  begin
    // Use EndPoint.
    EndCoord := EndPoint;
  end
  else
  begin
    // Use PEndNode.
    EndCoord.x := PEndNode^.ParentSchematicDevice.Left +
                  PEndNode^.PositionRelativeToDevice.x;
    EndCoord.y := PEndNode^.ParentSchematicDevice.Top +
                  PEndNode^.PositionRelativeToDevice.y;
  end;

  Width := abs(StartCoord.x - EndCoord.x);;
  Height := abs(StartCoord.y - EndCoord.y);;

  Top := StartCoord.y;
  Left := StartCoord.x;

  FlippedX := False;
  if EndCoord.x < StartCoord.x then
  begin
    // EndCoord is to left of StartCoord.
    Left := Left - Width;
    FlippedX := True;
  end;

  FlippedY := False;
  if EndCoord.y < StartCoord.y then
  begin
    // EndCoord is above StartCoord.
    Top := Top - Height;
    FlippedY := True;
  end;

  // Adjust canvas size if thinner than the wire's thickness.
  if Height < WIRE_THICKNESS then
  begin
    Height := WIRE_THICKNESS;
    Top := Top - (WIRE_THICKNESS div 2);
  end;
  if Width < WIRE_THICKNESS then
  begin
    Width := WIRE_THICKNESS;
    Left := Left - (WIRE_THICKNESS div 2);
  end;
end;

// AffectInputNode
//
// Updates the connected input node with the signal from the connected output
// node.
procedure TWire.AffectInputNode;
begin
  if PStartNode^.NodeType = NODE_INPUT then
  begin
    UpdateNodeState(PStartNode, PEndNode^.State);
  end
  else
  begin
    UpdateNodeState(PEndNode, PStartNode^.State);
  end;
end;

// OnPaintHandler
//
// Repaints the wire inside its canvas with the correct orientation based on
// FlippedX and FlippedY. Also sets the colour of the wire depending on the
// state of the connected output node.
procedure TWire.OnPaintHandler(Sender: TObject);
var
  // Points relative to TWire's canvas.
  StartPointRel, EndPointRel: TPoint;
  LineColour: TColor;
begin
  if PStartNode^.NodeType = NODE_OUTPUT then
  begin
    if PStartNode^.State = True then
    begin
      LineColour := WIRE_HIGH_COLOUR;
    end
    else
    begin
      LineColour := WIRE_LOW_COLOUR;
    end;
  end
  else
  begin
    if PStartNode^.State = True then
    begin
      LineColour := WIRE_HIGH_COLOUR;
    end
    else
    begin
      LineColour := WIRE_LOW_COLOUR;
    end;
  end;

  if (not FlippedY) and FlippedX then
  begin
    StartPointRel.x := Width-1;
    StartPointRel.y := 0;
    EndPointRel.x := 0;
    EndPointRel.y := Height-1;
  end;

  if FlippedY and (not FlippedX) then
  begin
    StartPointRel.x := 0;
    StartPointRel.y := Height-1;
    EndPointRel.x := Width-1;
    EndPointRel.y := 0;
  end;

  if FlippedX and FlippedY then
  begin
    StartPointRel.x := Width-1;
    StartPointRel.y := Height-1;
    EndPointRel.x := 0;
    EndPointRel.y := 0;
  end;

  if (not FlippedX) and (not FlippedY) then
  begin
    StartPointRel.x := 0;
    StartPointRel.y := 0;
    EndPointRel.x := Width-1;
    EndPointRel.y := Height-1;
  end;

  with Canvas do
  begin
    // Draw the wire.
    Pen.Width := WIRE_THICKNESS;

    if Selected then
    begin
      Pen.Color := WIRE_SELECTED_COLOUR;
    end
    else
    begin
      Pen.Color := LineColour;
    end;

    Line(StartPointRel, EndPointRel);
  end;
end;

// OnMouseDownHandler
procedure TWire.OnMouseDownHandler(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X: longint; Y: longint);
begin
  if ApplicationState = EDITING then
  begin
    SelectDevice(self);
  end;
end;

// OnMouseMoveHandler
procedure TWire.OnMouseMoveHandler(Sender: TObject; Shift: TShiftState;
  X: longint; Y: longint);
begin
  // Pass the MouseMove event to pnlSchema, so wires are redrawn correctly.
  frmLogicSim.pnlSchemaMouseMove(Sender, Shift, X, Y);
end;
// END TWire

// TInputDevice
// Create
constructor TInputDevice.Create(AOwner: TComponent;
  NewInputDeviceType: TInputDeviceType);
begin
  inherited Create(AOwner);
  InputDeviceType := NewInputDeviceType;

  InitialiseNewNode(OutputNode, self, NODE_OUTPUT, 60 - NODE_WIDTH, 23);

  SetBounds(10, 10, 60, 46);

  case InputDeviceType of
    ID_BUTTON: begin
                 ImageIDOff := BUTTON_RELEASED_IMG_ID;
                 ImageIDOn := BUTTON_PRESSED_IMG_ID;
                end;
    ID_SWITCH: begin
                 ImageIDOff := SWITCH_RELEASED_IMG_ID;
                 ImageIDOn := SWITCH_PRESSED_IMG_ID;
                end;
  end;

  ButtonPressedTime := 0;
  UpdateImage;
end;

// Destroy
destructor TInputDevice.Destroy;
begin
  UninitialiseNode(OutputNode);

  inherited Destroy;
end;

// Reset
procedure TInputDevice.Reset;
begin
  UpdateNodeState(@OutputNode, False);
  ButtonPressedTime := 0;

  inherited Reset;
end;

// Update
procedure TInputDevice.UpdateDevice;
var
  OldNodeState: Boolean;
begin
  inherited UpdateDevice;

  OldNodeState := OutputNode.State;

  if InputDeviceType = ID_BUTTON then
  begin
    if MilliSecondsBetween(Now, ButtonPressedTime)
       >= SIMULATION_BUTTON_TIME then
    begin
      UpdateNodeState(@OutputNode, False);

      // If the node state has changed, set InputDevicesChanged to true to
      // execute a pass of the simulator.
      if OldNodeState <> OutputNode.State then
      begin
        InputDevicesChanged := True;
      end;

      UpdateImage;
    end;
  end;
end;

// UpdateImage
procedure TInputDevice.UpdateImage;
begin
  if OutputNode.State = True then
  begin
    frmLogicSim.ilComponents.GetBitmap(ImageIDOn, Picture.Bitmap);
  end
  else
  begin
    frmLogicSim.ilComponents.GetBitmap(ImageIDOff, Picture.Bitmap);
  end;
end;

// GetInputDeviceType
function TInputDevice.GetInputDeviceType: TInputDeviceType;
begin
  Result := InputDeviceType;
end;

// MouseDownHandler
procedure TInputDevice.OnMouseDownHandler(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X: longint; Y: longint);
var
  OldNodeState: Boolean;
begin
  inherited OnMouseDownHandler(Sender, Button, Shift, X, Y);

  if CurrentTool = TOOL_WIRE then
  begin
    if IsMouseInNodeArea(X, Y, OutputNode) then
    begin
      NodeClicked(@OutputNode);
    end;
  end;

  if ApplicationState = SIMULATING then
  begin
    OldNodeState := OutputNode.State;

    case InputDeviceType of
      ID_SWITCH: begin
                   UpdateNodeState(@OutputNode, not OutputNode.State);
                 end;
      ID_BUTTON: begin
                   UpdateNodeState(@OutputNode, True);
                   ButtonPressedTime := Now;
                 end;
    end;

    UpdateImage;

    // If the node state has changed, set InputDevicesChanged to true to execute
    // a pass of the simulator.
    if OldNodeState <> OutputNode.State then
    begin
      InputDevicesChanged := True;
    end;
  end;
end;

// OnMouseMoveHandler
procedure TInputDevice.OnMouseMoveHandler(Sender: TObject; Shift: TShiftState;
  X: longint; Y: longint);
begin
  inherited OnMouseMoveHandler(Sender, Shift, X, Y);

  // If cursor is within the bounds of any node, change cursor to crCross, else
  // change it to crArrow (the default cursor).
  if Cursor = crCross then
  begin
    Cursor := crArrow;
  end;
  if CurrentTool = TOOL_WIRE then
  begin
    if IsMouseInNodeArea(X, Y, OutputNode) then
    begin
      Cursor := crCross;
    end;
  end;
end;

// OnPaintHandler
procedure TInputDevice.OnPaintHandler(Sender: TObject);
begin
  inherited OnPaintHandler(Sender);

  if CurrentTool = TOOL_WIRE then
  begin
    DrawNode(OutputNode);
  end;
end;
// END TInputDevice

// TOutputDevice
// Create
constructor TOutputDevice.Create(AOwner: TComponent;
  NewOutputDeviceType: TOutputDeviceType);
begin
  inherited Create(AOwner);
  OutputDeviceType := NewOutputDeviceType;

  InitialiseNewNode(InputNode, self, NODE_INPUT, 0, 23);

  SetBounds(0, 0, 60, 46);

  case OutputDeviceType of
    OD_INDICATOR:
    begin
      ImageIDOff := OUTPUT_INDICATOR_OFF_IMG_ID;
      ImageIDOn := OUTPUT_INDICATOR_ON_IMG_ID;
    end;
  end;

  frmLogicSim.ilComponents.GetBitmap(ImageIDOff, Picture.Bitmap);
end;

// Destroy
destructor TOutputDevice.Destroy;
begin
  UninitialiseNode(InputNode);

  inherited Destroy;
end;

// Reset
procedure TOutputDevice.Reset;
begin
  UpdateNodeState(@InputNode, False);

  inherited Reset;
end;

// UpdateImage
procedure TOutputDevice.UpdateImage;
begin
  if InputNode.State = True then
  begin
    frmLogicSim.ilComponents.GetBitmap(ImageIDOn, Picture.Bitmap);
  end
  else
  begin
    frmLogicSim.ilComponents.GetBitmap(ImageIDOff, Picture.Bitmap);
  end;
end;

procedure TOutputDevice.UpdateDevice;
begin
  UpdateImage;
end;

// OnPaintHandler
procedure TOutputDevice.OnPaintHandler(Sender: TObject);
begin
  inherited OnPaintHandler(Sender);

  if CurrentTool = TOOL_WIRE then
  begin
    DrawNode(InputNode);
  end;
end;

// OnMouseDownHandler
procedure TOutputDevice.OnMouseDownHandler(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X: longint; Y: longint);
begin
  inherited OnMouseDownHandler(Sender, Button, Shift, X, Y);

  if CurrentTool = TOOL_WIRE then
  begin
    if IsMouseInNodeArea(X, Y, InputNode) then
    begin
      NodeClicked(@InputNode);
    end;
  end;
end;

// OnMouseMoveHandler
procedure TOutputDevice.OnMouseMoveHandler(Sender: TObject; Shift: TShiftState;
  X: longint; Y: longint);
begin
  inherited OnMouseMoveHandler(Sender, Shift, X, Y);

  if Cursor = crCross then
  begin
    Cursor := crArrow;
  end;
  if CurrentTool = TOOL_WIRE then
  begin
    if IsMouseInNodeArea(X, Y, InputNode) then
    begin
      Cursor := crCross;
    end;
  end;
end;
// END TOutputDevice

// TLogicGate
// Create
constructor TLogicGate.Create(AOwner: TComponent; NewGateType: TGateType);
var
  ImageID: Integer;
begin
  inherited Create(AOwner);
  GateType := NewGateType;
  SetBounds(10, 10, 96, 46);

  // Initialise the nodes.
  InitialiseNewNode(OutputNode, self, NODE_OUTPUT, 96 - NODE_WIDTH, 23);
  if GateType = GATE_NOT then
  begin
    // GATE_NOT does not use the second input node.
    InitialiseNewNode(InputNode1, self, NODE_INPUT, 0, 23);
    InitialiseNewNode(InputNode2, self, NODE_UNUSED, 0, 31);
  end
  else
  begin
    // Gates other than GATE_NOT use both input nodes.
    InitialiseNewNode(InputNode1, self, NODE_INPUT, 0, 15);
    InitialiseNewNode(InputNode2, self, NODE_INPUT, 0, 31);
  end;

  case GateType of
    GATE_NOT: ImageID := GATE_NOT_IMG_ID;
    GATE_AND: ImageID := GATE_AND_IMG_ID;
    GATE_OR: ImageID := GATE_OR_IMG_ID;
    GATE_NAND: ImageID := GATE_NAND_IMG_ID;
    GATE_NOR: ImageID := GATE_NOR_IMG_ID;
    GATE_XOR: ImageID := GATE_XOR_IMG_ID;
    GATE_XNOR: ImageID := GATE_XNOR_IMG_ID;
  end;
  frmLogicSim.ilComponents.GetBitmap(ImageID, Picture.Bitmap);
end;

// Destroy
destructor TLogicGate.Destroy;
begin
  UninitialiseNode(OutputNode);
  UninitialiseNode(InputNode1);
  UninitialiseNode(InputNode2);

  inherited Destroy;
end;

// Reset
procedure TLogicGate.Reset;
begin
  UpdateNodeState(@InputNode1, False);
  UpdateNodeState(@InputNode2, False);
  UpdateNodeState(@OutputNode, False);

  inherited Reset;
end;

// Update
procedure TLogicGate.UpdateDevice;
var
  NewOutputNodeState: Boolean;
begin
  // Special case for GATE_NOT - only uses InputNode1.
  if GateType = GATE_NOT then
  begin
    NewOutputNodeState := not InputNode1.State;
  end;

  // Any other gate uses both InputNode1 and InputNode2.
  case GateType of
    GATE_AND: NewOutputNodeState := InputNode1.State and InputNode2.State;
    GATE_OR: NewOutputNodeState := InputNode1.State or InputNode2.State;
    GATE_NAND: NewOutputNodeState := not (InputNode1.State and InputNode2.State);
    GATE_NOR: NewOutputNodeState := not (InputNode1.State or InputNode2.State);
    GATE_XOR: NewOutputNodeState := InputNode1.State xor InputNode2.State;
    GATE_XNOR: NewOutputNodeState := not (InputNode1.State xor InputNode2.State);
  end;

  UpdateNodeState(@OutputNode, NewOutputNodeState);
end;

// GetOutputNodeState
function TLogicGate.GetOutputNodeState: Boolean;
begin
  Result := OutputNode.State;
end;

// OnPaintHandler
procedure TLogicGate.OnPaintHandler(Sender: TObject);
begin
  inherited OnPaintHandler(Sender);

  if CurrentTool = TOOL_WIRE then
  begin
    DrawNode(OutputNode);
    DrawNode(InputNode1);
    if GateType <> GATE_NOT then
    begin
      DrawNode(InputNode2);
    end;
  end;
end;

// OnMouseDownHandler
procedure TLogicGate.OnMouseDownHandler(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X: longint; Y: longint);
begin
  inherited OnMouseDownHandler(Sender, Button, Shift, X, Y);

  if CurrentTool = TOOL_WIRE then
  begin
    if IsMouseInNodeArea(X, Y, OutputNode) then
    begin
      NodeClicked(@OutputNode);
    end;
    if IsMouseInNodeArea(X, Y, InputNode1) then
    begin
      NodeClicked(@InputNode1);
    end;
    if IsMouseInNodeArea(X, Y, InputNode2) and (GateType <> GATE_NOT) then
    begin
      NodeClicked(@InputNode2);
    end;
  end;
end;

// OnMouseMoveHandler
procedure TLogicGate.OnMouseMoveHandler(Sender: TObject; Shift: TShiftState;
  X: longint; Y: longint);
begin
  inherited OnMouseMoveHandler(Sender, Shift, X, Y);

  if Cursor = crCross then
  begin
    Cursor := crArrow;
  end;
  if CurrentTool = TOOL_WIRE then
  begin
    if IsMouseInNodeArea(X, Y, OutputNode) or
       IsMouseInNodeArea(X, Y, InputNode1) or
       (IsMouseInNodeArea(X, Y, InputNode2) and (GateType <> GATE_NOT)) then
    begin
      Cursor := crCross;
    end;
  end;
end;
// END TLogicGate

// TSchematicDevice
// Create
constructor TSchematicDevice.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := frmLogicSim.pnlSchema;

  // Make the schematic device selected when first created.
  DeselectCurrentlySelected;
  CurrentlySelectedDevice := self;
  Selected := True;

  // Assign event handlers.
  OnPaint := @OnPaintHandler;
  OnMouseDown := @OnMouseDownHandler;
  OnMouseMove := @OnMouseMoveHandler;
end;

// DrawNode
//
// Draws the nodes as black ellipses on the schematic device's corresponding
// pins.
procedure TSchematicDevice.DrawNode(ANode: TNode);
begin
  with Canvas do
  begin
    Pen.Color := clBlack;
    Brush.Style := bsSolid;
    Brush.Color := clBlack;

    EllipseC(ANode.PositionRelativeToDevice.x + NODE_WIDTH div 2,
             ANode.PositionRelativeToDevice.y,
             NODE_WIDTH div 2, NODE_HEIGHT div 2);
  end;
end;

// Reset
procedure TSchematicDevice.Reset;
begin
  UpdateImage;
end;

// UpdateImage
procedure TSchematicDevice.UpdateImage;
begin

end;

// Update
procedure TSchematicDevice.UpdateDevice;
begin

end;

// OnPaintHandler
procedure TSchematicDevice.OnPaintHandler(Sender: TObject);
begin
  if Selected then
  begin
    with Canvas do
    begin
      Brush.Style := bsClear;
      Pen.Width := 1;
      Pen.Color := clSilver;
      Pen.Style := psDot;
      Rectangle(0, 0, self.Width, self.Height);
    end;
  end;
end;

// OnMouseDownHandler
procedure TSchematicDevice.OnMouseDownHandler(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X: longint; Y: longint);
begin
  if (ApplicationState = EDITING) and (CurrentTool = TOOL_MOUSE) then
  begin
    SelectDevice(self);
  end;
end;

// OnMouseMoveHandler
procedure TSchematicDevice.OnMouseMoveHandler(Sender: TObject;
  Shift: TShiftState; X: longint; Y: longint);
var
  CursorPositionRelativeToSchematic: TPoint;
begin
  if (ApplicationState = EDITING) and (CurrentTool = TOOL_MOUSE) then
  begin
    CursorPositionRelativeToSchematic :=
      frmLogicSim.pnlSchema.ScreenToClient(Mouse.CursorPos);

    if ssLeft in Shift then
    begin
      if SnapToGrid then
      begin
        // Snapping enabled.
        Left := ((CursorPositionRelativeToSchematic.x - Width div 2)
                 div GRID_SIZE) * GRID_SIZE;
        Top := ((CursorPositionRelativeToSchematic.y - Height div 2)
                 div GRID_SIZE) * GRID_SIZE;
      end
      else
      begin
        // Snapping disabled.
        Left := CursorPositionRelativeToSchematic.x - Width div 2;
        Top := CursorPositionRelativeToSchematic.y - Height div 2;
      end;
    end;
  end;

  RedrawAllWires;
end;
// END TSchematicDevice

end.
