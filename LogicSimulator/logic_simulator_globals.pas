// logic_simulator_globals.pas
//
// Contains declarations of application-wide global variables, and definitions
// of global constants and types.
unit logic_simulator_globals;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics;

type
  TApplicationState = (EDITING, SIMULATING, SAVING, LOADING);

const
  // Size of the grid squares (pixels).
  GRID_SIZE = 20;

  // The physical width and height of SchematicDevices' nodes (pixels).
  NODE_WIDTH = 8;
  NODE_HEIGHT = 6;

  // Colour of selected wire.
  WIRE_SELECTED_COLOUR = clSilver;

  // Wire colours for simulation.
  WIRE_HIGH_COLOUR = clRed;
  WIRE_LOW_COLOUR = clBlack;

  // Thickness of each wire (pixels).
  WIRE_THICKNESS = 2;

  // Index values for ilComponents image list.
  GATE_NOT_IMG_ID = 0;
  GATE_AND_IMG_ID = 1;
  GATE_OR_IMG_ID = 2;
  GATE_NAND_IMG_ID = 3;
  GATE_NOR_IMG_ID = 4;
  GATE_XOR_IMG_ID = 5;
  GATE_XNOR_IMG_ID = 6;
  OUTPUT_INDICATOR_OFF_IMG_ID = 7;
  OUTPUT_INDICATOR_ON_IMG_ID = 8;
  BUTTON_RELEASED_IMG_ID = 9;
  BUTTON_PRESSED_IMG_ID = 10;
  SWITCH_RELEASED_IMG_ID = 11;
  SWITCH_PRESSED_IMG_ID = 12;

  // Time between each simulation pass (milliseconds).
  SIMULATION_INTERVAL = 10;

  // Time for buttons to stay pressed (milliseconds).
  SIMULATION_BUTTON_TIME = 1000;

var
  // Current state of the application.
  ApplicationState: TApplicationState;

  // Holds whether the schematic has been changed since the last save. Used when
  // saving, loading and creating a new file.
  SchematicChanged: Boolean;

  // Devices will snap to the grid if set to TRUE.
  SnapToGrid: Boolean;

  // Lists holding every schematic object created.
  GatesList, InputDevicesList, OutputDevicesList, WiresList: TList;

  // Time at which the simulation started.
  SimulationStartTime: TDateTime;

  // Have any of the input devices been clicked?
  InputDevicesChanged: Boolean;

implementation


end.

