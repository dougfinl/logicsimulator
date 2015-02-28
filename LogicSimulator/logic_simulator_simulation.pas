unit logic_simulator_simulation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, Controls;

procedure StartSimulation;
procedure StopSimulation;
procedure SimulateOnce;


implementation

uses
  logic_simulator_form, logic_simulator_globals, logic_simulator_common;

var
  OldGateValues:     array of Boolean;
  CurrentGateValues: array of Boolean;

// SchematicDevicesChanged
//
// Returns TRUE if the output node of any schematic device has changed since the
// last call. Otherwise, returns FALSE.
function SchematicDevicesChanged: Boolean;
var
  i: Integer;
  Tmp: Pointer;
begin
  // Copy CurrentGateValues into OldGateValues.
  for i := 0 to Length(CurrentGateValues)-1 do
  begin
    OldGateValues[i] := CurrentGateValues[i];
  end;

  // Update CurrentGateValues with the current gate (output node) values.
  i := 0;
  for Tmp in GatesList do
  begin
    CurrentGateValues[i] := TLogicGate(Tmp).GetOutputNodeState;
    Inc(i);
  end;

  // Return TRUE if there is a difference between CurrentGateValues and
  // OldGateValues.
  Result := False;
  for i := 0 to Length(CurrentGateValues)-1 do
  begin
    if CurrentGateValues[i] <> OldGateValues[i] then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

// ResetSchematicDevices
//
// Calls the function 'Reset' on every device in the schematic.
procedure ResetSchematicDevices;
var
  Tmp: Pointer;
begin
  for Tmp in InputDevicesList do
  begin
    TInputDevice(Tmp).Reset;
  end;

  for Tmp in OutputDevicesList do
  begin
    TOutputDevice(Tmp).Reset;
  end;

  for Tmp in GatesList do
  begin
    TLogicGate(Tmp).Reset;
  end;
end;

// StartSimulation
//
// Disables all editing functionality, sets up variables and timers for use
// while simulating and executes an initial simulation pass.
procedure StartSimulation;
var
  Tmp: Pointer;
begin
  DeselectCurrentlySelected;
  SetToolType(TOOL_MOUSE);

  // Disable editing.
  with frmLogicSim do
  begin
    btnRun.Enabled := False;
    btnStop.Enabled := True;
    btnMouse.Enabled := False;
    btnWire.Enabled := False;
    scrollComponents.Hide;
  end;
  ApplicationState := SIMULATING;
  UpdateStatusBar('Simulating');

  ResetSchematicDevices;
  frmLogicSim.SimulationTimer.Enabled := True;
  SimulationStartTime := Now;

  // Change the size of OldGateValues and CurrentGateValues to accommodate every
  // gate in the circuit.
  SetLength(OldGateValues, GatesList.Count);
  SetLength(CurrentGateValues, GatesList.Count);

  // Set to true, to execute an initial simulation.
  InputDevicesChanged := True;

  // Trigger the input device buttons, to simulate some initial use of loopback
  // circuits such as latches.
  for Tmp in InputDevicesList do
  begin
    if TInputDevice(Tmp).GetInputDeviceType = ID_BUTTON then
    begin
      TInputDevice(Tmp).OnMouseDownHandler(Nil, mbLeft, [], 10, 10);

      // Sleep, to ensure that each device is triggered in its own complete
      // cycle of the simulation timer.
      Sleep(SIMULATION_INTERVAL*2);
    end;
  end;
end;

// StopSimulation
//
// Clears up any resources used for the simulation and enables editing of the
// schematic.
procedure StopSimulation;
begin
  ResetSchematicDevices;
  frmLogicSim.SimulationTimer.Enabled := False;

  // Enable editing.
  with frmLogicSim do
  begin
    btnRun.Enabled := True;
    btnStop.Enabled := False;
    btnMouse.Enabled := True;
    btnWire.Enabled := True;
    scrollComponents.Show;
  end;
  ApplicationState := EDITING;
  UpdateStatusBar('Edit mode');
end;

// SimulateOnce
//
// Updates all devices and passes signals between devices until the schematic is
// no longer changing or until LoopCounter exceeds 100.
//
// When the schematic is no longer changing, it can be assumed that the circuit
// is in a stable state and can be presented to the user.
//
// LoopCounter is used to prevent infinite loops, such as when loopback circuits
// are created (e.g. SR flip-flops).
procedure SimulateOnce;
var
  Tmp: Pointer;
  LoopCounter: Integer;
begin
  LoopCounter := 0;
  repeat
    for Tmp in WiresList do
    begin
      TWire(Tmp).AffectInputNode;
    end;

    for Tmp in GatesList do
    begin
      TLogicGate(Tmp).UpdateDevice;
    end;

    Inc(LoopCounter);
  until (not SchematicDevicesChanged) or (LoopCounter >= 100);

  RedrawAllWires;

  for Tmp in OutputDevicesList do
  begin
    TOutputDevice(Tmp).UpdateDevice;
  end;

  InputDevicesChanged := False;
end;

end.

