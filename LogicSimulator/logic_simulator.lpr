program logic_simulator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, logic_simulator_form, logic_simulator_common, logic_simulator_globals,
  logic_simulator_simulation;

{$R *.res}

begin
  Application.Title:='Logic Simulator';
  Application.Initialize;
  Application.CreateForm(TfrmLogicSim, frmLogicSim);
  Application.Run;
end.

