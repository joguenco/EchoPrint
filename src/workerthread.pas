unit WorkerThread;

// --------------------------------------
// Common file: the "worker" thread
// V1.1 3/2022 arminlinder@arminlinder.de
// --------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ServerUnit;

  // --------------------------------------------------------------------------------
  // This is the "workhorse" of the daemon, and just a normal therad, see the Lazarus
  // docs about threads and mutlitasking for details.

  // Execute holds the main work code of the service
  // Do not try the execute method of TDaemon, since it does not multitask,
  // the service thread will stop responding to control messages if looping in the
  // TDaemon execute method. Thus we need a worker thread.
  // --------------------------------------------------------------------------------

type
  TWorkerThread = class(TThread)
  private
    FPort: integer;
  public
    procedure Execute; override;  // the actual worker thread code goes here
    constructor Create(port: integer);
    destructor Destroy; override;
  end;

implementation

uses FileLoggerUnit;

  // --------------------------------------------------------------------------
  // TThread.execute: this is the core of the workhorse, the routine which does
  // actually hold the service's working code
  // --------------------------------------------------------------------------

procedure TWorkerThread.Execute;
begin
  LogToFile('Daemon worker thread executing');
  Server.Start(FPort);
end;

// -------------------------------------------------
// Construction and destruction of the worker thread
// -------------------------------------------------

constructor TWorkerThread.Create(port: integer);
begin
  // Create a suspended worker thread to allow further parametrizon before it
  // does actually start
  // The thread will be started if the OS sends a "Start" Signal to TDaemon
  // See OnStart event handler of the TDeamon class
  inherited Create(True);
  FPort := port;
  LogToFile('Daemon worker thread created');
end;

destructor TWorkerThread.Destroy;
begin
  // Nothing to do here, just for logging
  Server.Stop;
  LogToFile('Daemon worker thread destroyed');
  inherited Destroy;
end;

end.
