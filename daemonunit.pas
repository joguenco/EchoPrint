unit DaemonUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DaemonApp, DaemonWorkerThread,
  {$IFDEF UNIX}DaemonSystemdInstallerUnit,{$ENDIF}// Unit not required on Windows
  LazFileUtils;

type
  { TDaemon1 }

  TDaemon1 = class(TDaemon)
    procedure DataModuleAfterInstall(Sender: TCustomDaemon);
    procedure DataModuleBeforeUnInstall(Sender: TCustomDaemon);
    procedure DataModuleShutDown(Sender: TCustomDaemon);
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: boolean);
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: boolean);

  private
    FDaemonWorkerThread: TDaemonWorkerThread;
  public

  end;

  // Added by scaffolded code, not required
  // var
  //Daemon1: TDaemon1;

implementation

{$R *.lfm}

uses FileLoggerUnit;

  { TDaemon1 }



procedure TDaemon1.DataModuleAfterInstall(Sender: TCustomDaemon);
var
  isInstalled: boolean = True;
  {$IFDEF UNIX}
FilePath:String;
  {$ENDIF}
begin
  {$IFDEF UNIX}
FilePath := GetSystemdControlFilePath(Self.Definition.Name);
LogToFile('Daemon installing systemd control file: ' + FilePath);
isInstalled := CreateSystemdControlFile(self, FilePath);
if not isInstalled then
  LogToFile('Error creating systemd control file');
  {$ENDIF}
  if isInstalled then
    LogToFile('Daemon installed');
end;

procedure TDaemon1.DataModuleBeforeUnInstall(Sender: TCustomDaemon);
var
  isUnInstalled: boolean = True;
  {$IFDEF UNIX}
FilePath: string;
  {$ENDIF}
begin
  {$IFDEF UNIX}
  FilePath := GetSystemdControlFilePath(Self.Definition.Name);
  LogToFile('Daemon uninstalling systemd control file: ' + FilePath);
  isUnInstalled := RemoveSystemdControlFile(FilePath);
  if not isUninstalled then
    LogToFile('Error removing systemd control file');
  {$ENDIF}
  if isUninstalled then
    LogToFile('Daemon uninstalled');
end;

procedure TDaemon1.DataModuleShutDown(Sender: TCustomDaemon);
begin
  self.Stop;   // On shutdown, we trigger the stop handler. This will do nicely for this demo
  LogToFile('Daemon received shutdown signal');
end;

procedure TDaemon1.DataModuleStart(Sender: TCustomDaemon; var OK: boolean);
begin
  LogToFile(Format('Daemon received start signal, PID:%d', [GetProcessID]));
  // Create a suspended worker thread - see DaemonWorkerThread unit
  FDaemonWorkerThread := TDaemonWorkerThread.Create;
  // Parametrize it
  FDaemonWorkerThread.FreeOnTerminate := False;
  // Start the worker
  FDaemonWorkerThread.Start;
  OK := True;
end;

procedure TDaemon1.DataModuleStop(Sender: TCustomDaemon; var OK: boolean);
begin
  LogToFile('Daemon received stop signal');
  // stop and terminate the worker
  if assigned(FDaemonWorkerThread) then
  begin
    FDaemonWorkerThread.Terminate;
    // Wait for the thread to terminate.
    FDaemonWorkerThread.WaitFor;
    FreeAndNil(FDaemonWorkerThread);
  end;
  LogToFile('Daemon stopped');
  OK := True;
end;

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TDaemon1);
end;

initialization
  RegisterDaemon;
end.
