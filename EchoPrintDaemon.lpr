program EchoPrintDaemon;

uses
  {$IFDEF UNIX}
  CThreads,
  {$ENDIF}
  DaemonApp,
  lazdaemonapp,
  DaemonMapperUnit,
  DaemonUnit,
  FileLoggerUnit { add your units here };

begin
  Application.Initialize;
  Application.Run;
end.
