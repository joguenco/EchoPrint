program EchoPrint;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  ServerUnit;

type

  { TEchoPrint }

  TEchoPrint = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TEchoPrint }

  procedure TEchoPrint.DoRun;
  begin
    Server.Start;

    // stop program loop
    Terminate;
  end;

  constructor TEchoPrint.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
  end;

  destructor TEchoPrint.Destroy;
  begin
    inherited Destroy;
  end;

var
  Application: TEchoPrint;
begin
  Application := TEchoPrint.Create(nil);
  Application.Title := 'POS Print Server';
  Application.Run;
  Application.Free;
end.
