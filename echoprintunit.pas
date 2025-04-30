unit EchoPrintUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  WorkerThread;

type

  { TEchoPrintForm }

  TEchoPrintForm = class(TForm)
    ButHide: TButton;
    TrayIcon: TTrayIcon;
    procedure ButHideClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
        FDaemonWorkerThread: TWorkerThread;
  public

  end;

var
  EchoPrintForm: TEchoPrintForm;

implementation

{$R *.lfm}

{ TEchoPrintForm }

procedure TEchoPrintForm.ButHideClick(Sender: TObject);
begin
  EchoPrintForm.Hide;
  TrayIcon.Visible := True;
end;

procedure TEchoPrintForm.FormCreate(Sender: TObject);
begin
  TrayIcon.Visible := False;
   FDaemonWorkerThread := TWorkerThread.Create;
  // Parametrize it
  FDaemonWorkerThread.FreeOnTerminate := False;
  // Start the worker
  FDaemonWorkerThread.Start;
end;

procedure TEchoPrintForm.TrayIconDblClick(Sender: TObject);
begin
  TrayIcon.Visible := False;
  EchoPrintForm.Show;
end;

end.
