unit EchoPrintUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, WorkerThread, fpjson, jsonparser, fphttpclient, IniFiles, LCLIntf,
  Crt;

type

  { TEchoPrintForm }

  TEchoPrintForm = class(TForm)
    ButHide: TButton;
    ButPing: TButton;
    lblLinkGitHub: TLabel;
    lblMessageServer: TLabel;
    TrayIcon: TTrayIcon;
    procedure ButHideClick(Sender: TObject);
    procedure ButPingClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lblLinkGitHubClick(Sender: TObject);
    procedure lblLinkGitHubMouseEnter(Sender: TObject);
    procedure lblLinkGitHubMouseLeave(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
    FWorkerThread: TWorkerThread;
    FPort: integer;
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

procedure TEchoPrintForm.ButPingClick(Sender: TObject);
var
  httpClient: TFPHTTPClient;
  statusCode: integer;
  response: string;
  jObj: TJSONObject;
begin
  httpClient := TFPHTTPClient.Create(nil);
  try
    try
      response := httpClient.Get('http://localhost:' + IntToStr(FPort) + '/ping');
      statusCode := httpClient.ResponseStatusCode;
      if statusCode = 200 then
      begin
        jObj := GetJSON(response) as TJSONObject;
        MessageDlg('', jObj.Strings['message'], mtInformation, [], 0);
      end;
    except
      on E: Exception do
      begin
        MessageDlg('', E.Message, mtError, [], 0);
      end;
    end;
  finally
    httpClient.Free
  end;
end;

procedure TEchoPrintForm.FormActivate(Sender: TObject);
begin
  Delay(1800);
  EchoPrintForm.Hide;
  TrayIcon.Visible := True;
end;

procedure TEchoPrintForm.FormCreate(Sender: TObject);
var
  appINI: TIniFile;
begin
  appINI := TIniFile.Create(ChangeFileExt('config', '.ini'));
  FPort := appINI.ReadInteger('Server', 'Port', 9090);

  TrayIcon.Visible := False;
  FWorkerThread := TWorkerThread.Create(FPort);
  // Parametrize it
  FWorkerThread.FreeOnTerminate := False;
  // Start the worker
  FWorkerThread.Start;

  lblMessageServer.Caption := lblMessageServer.Caption + ' ' + IntToStr(FPort);
end;

procedure TEchoPrintForm.FormDestroy(Sender: TObject);
begin
  FWorkerThread.Terminate;
end;

procedure TEchoPrintForm.lblLinkGitHubClick(Sender: TObject);
begin
  OpenURL(lblLinkGitHub.Caption);
end;

procedure TEchoPrintForm.lblLinkGitHubMouseEnter(Sender: TObject);
begin
  lblLinkGitHub.Cursor := crHandPoint;
  lblLinkGitHub.Font.Underline := True;
end;

procedure TEchoPrintForm.lblLinkGitHubMouseLeave(Sender: TObject);
begin
  lblLinkGitHub.Cursor := crDefault;
  lblLinkGitHub.Font.Underline := False;
end;

procedure TEchoPrintForm.TrayIconDblClick(Sender: TObject);
begin
  TrayIcon.Visible := False;
  EchoPrintForm.Show;
end;

end.
