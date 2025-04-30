unit EchoPrintUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TEchoPrintForm }

  TEchoPrintForm = class(TForm)
    ButHide: TButton;
    TrayIcon: TTrayIcon;
    procedure ButHideClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private

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
end;

procedure TEchoPrintForm.TrayIconDblClick(Sender: TObject);
begin
  TrayIcon.Visible := False;
  EchoPrintForm.Show;
end;

end.
