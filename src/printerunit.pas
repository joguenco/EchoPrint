(*
  Print in POS printer, the printer must be set as  default
  Jorge Luis
  jorgeluis@resolvedor.dev
  https://resolvedor.dev
  2025
*)
unit PrinterUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Interfaces, Printers, OSPrinters, fpjson, RUtils, FileLoggerUnit;

type
  TCommand = (OpenCashDrawer, CutPaper);
  { TPrinterPos }

  TPrinterPos = class
  private
    procedure WriteString(S: string);
    procedure EscPosCommand(command: TCommand);
    procedure Footer;
  public
    procedure Ping;
    procedure ArrayWriter(jObj: TJSONObject);
  end;

var
  PrinterPos: TPrinterPos;

implementation

procedure TPrinterPos.WriteString(S: string);
var
  Written: integer;
begin
  Printer.Write(S[1], Length(S), Written);
end;

procedure TPrinterPos.Ping;
begin
  with Printer do
  try
    RawMode := True;
    BeginDoc;
    EscPosCommand(OpenCashDrawer);
    WriteString('Pong' + LineEnding);
  finally
    Footer;
    EscPosCommand(CutPaper);
    EndDoc;
  end;

end;

procedure TPrinterPos.ArrayWriter(jObj: TJSONObject);
var
  jArray: TJSONArray;
  jEnum: TJSONEnum;
  jLine: TJSONObject;
begin
  jArray := jObj.Arrays['lines'];
  with Printer do
  try
    RawMode := True;
    BeginDoc;
    EscPosCommand(OpenCashDrawer);
    for jEnum in jArray do
    begin
      jLine := jEnum.Value as TJSONObject;
      WriteString(RemoveDiacritics(jLine.Strings['line'] + LineEnding));
    end;
  finally
    Footer;
    EscPosCommand(CutPaper);
    EndDoc;
  end;
end;

procedure TPrinterPos.EscPosCommand(command: TCommand);
const
  fileCutPaper: string = 'cutpaper.txt';
  fileOpenCashDrawer: string = 'opencashdrawer.txt';
var
  fileCommand: string = '';
  prnfile: System.Text;
  buffer: string;
begin

  case (command) of
    CutPaper: fileCommand := fileCutPaper;
    OpenCashDrawer: fileCommand := fileOpenCashDrawer;
  end;

  try
    try
      AssignFile(prnfile, fileCommand);
      Reset(prnfile);

      while not EOF(prnfile) do
      begin
        Readln(prnfile, buffer);
        WriteString(buffer);
      end;
    except
      on E: Exception do
      begin
        LogToFile('[ERROR] ' + fileCommand + ' ' + E.Message);
      end;
    end;
  finally
    CloseFile(prnfile);
  end;
end;

procedure TPrinterPos.Footer;
var
  i: integer;
begin
  for i := 1 to 6 do
    WriteString(LineEnding);
end;

end.
