(*
  REST API Server
  Jorge Luis
  jorgeluis@resolvedor.dev
  https://resolvedor.dev
  2025
*)

unit ServerUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Interfaces, fphttpapp, HTTPDefs, httproute,
  fpjson, httpprotocol, PrinterUnit;

type

  { TServer }

  TServer = class
  public
    procedure Start;
  private
    procedure Ping(Request: TRequest; Response: TResponse);
    procedure CatchAll(Request: TRequest; Response: TResponse);
    procedure TextArray(Request: TRequest; Response: TResponse);
    procedure BeforeRequestHandler(Sender: TObject; Request: TRequest;
      Response: TResponse);
  end;

var
  Server: TServer;

implementation

procedure TServer.Start;
begin
  (*
  A CORS preflight request is a CORS request that checks to see if the CORS protocol
  is understood and a server is aware using specific methods and headers.

  It is an OPTIONS request, using two or three HTTP request headers:
  Access-Control-Request-Method, Origin, and optionally Access-Control-Request-Headers.

  For CORS error:
  has been blocked by CORS policy:
  Response to preflight request doesn't pass access control check:
  It does not have HTTP ok status

  HTTPRouter.BeforeRequest := @BeforeRequestHandler;
  *)
  HTTPRouter.BeforeRequest := @BeforeRequestHandler;
  HTTPRouter.RegisterRoute('/ping', rmGet, @Ping);
  HTTPRouter.RegisterRoute('/catchall', rmAll, @CatchAll, True);
  HTTPRouter.RegisterRoute('/print', rmPost, @TextArray);
  Application.Port := 9090;
  //Application.Threaded := True;
  Application.Initialize;

  WriteLn('[INFO] Server is ready at localhost: ' + IntToStr(Application.Port));
  Application.Run;
end;

procedure TServer.Ping(Request: TRequest; Response: TResponse);
begin
  try
    PrinterPos.Ping;
    Response.Content := '{"message":"Pong"}';
    Response.Code := 200;
  except
    on E: Exception do
    begin
      WriteLn('[ERROR] ' + E.Message);
      Response.Content := '{"message":"' + E.Message + '"}';
      Response.Code := 500;
    end;
  end;
  Response.SetCustomHeader('Access-Control-Allow-Origin', '*');
  Response.SetCustomHeader('Access-Control-Allow-Headers',
    'Content-Type, Authorization');
  Response.SetCustomHeader('Access-Control-Allow-Methods', 'POST, GET');
  Response.SetCustomHeader('Connection', 'Keep-Alive');
  Response.ContentType := 'application/json';
  Response.ContentLength := Length(Response.Content);
  Response.SendContent;
  WriteLn('[INFO] Request GET to /ping and response status: ' + IntToStr(Response.Code));
end;

procedure TServer.TextArray(Request: TRequest; Response: TResponse);
var
  jObject: TJSONObject;
begin
  try
    jObject := GetJSON(Request.Content) as TJSONObject;
    PrinterPos.ArrayWriter(jObject);

    Response.Content := '{"message":"Printed"}';
    Response.Code := 200;
  except
    on E: Exception do
    begin
      WriteLn('[ERROR] ' + E.Message);
      Response.Content := '{"message":"' + E.Message + '"}';
      Response.Code := 500;
    end;
  end;
  Response.SetCustomHeader('Access-Control-Allow-Origin', '*');
  Response.SetCustomHeader('Access-Control-Allow-Headers',
    'Content-Type, Authorization, X-Requested-With');
  Response.SetCustomHeader('Access-Control-Allow-Methods', 'POST, GET');
  Response.SetCustomHeader('Connection', 'Keep-Alive');
  Response.ContentType := 'application/json';
  Response.ContentLength := Length(Response.Content);
  Response.SendContent;
  WriteLn('[INFO] Request POST to /print/v1/text/array and response status: ' +
    IntToStr(Response.Code));
end;

procedure TServer.BeforeRequestHandler(Sender: TObject; Request: TRequest;
  Response: TResponse);
begin
  Response.SetCustomHeader('Access-Control-Allow-Origin', '*');
  Response.SetCustomHeader('Access-Control-Allow-Methods',
    'GET, POST, PUT, DELETE, OPTIONS');
  Response.SetCustomHeader('Access-Control-Allow-Headers',
    'Content-Type, Accept, Accept-Language, Accept-Encoding');

  if Request.Method = 'OPTIONS' then
  begin
    Response.Code := 200;
    Response.SendResponse;
    Exit;
  end;
end;

procedure TServer.CatchAll(Request: TRequest; Response: TResponse);
begin
  Response.Content := '{"message":"This endpoint is not available"}';
  Response.Code := 404;
  Response.SetCustomHeader('Access-Control-Allow-Origin', '*');
  Response.SetCustomHeader('Access-Control-Allow-Headers',
    'Content-Type, Authorization');
  Response.SetCustomHeader('Access-Control-Allow-Methods', 'POST, GET');
  Response.SetCustomHeader('Connection', 'Keep-Alive');
  Response.ContentType := 'application/json';
  Response.ContentLength := Length(Response.Content);
  Response.SendContent;
  WriteLn('[WARN] Resource not available and response status: ' +
    IntToStr(Response.Code));
end;

end.
