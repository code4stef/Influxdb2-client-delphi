unit influxDB.Core;

interface
uses InfluxDB.Types,
     System.Classes;


type
  TRequestMethod = (GET, POST, PUT, DELETE, PATCH);

const
  APIEndpoint = '/api/v2';
  HTTP_USER_AGENT_V2 = 'InfluxDB Client 2.0';

type

TInfluxClient  = class( TInterfacedObject )
private
  FServerURL : string;
  FToken : string;
  function GetAuthorizationHeader : string;

public
  constructor Create(ServerURL, Token: string);

  function Ready() : TReady;
  function Health() : THealthCheck;
  function Ping(): boolean;
  procedure Close();
  function ServerURL() : string;
end;


implementation
uses System.JSON, REST.Client, REST.Types,system.Rtti,REST.Json;

procedure TInfluxClient.Close;
begin

end;

constructor TInfluxClient.Create(ServerURL, Token: string);
begin
  FServerURL := ServerURL;
  FToken := Token;
end;

function TInfluxClient.GetAuthorizationHeader: string;
begin
  Result := 'Token '+FToken;
end;

function TInfluxClient.Health: THealthCheck;
var
  LClient: TRESTClient;
  LRequest: TRESTRequest;
  LResponse: TRESTResponse;
  res : THealthCheck;
begin
  Result := res;
  LClient := TRESTClient.Create(nil);
  try
    LClient.BaseURL := FServerURL + '/health';

    LRequest := TRESTRequest.Create(nil);
    try
      LRequest.Client := LClient;
      LRequest.Method := TRESTRequestMethod.rmGET;
      LRequest.Accept :='application/json';

      LRequest.Params.AddItem('Authorization', GetAuthorizationHeader, pkHTTPHEADER, [poDoNotEncode]);
      LResponse := TRESTResponse.Create(nil);
      LRequest.Response := LResponse;
      try
        LRequest.Execute;
        if LResponse.StatusCode = 200 then
        begin
          Result := TJson.JsonToObject<THealthCheck>(LResponse.JSONText);
        end;
      finally
        LResponse.Free;
      end;

    finally
      LRequest.Free;
    end;
  finally
    LClient.Free;
  end;
end;

function TInfluxClient.Ping: boolean;
var
  LClient: TRESTClient;
  LRequest: TRESTRequest;
  LResponse: TRESTResponse;
  LPoint: TJSONObject;
  LPoints: TJSONArray;
begin
  Result := False;
  LClient := TRESTClient.Create(nil);
  try
    LClient.BaseURL := FServerURL + '/ping';

    LRequest := TRESTRequest.Create(nil);
    try
      LRequest.Client := LClient;
      LRequest.Method := TRESTRequestMethod.rmGET;
      LRequest.Accept :='application/json';

      LRequest.Params.AddItem('Authorization', GetAuthorizationHeader, pkHTTPHEADER, [poDoNotEncode]);
      LResponse := TRESTResponse.Create(nil);
      LRequest.Response := LResponse;
      try
        LRequest.Execute;
        if LResponse.StatusCode = 204 then
        begin
          Result := True;
        end;
      finally
        LResponse.Free;
      end;

    finally
      LRequest.Free;
    end;
  finally
    LClient.Free;
  end;

end;

function TInfluxClient.Ready: TReady;
var
  LClient: TRESTClient;
  LRequest: TRESTRequest;
  LResponse: TRESTResponse;
  res : TReady;
begin
  Result := res;
  LClient := TRESTClient.Create(nil);
  try
    LClient.BaseURL := FServerURL + '/ready';

    LRequest := TRESTRequest.Create(nil);
    try
      LRequest.Client := LClient;
      LRequest.Method := TRESTRequestMethod.rmGET;
      LRequest.Accept :='application/json';

      LRequest.Params.AddItem('Authorization', GetAuthorizationHeader, pkHTTPHEADER, [poDoNotEncode]);
      LResponse := TRESTResponse.Create(nil);
      LRequest.Response := LResponse;
      try
        LRequest.Execute;
        if LResponse.StatusCode = 200 then
        begin
          Result := TJson.JsonToObject<TReady>(LResponse.JSONText);
        end;
      finally
        LResponse.Free;
      end;

    finally
      LRequest.Free;
    end;
  finally
    LClient.Free;
  end;
end;

function TInfluxClient.ServerURL: string;
begin
  Result := FServerURL;
end;

end.
