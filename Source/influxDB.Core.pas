unit influxDB.Core;

interface
uses InfluxDB.Types,
     InfluxDB.Interfaces,
     System.Classes;


type
  TRequestMethod = (GET, POST, PUT, DELETE, PATCH);

const
  APIEndpoint = '/api/v2';
  HTTP_USER_AGENT_V2 = 'InfluxDB Client 2.0';

type

  TWriteAPI  =  class(TInterfacedObject, IWriteAPI)
  private
    FServerURL    : string;
    FOrganisation : string;
    FBucket       : string;
    FToken        : string;
    FPrecision    : TPointPrecision;
    FOptions      : TWriteOptions;
    function GetAuthorizationHeader: string;
    function Write(Line: string): TError;
  public
    constructor Create(AServerURL : string; AToken : string; AOrganisation : string; ABucket : string; APrecision : TPointPrecision; AOptions : TWriteOptions);
    // WriteRecord writes asynchronously line protocol record into bucket.
    // WriteRecord adds record into the buffer which is sent on the background when it reaches the batch size.
    // Blocking alternative is available in the WriteAPIBlocking interface
    function WriteRecord(line : string) : TError;
    // WritePoint writes asynchronously Point into bucket.
    // WritePoint adds Point into the buffer which is sent on the background when it reaches the batch size.
    // Blocking alternative is available in the WriteAPIBlocking interface
    function WritePoint(point : IInfluxPoint) : TError;
    procedure AddDefaultTags(Key, Value : string);
  end;


type

TInfluxClient  = class( TInterfacedObject )
private
  FServerURL : string;
  FToken : string;
  FWriteOptions : TWriteOptions;
  function GetAuthorizationHeader : string;

public
  constructor Create(ServerURL, Token: string; AOptions : TWriteOptions);

  function WriteAPIBlocking(AOrganisation : string; ABucket : string ) : IWriteAPI;

  // IInlfuxClient
  function Ready() : TReady;
  function Health() : THealthCheck;
  function Ping(): boolean;
  procedure Close();
  function ServerURL() : string;

  property WriteOptions : TWriteOptions read FWriteOptions write FWriteOptions;
end;




implementation
uses System.JSON, REST.Client, REST.Types,system.Rtti,REST.Json, System.SysUtils;

procedure TInfluxClient.Close;
begin

end;

constructor TInfluxClient.Create(ServerURL, Token: string; AOptions : TWriteOptions);
begin
  FServerURL := ServerURL;
  FToken := Token;
  FWriteOptions := AOptions;
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

function TInfluxClient.WriteAPIBlocking(AOrganisation,
  ABucket: string): IWriteAPI;
begin
  Result := TWriteAPI.Create(FServerURL,FToken, AOrganisation, ABucket, FWriteOptions.precision, FWriteOptions);
end;

{ TWriteAPI }

procedure TWriteAPI.AddDefaultTags(Key, Value: string);
begin
  if not Assigned(FOptions) then
    Exit;

  FOptions.defaultTags.Add(Key,Value);
end;

constructor TWriteAPI.Create(AServerURL : string; AToken : string; AOrganisation : string; ABucket : string; APrecision : TPointPrecision; AOptions : TWriteOptions);
begin
  inherited Create;
  FServerURL := AServerURL;
  FToken := AToken;
  FOrganisation := AOrganisation;
  FBucket := ABucket;
  FPrecision := APrecision;
  FOptions  := AOptions;
end;

function TWriteAPI.GetAuthorizationHeader: string;
begin
  Result := 'Token '+FToken;
end;

function TWriteAPI.Write( Line : string ) :TError;
var
  LClient: TRESTClient;
  LRequest: TRESTRequest;
  LResponse: TRESTResponse;
  LPoint: TJSONObject;
  LPoints: TJSONArray;
begin
  Result := NewError(); // Create default TError
  LClient := TRESTClient.Create(nil);
  try
    LClient.BaseURL := FServerURL + APIEndpoint + '/write';

    LRequest := TRESTRequest.Create(nil);
    try
      LRequest.Client := LClient;
      LRequest.Method := TRESTRequestMethod.rmPOST;
      LRequest.Accept :='application/json';

      LRequest.Resource := '?org={org}&bucket={bucket}&precision={precision}';
      LRequest.AddParameter('org',FOrganisation,pkURLSEGMENT);
      LRequest.AddParameter('bucket',FBucket,pkURLSEGMENT);
      LRequest.AddParameter('precision', PointPrecisionToString(FPrecision), TRESTRequestParameterKind.pkURLSEGMENT);
      LRequest.AddParameter('Content-Type','text/plain',pkHTTPHEADER) ;
      LRequest.AddParameter('Authorization', GetAuthorizationHeader, pkHTTPHEADER, [poDoNotEncode]);

      LRequest.Body.Add(Line+' aa ',ctTEXT_PLAIN);  // Add data formatted as Line Protocol


      LResponse := TRESTResponse.Create(nil);
      LRequest.Response := LResponse;
      try
        try
          LRequest.Execute;
          if ((LResponse.StatusCode >= 200 ) and (LResponse.StatusCode<=300)) then
          begin
            Result.Err := False; // No error
          end
          else
          begin
            Result.Err := True;
            Result.StatusCode := LResponse.StatusCode;
            if LResponse.ContentType = 'application/json' then
            begin
              	Result.Code := LResponse.StatusText;
                Result.Message := LResponse.Content;
            end
          end;

        except
          On E : Exception do
          begin
            Result.Err := True;
            Result.Message := E.Message;
            Result.StatusCode := -1;
            Result.Code := '';

          end;

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

function TWriteAPI.WritePoint(point: IInfluxPoint): TError;
begin
  result := Write(point.PointToLineProtocol(FPrecision));
end;

function TWriteAPI.WriteRecord(line: string): TError;
begin
  result := Write(line);
end;

end.
