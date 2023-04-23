unit InfluxDB2;

interface
uses InfluxDB.Types,
     InfluxDB.Interfaces,
     InfluxDB.Core;

type

  TInfluxDBV2 = class(TInfluxClient, IInfluxClient)
  private
    function DefaultOptions: TWriteOptions;
  public
    constructor Create(AServerURL: String; AToken : string);
    class function NewClient(ServerURL: String; Token : string): IInfluxClient;
    class function NewClientWithOptions(ServerURL: String; Token : string; AOptions : TWriteOptions): IInfluxClient;
    class function NewPointWithMeasurement( Measurement : string ) : IInfluxPoint;


  end;


implementation

{ TInfluxDBClient }

constructor TInfluxDBV2.Create(AServerURL: String; AToken : string);
begin
  inherited Create(AServerURL, AToken, DefaultOptions());
end;

function TInfluxDBV2.DefaultOptions: TWriteOptions;
begin
  Result := TWriteOptions.Create;
  Result.batchSize:= 5000;
  Result.flushInterval:= 1000;
  Result.precision := pDefaultNanoSeconds;
  Result.useGZip := false;
  Result.retryBufferLimit:= 50000;
  Result.maxRetries:= 5;
  Result.retryInterval:= 5000;
  Result.maxRetryInterval:= 125000;
  Result.maxRetryTime:= 180000;
  Result.exponentialBase:= 2;
end;

class function TInfluxDBV2.NewClient(ServerURL: String; Token: string): IInfluxClient;
begin
  Result := TInfluxDBV2.Create(ServerURL, Token);
end;


class function TInfluxDBV2.NewClientWithOptions(ServerURL, Token: string;
  AOptions: TWriteOptions): IInfluxClient;
begin
 inherited Create(ServerURL, Token, AOptions);
end;

class function TInfluxDBV2.NewPointWithMeasurement(
  Measurement: string): IInfluxPoint;
begin
    Result := TInfluxPoint.Create(Measurement);
end;

end.
