unit InfluxDB2;

interface
uses InfluxDB.Types,
     InfluxDB.Interfaces,
     InfluxDB.Core;

type

  TInfluxDBV2 = class(TInfluxClient, IInfluxClient)
  public
    constructor Create(AServerURL: String; AToken : string);
    class function NewClient(ServerURL: String; Token : string): IInfluxClient;
  end;


implementation

{ TInfluxDBClient }

constructor TInfluxDBV2.Create(AServerURL: String; AToken : string);
begin
  inherited Create(AServerURL, AToken);
end;

class function TInfluxDBV2.NewClient(ServerURL: String; Token: string): IInfluxClient;
begin
  Result := TInfluxDBV2.Create(ServerURL, Token);
end;


end.
