unit InfluxDB.Core;

interface

type

TInfluxClient  = class
private
  FServerURL : string;
  FToken : string;

public
  constructor Create(ServerURL, Token: string);
end;

implementation

constructor TInfluxClient.Create(ServerURL, Token: string);
begin
  inherited;
  // TODO -cMM: TInfluxClient.Create default body inserted
end;

end.
