unit Demo.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    btnClientFunctions: TButton;
    edtServerURL: TEdit;
    Label1: TLabel;
    s: TLabel;
    edtToken: TEdit;
    Memo1: TMemo;
    btnWriteExample: TButton;
    procedure btnClientFunctionsClick(Sender: TObject);
    procedure btnWriteExampleClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses  influxDB2,
      InfluxDB.Interfaces,
      InfluxDB.Types ,
      system.DateUtils,
      System.Diagnostics,
      system.Generics.Collections,
      system.Rtti;

{$R *.dfm}

procedure TForm1.btnClientFunctionsClick(Sender: TObject);
var client : IInfluxClient;
    health : THealthCheck;
    ready : TReady;
    procedure Log(text : string ); overload;
    begin
      memo1.Lines.Add(text);
    end;
    procedure Log(Pair,Value : string ); overload;
    begin
      memo1.Lines.AddPair(Pair,Value);
    end;
begin
  client := TInfluxDBV2.NewClient(edtServerURL.Text, edtToken.Text);

  Memo1.Lines.Text := '';

  Log('-- Ping --');
  if client.Ping() then
    Log('Ping OK')
  else
    Log('Ping Error');

  Log('-- Health --');
  health := client.Health();
  if Assigned(health) then
  begin
    Log('Health.Message',health.Message);
    Log('Health.Name',health.Name);
    Log('Health.status',health.Status);
    Log('Health.version',health.version);
    Log('Health.commit',health.commit);
  end
  else
    Log('Health error');

  Log('-- Ready --');
  ready := client.Ready();
  if Assigned(health) then
  begin
    Log('Ready.Status',ready.Status);
    Log('Ready.Up',ready.Up);
    Log('Ready.Started',FormatDateTime('YYYY-MM-DD HH:NN:SS', Ready.Started));
  end
  else
    Log('Ready error');

end;


procedure TForm1.btnWriteExampleClick(Sender: TObject);
var p : IInfluxPoint;
    client : IInfluxClient;
    writeAPI : IWriteAPI;
    val1 : double;
    val2 : int64;
    i:integer;
    starttime : TStopwatch;
    point1,point2 : IInfluxPoint;
    error : TError;
    procedure Log(text : string ); overload;
    begin
      memo1.Lines.Add(text);
    end;
begin
  starttime := TStopWatch.StartNew;
  randomize;
  //
  client := TInfluxDBV2.NewClient(edtServerURL.Text, edtToken.Text);
  writeAPI := client.WriteAPIBlocking('org1','buck1');
  // setup default tags for all writes through this API
  writeApi.AddDefaultTags('location','hostname');
  // write point with the current (client-side) timestamp
  point1 := newPoint('temperature',
                      [TPair<string,string>.Create('example','exp')],
                      [TPair<string,TValue>.Create('value',20 + round(100 * random(1000) / 10) )],
                      now()
                      );


  error := writeApi.writePoint(point1);
  if error.Err then
  begin
    Log('Write Error '+IntToStr(error.StatusCode ) + ' ' + error.Code  +' ' + error.Message);
  end
  else
    Log('Write Ok');
  point1:= nil;

  point2 := NewPointWithMeasurement('temperature')
  .AddTag('example', 'exp')
  .AddField('value', 10 + round(100 * random(1000) / 10))
  .SetTime(Now()); // can be also a number, but in writeApi's precision units (s, ms, us, ns)!
  error := writeApi.writePoint(point2);
  if error.Err then
  begin
    Log('Write Error '+IntToStr(error.StatusCode ) + ' '  + error.Code  +' ' + error.Message);
  end
  else
    Log('Write Ok');
  point2:= nil;

  starttime.Stop;
  writeAPI := nil;
  client := nil;
  ShowMessage('Done in '+starttime.Elapsed);


end;

end.
