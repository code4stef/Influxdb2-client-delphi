unit Demo.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    edtServerURL: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtToken: TEdit;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
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
      InfluxDB.Types;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
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

end.
