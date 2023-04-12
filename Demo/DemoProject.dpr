program DemoProject;

uses
  Vcl.Forms,
  Demo.Main in 'Demo.Main.pas' {Form1},
  influxDB2 in '..\Source\influxDB2.pas',
  InfluxDB.Types in '..\Source\InfluxDB.Types.pas',
  InfluxDB.Interfaces in '..\Source\InfluxDB.Interfaces.pas',
  influxDB.Core in '..\Source\influxDB.Core.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
