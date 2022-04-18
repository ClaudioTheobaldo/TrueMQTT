program MQTTProtocol;

uses
  Vcl.Forms,
  uMQTTProtocol in 'uMQTTProtocol.pas' {frmMQTT};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMQTT, frmMQTT);
  Application.Run;
end.
