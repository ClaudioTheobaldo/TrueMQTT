unit MQTT.DefaultClientBuilder;

interface

uses
  MQTT.Client;

type
  TSocketType = (stOverbyte, stIndy);

  TMQTTDefaultClientBuilder = class
    class function BuildDefaultClient(pSocketType: TSocketType;
      pKeepAliveSecondsInterval: UInt16 = 10): TMQTTClient;
  end;

implementation

uses
  MQTT.ISocket, MQTT.DefaultTimer,
  MQTT.OverbyteSocket, MQTT.IndySocket,
  MQTT.V311PacketBuilder, MQTT.V311Parser;

{ TMQTTDefaultClientBuilder }

class function TMQTTDefaultClientBuilder.BuildDefaultClient(
  pSocketType: TSocketType; pKeepAliveSecondsInterval: UInt16): TMQTTClient;
var
  vSocket: IMQTTSocket;
begin
  case pSocketType of
    stOverbyte: vSocket := TMQTTOverbyteSocket.Create;
    stIndy: vSocket := TMQTTIndySocket.Create;
  end;

  Result := TMQTTClient.Create(vSocket, TMQTTDefaultTimer.Create,
    TMQTTV311PacketBuilder.Create, TMQTTV311Parser.Create,
    pKeepAliveSecondsInterval);
end;

end.