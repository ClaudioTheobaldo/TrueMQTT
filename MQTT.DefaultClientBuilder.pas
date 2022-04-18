unit MQTT.DefaultClientBuilder;

interface

uses
  MQTT.Client;

type
  TSocketType = (stOverbyte, stIndy);

  TMQTTDefaultClientBuilder = class
    class function NewDefaultClient(pSocketType: TSocketType;
      pKeepAliveSecondsInterval: UInt16 = 10): TMQTTClient;
  end;

implementation

uses
  MQTT.ISocket, MQTT.DefaultTimer,
  MQTT.OverbyteSocket, MQTT.IndySocket,
  MQTT.MQTTV311PacketBuilder;

{ TMQTTDefaultClientBuilder }

class function TMQTTDefaultClientBuilder.NewDefaultClient(
  pSocketType: TSocketType; pKeepAliveSecondsInterval: UInt16): TMQTTClient;
var
  vSocket: IMQTTSocket;
begin
  case pSocketType of
    stOverbyte: vSocket := TMQTTOverbyteSocket.Create;
    stIndy: vSocket := TMQTTIndySocket.Create;
  end;

  Result := TMQTTClient.Create(vSocket,
    TMQTTDefaultTimer.Create, TMQTTV311PacketBuilder.Create,
    pKeepAliveSecondsInterval);
end;

end.