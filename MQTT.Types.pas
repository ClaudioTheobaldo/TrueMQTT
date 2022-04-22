unit MQTT.Types;

interface

uses
  System.SysUtils;

type
  TMQTTControlPacket =
  (
    Reserved,
    CONNECT, // C1 -> S1 (Client request to connect to server)
    CONNACK, // C1 <- S1 (Connect acknowledgment)
    PUBLISH, // C1 <-> S1 (Publish message)
    PUBACK, // C1 <-> S1 (Publish acknowledgment)
    PUBREC, // C1 <-> S1 (Publish received (assured delivery part 1))
    PUBREL, // C1 <-> S1 (Publish release (assured delivery part 2))
    PUBCOMP, // C1 <-> S1 (Publish complete (assured delivery part 3))
    SUBSCRIBE, // C1 -> S1 (Client subscribe request)
    SUBACK, // C1 <- S1   (Subscribe acknowledgment)
    UNSUBSCRIBE, // C1 -> S1 (Unsubscribe request)
    UNSUBACK, // C1 <- S1 (Unsubscribe acknowledgment)
    PINGREQ, // C1 -> S1 (PING request)
    PINGRESP, // C1 <- S1 (PING response)
    DISCONNECT, // C1 -> S1 (Client is disconnecting)
    AUTH
  );

  TMQTTClientState = (mcsDisconnected, mcsConnecting, mcsConnected);

  TConnackReturnCodes = (crcAccepted, crcRefusedWrongVersion, crcRefuseBadIdentifier,
    crcRefuseUnavailableServer, crcRefuseUnauthorized);

  TQosLevel = (qlAtMostOnceDelivery, qlAtLeastOnceDelivery, qlExactlyOnceDelivery);

  TOnData = procedure(pData: TBytes) of object;
  TSubscribeEvent = procedure(pPacketID: UInt16; pQoSLevel: TQoSLevel; const pTopic: string) of object;
  TUnsubscribeEvent = procedure(pPacketID: UInt16) of object;
  TPubackEvent = procedure(pPacketID: UInt16; const pTopic: string) of object;
  TPubcompEvent = procedure(pPacketID: UInt16; const pTopic: string) of object;
  TPublishEvent = procedure(pPacketID: UInt16; const pTopic: string; pPayload: TBytes) of object;

  TConnectParams = packed record
    ClientId: string;
    WillRetain: Boolean;
    WillQos: TQosLevel;
    WillTopic: string;
    WillMessage: string;
    Username: string;
    Password: string;
    constructor Create(pWillQos: TQosLevel; pWillRetain: Boolean;
      const pClientID, pWillTopic, pWillMessage, pUsername, pPassword: string);
  end;

  // It needs to be packed elsewise some weird exception happens when cleaning up memory.
  TTopicFilter = packed record
    Topic: string;
    Qos: TQosLevel;
  end;

  TTopic = packed record
    PacketID: UInt16;
    TopicFilter: TTopicFilter;
    constructor Create(pPacketID: UInt16; pTopicFilter: TTopicFilter);
  end;

implementation

{ TTopic }

constructor TTopic.Create(pPacketID: UInt16; pTopicFilter: TTopicFilter);
begin
  PacketID := pPacketID;
  TopicFilter := pTopicFilter;
end;

{ TClientParams }

constructor TConnectParams.Create(pWillQos: TQosLevel; pWillRetain: Boolean;
  const pClientID, pWillTopic, pWillMessage, pUsername, pPassword: string);
begin
  ClientId := pClientId;
  WillQos := pWillQos;
  WillRetain := pWillRetain;
  WillTopic := pWillTopic;
  WillMessage := pWillMessage;
  Username := pUsername;
  Password := pPassword;
end;

end.