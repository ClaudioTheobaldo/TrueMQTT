unit MQTT.IPacketBuilder;

interface

uses
  System.SysUtils,
  MQTT.Types;

type
  IPacketBuilder = interface
  ['{D0091A10-3FCD-49AC-83C0-7A2003F003CB}']
    function BuildConnectPacket(pKeepAliveInterval: UInt16; pClientParams: TConnectParams): TBytes;
    function BuildPublishPacket(const pTopic: string; pPayload: TBytes;
      pRetain, pDup: Boolean; pQosLevel: TQosLevel; var pPacketID: UInt16): TBytes;
    function BuildPubrelPacket(pPacketIdentifier: UInt16): TBytes;
    function BuildSubscribePacket(const pTopics: TArray<TTopicFilter>; var pPacketIdentifier: UInt16): TBytes;
    function BuildUnsubscribePacket(pTopics: TArray<string>; var pPacketIdentifier: UInt16): TBytes;
    function BuildPingReqPacket: TBytes;
    function BuildDisconnectPacket: TBytes;
  end;

implementation

end.