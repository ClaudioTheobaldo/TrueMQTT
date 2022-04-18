unit MQTT.Client;

interface

uses
  System.SysUtils, System.Classes,
  System.Generics.Collections, System.Generics.Defaults,
  OverbyteIcsWndControl, OverbyteIcsWSocket,
  MQTT.Types, MQTT.ITimer,
  MQTT.ISocket, MQTT.IMQTTPacketBuilder;

type
  TMQTTClient = class
  strict private
    fSocket: IMQTTSocket;
    fHost: string;
    fPort: UInt16;

    fState: TMQTTClientState;

    fKeepAliveSecondInterval: UInt16;
    fKeepAliveTimer: IMQTTTimer;

    fPacketBuilder: IMQTTPacketBuilder;

    // Containers
    fPublishQosOneDict: TDictionary<UInt16, string>;
    fPublishQosTwoDict: TDictionary<UInt16, string>;
    fSubscriptionsDict: TDictionary<UInt16, TArray<TTopicFilter>>;
    fUnsubscribeDict: TDictionary<UInt16, TArray<string>>;
    fCurrentSubscriptions: TList<TTopic>;

    // Events
    fOnConnected: TNotifyEvent;
    fOnDisconnected: TNotifyEvent;
    fOnPuback: TPubackEvent;
    fOnPubComp: TPubcompEvent;
    fOnPublish: TPublishEvent;
    fOnSubscribeSuccess: TSubscribeEvent;
    fOnSubscribeFailure: TSubscribeEvent;
    fOnUnsubscribe: TUnsubscribeEvent;
    fOnPingResp: TNotifyEvent;

    // Container Utilities
    procedure AddOrSetTopic(pPacketIdentifier: UInt16;
      pTopicFilter: TTopicFilter);

    // Socket handlers
    procedure HandleSessionConnected(Sender: TObject);
    procedure HandleSessionClosed(Sender: TObject);
    procedure HandleDataAvailable(pData: TBytes);

    // Protocol handlers
    // TODO - Need to handle timeouts to cleanup the dictionaries in case these
    // messages never come to us.
    procedure HandleConnack(pBytes: TBytes);
    procedure HandlePuback(pBytes: TBytes);
    procedure HandlePubrec(pBytes: TBytes);
    procedure HandlePubComp(pBytes: TBytes);
    procedure HandlePublish(pPacket: TBytes);
    procedure HandleSuback(pPacket: TBytes);
    procedure HandleUnsuback(pBytes: TBytes);
    procedure HandlePingResp;

    // Timer Handlers
    procedure KeepAliveHandler(Sender: TObject);
  public
    constructor Create(pSocket: IMQTTSocket; pTimer: IMQTTTimer; pPacketBuilder: IMQTTPacketBuilder;
       pKeepAliveSecondsInterval: UInt16 = 10); reintroduce;
    destructor Destroy; override;

    function Connect(const pHost: string; pPort: UInt16): Boolean;
    procedure Disconnect;

    // MQTT messages
    procedure Publish(const pTopic: string; pPayload: TBytes;
      pRetain: Boolean = False; pQosLevel: TQosLevel = qlAtMostOnceDelivery);
    procedure Subscribe(pTopics: TArray<TTopicFilter>);
    procedure Unsubscribe(pTopics: TArray<string>);
    procedure PingReq;

    // Properties
    property Host: string read fHost;
    property Port: UInt16 read fPort;

    // Utilities
    function GetCurrentSubscriptions: TStringList;

    // Events
    property OnConnected: TNotifyEvent read fOnConnected write fOnConnected;
    property OnDisconnected: TNotifyEvent read fOnDisconnected write fOnDisconnected;
    property OnPuback: TPubackEvent read fOnPuback write fOnPuback;
    property OnPubComp: TPubCompEvent read fOnPubComp write fOnPubComp;
    property OnPublish: TPublishEvent read fOnPublish write fOnPublish;
    property OnSubscribeSuccess: TSubscribeEvent read fOnSubscribeSuccess write fOnSubscribeSuccess;
    property OnSubscribeFailure: TSubscribeEvent read fOnSubscribeFailure write fOnSubscribeFailure;
    property OnUnsubscribe: TUnsubscribeEvent read fOnUnsubscribe write fOnUnsubscribe;
    property OnPingResp: TNotifyEvent read fOnPingResp write fOnPingResp;
  end;

implementation

uses
  MQTT.Utils;

{ TMQTTClient }

{$REGION Ctor-Dtor}
constructor TMQTTClient.Create(pSocket: IMQTTSocket; pTimer: IMQTTTimer;
  pPacketBuilder: IMQTTPacketBuilder; pKeepAliveSecondsInterval: UInt16 = 10);
begin
  if pSocket = nil then
    raise Exception.Create('Unable to create MQTTClient because of nil Socket reference');
  if pTimer = nil then
    raise Exception.Create('Unable to create MQTTClient because of nil Timer reference');
  if pPacketBuilder = nil then
    raise Exception.Create('Unable to create MQTTClient because of nil PacketBuilder reference');

  fState := TMQTTClientState.mcsDisconnected;
  fKeepAliveSecondInterval := pKeepAliveSecondsInterval;

  fSocket := pSocket;
  fSocket.SetOnConnectedHandler(HandleSessionConnected);
  fSocket.SetOnDisconnectedHandler(HandleSessionClosed);
  fSocket.SetOnDataHandler(HandleDataAvailable);

  fKeepAliveTimer := pTimer;
  fKeepAliveTimer.SetInterval((fKeepAliveSecondInterval * 1000) - 1000);
  fKeepAliveTimer.SetTimerHandler(KeepAliveHandler);

  fPacketBuilder := pPacketBuilder;

  fPublishQosOneDict := TDictionary<UInt16, string>.Create;
  fPublishQosTwoDict := TDictionary<UInt16, string>.Create;
  fSubscriptionsDict := TDictionary<UInt16, TArray<TTopicFilter>>.Create;
  fUnsubscribeDict := TDictionary<UInt16, TArray<string>>.Create;
  fCurrentSubscriptions := TList<TTopic>.Create;
end;

destructor TMQTTClient.Destroy;
begin
  fKeepAliveTimer.SetTimerHandler(nil);
  FreeAndNil(fPublishQosOneDict);
  FreeAndNil(fPublishQosTwoDict);
  FreeAndNil(fSubscriptionsDict);
  FreeAndNil(fUnsubscribeDict);
  FreeAndNil(fCurrentSubscriptions);
  if fState = mcsConnected then
    Disconnect;
end;
{$ENDREGION}

{$REGION Conn-Disc}
function TMQTTClient.Connect(const pHost: string; pPort: UInt16): Boolean;
begin
  if (fState in [mcsConnecting, mcsConnected]) then
    Exit(False);
  Exit(fSocket.Connect(pHost, pPort));
  fHost := pHost;
  fPort := pPort;
end;

procedure TMQTTClient.Disconnect;
begin
  if fSocket.IsConnected then
  begin
    fKeepAliveTimer.SetTimerHandler(nil);
    fKeepAliveTimer.Enable(False);
    fSocket.Send(fPacketBuilder.BuildDisconnectPacket);
    fSocket.Disconnect;
  end;
end;

function TMQTTClient.GetCurrentSubscriptions: TStringList;
var
  vSubscriptions: TTopic;
begin
  Result := TStringList.Create;
  for vSubscriptions in fCurrentSubscriptions do
    Result.Add(vSubscriptions.TopicFilter.Topic);
end;
{$ENDREGION}

{$REGION MQTT ACTIONS}
procedure TMQTTClient.Publish(const pTopic: string; pPayload: TBytes;
  pRetain: Boolean = False; pQosLevel: TQosLevel = qlAtMostOnceDelivery);
var
  vPacketID: UInt16;
begin
  if fState <> mcsConnected then
    Exit;

  fSocket.Send(fPacketBuilder.BuildPublishPacket(pTopic, pPayload, pRetain, False{DUP Flag}, pQosLevel, vPacketID));
  if pQosLevel = qlAtLeastOnceDelivery then
    fPublishQosOneDict.AddOrSetValue(vPacketID, pTopic)
  else if pQosLevel = qlExactlyOnceDelivery then       
    fPublishQosTwoDict.AddOrSetValue(vPacketID, pTopic);
end;

procedure TMQTTClient.Subscribe(pTopics: TArray<TTopicFilter>);
var
  vPacketID: UInt16;
  vSubscribePacket: TBytes;
begin
  if fState <> mcsConnected then
    Exit;

  // Maybe should raise an error
  if pTopics = nil then
    Exit;

  vSubscribePacket := fPacketBuilder.BuildSubscribePacket(pTopics, vPacketID);
  fSocket.Send(vSubscribePacket);
  fSubscriptionsDict.AddOrSetValue(vPacketID, pTopics);
end;

procedure TMQTTClient.Unsubscribe(pTopics: TArray<string>);
var
  vPacketID: UInt16;
  vUnsubscribePacket: TBytes;
begin
  if fState <> mcsConnected then
    Exit;

  // Maybe should raise an error
  if pTopics = nil then
    Exit;

  vUnsubscribePacket := fPacketBuilder.BuildUnsubscribePacket(pTopics, vPacketID);
  fSocket.Send(vUnsubscribePacket);
  fUnsubscribeDict.AddOrSetValue(vPacketID, pTopics);
end;

procedure TMQTTClient.PingReq;
begin
  if fState <> mcsConnected then
    Exit;
  fSocket.Send(fPacketBuilder.BuildPingReqPacket);
end;
{$ENDREGION}

{$REGION SOCKETEVENTHANDLERS}
procedure TMQTTClient.HandleSessionConnected(Sender: TObject);
begin
  fSocket.Send(fPacketBuilder.BuildConnectPacket(fKeepAliveSecondInterval));
end;

procedure TMQTTClient.HandleSessionClosed(Sender: TObject);
begin
  fState := mcsDisconnected;
  if Assigned(fOnDisconnected) then
    fOnDisconnected(Self);
end;

procedure TMQTTClient.HandleDataAvailable(pData: TBytes);
var
  vPacketType: TMQTTControlPacket;
begin
  if Length(pData) >= 2 then
  begin
    Byte(vPacketType) := (pData[0] and $F0) shr 4;
    case vPacketType of
      TMQTTControlPacket.CONNACK: HandleConnack(pData);
      TMQTTControlPacket.PUBLISH: HandlePublish(pData);
      TMQTTControlPacket.PUBACK: HandlePuback(pData);
      TMQTTControlPacket.PUBREC: HandlePubrec(pData);
      TMQTTControlPacket.PUBCOMP: HandlePubComp(pData);
      TMQTTControlPacket.SUBACK: HandleSuback(pData);
      TMQTTControlPacket.UNSUBACK: HandleUnsuback(pData);
      TMQTTControlPacket.PINGRESP: HandlePingResp;
    end;
  end;
end;
{$ENDREGION}

{$REGION PROTOCOL HANDLERS}
procedure TMQTTClient.HandleConnack(pBytes: TBytes);
begin
  if (pBytes = nil) and (Length(pBytes) <> 4) then
    Exit;

  //Still needs to handle SessionPresent (page 32)
  case TConnackReturnCodes(pBytes[3]) of
      TConnackReturnCodes.crcAccepted:
      begin
        fState := mcsConnected;
        if Assigned(fOnConnected) then
          fOnConnected(Self);
        fKeepAliveTimer.Enable(True);
      end;
      TConnackReturnCodes.crcRefusedWrongVersion: raise Exception.Create('Wrong version');
      TConnackReturnCodes.crcRefuseBadIdentifier: raise Exception.Create('Incorrect client-id');
      TConnackReturnCodes.crcRefuseUnavailableServer: raise Exception.Create('Server unavailable');
      TConnackReturnCodes.crcRefuseUnauthorized: raise Exception.Create('Unauthorized');
    else
      raise Exception.Create('Bad CONNACK reserved return code for the current version');
  end;
end;

procedure TMQTTClient.HandlePuback(pBytes: TBytes);
var
  vPacketID: UInt16;
  vPair: TPair<UInt16, string>;
begin
  if (pBytes = nil) and (Length(pBytes) < 4)then
    Exit;

  PByte(@vPacketID)[0] := pBytes[3]; //LSB
  PByte(@vPacketID)[1] := pBytes[2]; //MSB
  vPair := fPublishQosOneDict.ExtractPair(vPacketID);
  if vPair.Value <> Default(string) then
    if Assigned(fOnPuback) then
      fOnPuback(vPair.Key, vPair.Value);
end;

procedure TMQTTClient.HandlePubrec(pBytes: TBytes);
var
  vPacketID: UInt16;
  vValue: string;
begin
  if (pBytes = nil) and (Length(pBytes) <> 4)then
    Exit;

  PByte(@vPacketID)[0] := pBytes[3]; //LSB
  PByte(@vPacketID)[1] := pBytes[2]; //MSB
  if fPublishQosTwoDict.TryGetValue(vPacketID, vValue) then
    if vValue <> Default(string) then
      fSocket.Send(fPacketBuilder.BuildPubrelPacket(vPacketID));
end;

procedure TMQTTClient.HandlePubComp(pBytes: TBytes);
var
  vPacketID: UInt16;
  vPair: TPair<UInt16, string>;
begin
  if (pBytes = nil) and (Length(pBytes) < 4)then
    Exit;

  PByte(@vPacketID)[0] := pBytes[3]; //LSB
  PByte(@vPacketID)[1] := pBytes[2]; //MSB
  vPair := fPublishQosTwoDict.ExtractPair(vPacketID);
  if vPair.Value <> Default(string) then
    if Assigned(fOnPubComp) then
      fOnPubComp(vPair.Key, vPair.Value);
end;

procedure TMQTTClient.HandlePublish(pPacket: TBytes);
const
  c_RetainMask = 1;
  c_QosMask = 6;
  c_DupMask = 8;
  c_PacketTypeMask = 240;
var
  vQoS: TQoSLevel;
  //vRetain, vDup: Boolean;
  vPacketType: TMQTTControlPacket;
  vRemainingLength, vSize, vIndex, vPayloadSize: Integer;
  vTopicLen, vPacketID: UInt16;
  vTopicAsBytes, vPayload: TBytes;
  vTopic: string;
begin
  // 1 - Fixed header
  if Length(pPacket) < 5 then // Not sure about this one...
    Exit;

  //vRetain := (pPacket[0] and c_RetainMask) = 1;
  vQos := TQoSLevel((pPacket[0] and c_QosMask) shr 1);
  //vDup := ((pPacket[0] and c_DupMask) shr 3) = 1;
  vPacketType := TMQTTControlPacket((pPacket[0] and c_PacketTypeMask) shr 4);

  if vPacketType <> TMQTTControlPacket.PUBLISH then
    Exit;

  vRemainingLength := DecodeVarInt32(PByte(@pPacket[1]), vSize);
  vIndex := 1 + vSize;

  // 2 - Variable header
  PByte(@vTopicLen)[0] := pPacket[vIndex + 1];
  PByte(@vTopicLen)[1] := pPacket[vIndex];
  Inc(vIndex, 2);
  SetLength(vTopicAsBytes, vTopicLen);
  Move(pPacket[vIndex], vTopicAsBytes[0], vTopicLen);
  Inc(vIndex, vTopicLen);
  if vQos in [qlAtLeastOnceDelivery..qlExactlyOnceDelivery] then
  begin
    PByte(@vPacketID)[1] := pPacket[vIndex];
    PByte(@vPacketID)[0] := pPacket[vIndex + 1];
    Inc(vIndex, 2);
  end
  else
    vPacketID := 0;

  // 3 - Payload (RemainingLen - VariableHaderLen = vPayloadLen)
  vPayloadSize := vRemainingLength - (vTopicLen + 2);
  if vQos in [qlAtLeastOnceDelivery..qlExactlyOnceDelivery] then
    Dec(vPayloadSize, 2);
  SetLength(vPayload, vPayloadSize);
  Move(pPacket[vIndex], vPayload[0], vPayloadSize);

  vTopic := TEncoding.UTF8.GetString(vTopicAsBytes);
  if Assigned(fOnPublish) then
    fOnPublish(vPacketID, vTopic, vPayload);
end;


procedure TMQTTClient.HandleSuback(pPacket: TBytes);
const
  c_SubackFailure = $80;
var
  vPacketIdentifier: UInt16;
  vSize, vCount, vIndex: Integer;
  vReturnCodes: TList<Integer>;
  vPair: TPair<UInt16, TArray<TTopicFilter>>;
begin
  if Length(pPacket) <= 4 then
    Exit;

  DecodeVarInt32(PByte(@pPacket[1]), vSize);
  vIndex := 1 + vSize;

  PByte(@vPacketIdentifier)[1] := pPacket[vIndex]; // MSB
  PByte(@vPacketIdentifier)[0] := pPacket[vIndex + 1]; // LSB
  Inc(vIndex, 2);

  vPair := fSubscriptionsDict.ExtractPair(vPacketIdentifier);

  if vPair.Value = Default(TArray<TTopicFilter>) then
    Exit;

  vReturnCodes := TList<Integer>.Create;
  try
    for vCount := vIndex to Length(pPacket) - 1 do
    begin
      if TQosLevel(pPacket[vCount]) in [qlAtMostOnceDelivery..qlExactlyOnceDelivery] then
      begin
        vReturnCodes.Add(vCount - vIndex);
        if Assigned(fOnSubscribeSuccess) then
          fOnSubscribeSuccess(vPacketIdentifier, TQosLevel(pPacket[vCount]),
            vPair.Value[vCount - vIndex].Topic);
      end
      else if pPacket[vCount] = c_SubackFailure then
      begin
        if Assigned(fOnSubscribeFailure) then
          fOnSubscribeFailure(vPacketIdentifier, TQosLevel(pPacket[vCount]),
            vPair.Value[vCount - vIndex].Topic);
      end
      else
        // Subscribe error event would be nice "fOnSubscribeError"
        raise Exception.Create(Format('Bad return code on suback. PacketIdentifier [%d]',
          [vPacketIdentifier]));
    end;

    for vCount := 0 to vReturnCodes.Count - 1 do
      AddOrSetTopic(vPacketIdentifier, vPair.Value[vReturnCodes[vCount]])

  finally
    FreeAndNil(vReturnCodes);
  end;
end;

procedure TMQTTClient.HandleUnsuback(pBytes: TBytes);
var
  vPacketIdentifier: UInt16;
  vPair: TPair<UInt16, TArray<string>>;
  vCount, vSubCount: Integer;
begin
  if Length(pBytes) < 4 then
    Exit;

  PByte(@vPacketIdentifier)[1] := pBytes[2]; // MSB
  PByte(@vPacketIdentifier)[0] := pBytes[3]; // LSB

  vPair := fUnsubscribeDict.ExtractPair(vPacketIdentifier);

  if vPair.Value <> Default(TArray<string>) then
  begin
    for vCount := 0 to High(vPair.Value) do
      for vSubCount := fCurrentSubscriptions.Count - 1 downto 0 do
        if vPair.Value[vCount] = fCurrentSubscriptions[vSubCount].TopicFilter.Topic then
        begin
          fCurrentSubscriptions.Delete(vSubCount);
          Break;
        end;
  end;

  if Assigned(fOnUnsubscribe) then
    fOnUnsubscribe(vPacketIdentifier);
end;

procedure TMQTTClient.HandlePingResp;
begin
  if Assigned(fOnPingResp) then
    fOnPingResp(Self);
end;
{$ENDREGION}

{$REGION CONTAINER UTILITIES}
procedure TMQTTClient.AddOrSetTopic(pPacketIdentifier: UInt16;
  pTopicFilter: TTopicFilter);
var
  vCount: Integer;
begin
  for vCount := 0 to fCurrentSubscriptions.Count - 1 do
    if fCurrentSubscriptions[vCount].TopicFilter.Topic = pTopicFilter.Topic then
    begin
      fCurrentSubscriptions.Items[vCount] := TTopic.Create(pPacketIdentifier,
        pTopicFilter);
      Exit;
    end;
  fCurrentSubscriptions.Add(TTopic.Create(pPacketIdentifier, pTopicFilter));
end;
{$ENDREGION}

{$REGION TIMER HANDLERS}
procedure TMQTTClient.KeepAliveHandler(Sender: TObject);
begin
  if fState = mcsConnected then
    PingReq;
end;
{$ENDREGION}

end.