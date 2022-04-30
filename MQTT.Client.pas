unit MQTT.Client;

interface

uses
  System.SysUtils, System.Classes,
  System.Generics.Collections, System.Generics.Defaults,
  MQTT.Types, MQTT.ITimer,
  MQTT.ISocket, MQTT.IPacketBuilder,
  MQTT.IParser;

type
  TMQTTClient = class
  strict private
    fSocket: IMQTTSocket;
    fHost: string;
    fPort: UInt16;

    fConnectParams: TConnectParams;

    fState: TMQTTClientState;

    fKeepAliveSecondInterval: UInt16;
    fKeepAliveTimer: ITimer;

    // Protocol Interfaces
    fPacketBuilder: IPacketBuilder;
    fParser: IParser;

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
    {
      TODO - Need to handle timeouts to cleanup the dictionaries in case these
      messages never come to us.
    }

    procedure HandleConnack(pReturnCode: TConnackReturnCodes);
    procedure HandlePuback(pPacketID: Word);
    procedure HandlePubrec(pPacketID: Word);
    procedure HandlePubcomp(pPacketID: Word);
    procedure HandlePublish(pPacketID: UInt16; const pTopic: string; pPayload: TBytes);
    procedure HandleSuback(pPacketID:UInt16; pQosLevels: TArray<Integer>);
    procedure HandleUnsuback(pPacketID: Word);
    procedure HandlePing(pSender: TObject);

    // Timer Handlers
    procedure KeepAliveHandler(Sender: TObject);
  public
    constructor Create(pSocket: IMQTTSocket; pTimer: ITimer;
      pPacketBuilder: IPacketBuilder; pParser: IParser;
      pKeepAliveSecondsInterval: UInt16 = 10); reintroduce;
    destructor Destroy; override;

    function Connect(const pHost: string; pPort: UInt16;
      pConnectParams: TConnectParams): Boolean; overload;

    function Connect(const pHost: string; pPort: UInt16;
      const pClientID: string = ''; pWillQos: TQosLevel = qlAtMostOnceDelivery;
      pWillRetain: Boolean = False; const pWillTopic: string = '';
      const pWillMessage: string = ''; const pUsername: string = '';
      const pPassword: string = ''): Boolean; overload;

    function Connect(const pHost: string; pPort: UInt16;
      const pUsername, pPassword: string): Boolean; overload;

    procedure Disconnect;

    // MQTT messages
    procedure Publish(const pTopic: string; pPayload: TBytes;
      pRetain: Boolean = False; pQosLevel: TQosLevel = qlAtMostOnceDelivery);
    procedure Subscribe(pTopics: TArray<TTopicFilter>);
    procedure Unsubscribe(pTopics: TArray<string>);
    procedure PingReq;

    // Utilities
    function GetProtocolVersion: string;
    function GetCurrentSubscriptions: TStringList;

    // Properties
    property Host: string read fHost;
    property Port: UInt16 read fPort;

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
  MQTT.Utils, MQTT.Consts;

{ TMQTTClient }

{$REGION Ctor-Dtor}
constructor TMQTTClient.Create(pSocket: IMQTTSocket; pTimer: ITimer;
  pPacketBuilder: IPacketBuilder; pParser: IParser;
  pKeepAliveSecondsInterval: UInt16 = 10);
begin
  if pSocket = nil then
    raise Exception.Create('Unable to create MQTTClient because of nil Socket reference');
  if pTimer = nil then
    raise Exception.Create('Unable to create MQTTClient because of nil Timer reference');
  if pPacketBuilder = nil then
    raise Exception.Create('Unable to create MQTTClient because of nil PacketBuilder reference');
  if pParser = nil then
    raise Exception.Create('Unable to create MQTTClient because of nil Parser reference');

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

  fParser := pParser;
  fParser.SetOnConnack(HandleConnack);
  fParser.SetOnPuback(HandlePuback);
  fParser.SetOnPubrec(HandlePubrec);
  fParser.SetOnPubcomp(HandlePubcomp);
  fParser.SetOnPublish(HandlePublish);
  fParser.SetOnSuback(HandleSuback);
  fParser.SetOnUnsuback(HandleUnsuback);
  fParser.SetOnPing(HandlePing);

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
function TMQTTClient.Connect(const pHost: string; pPort: UInt16;
  pConnectParams: TConnectParams): Boolean;
begin
  fConnectParams := pConnectParams;
  if (fState in [mcsConnecting, mcsConnected]) then
    Exit(False);
  Exit(fSocket.Connect(pHost, pPort));
  fHost := pHost;
  fPort := pPort;
end;

function TMQTTClient.Connect(const pHost: string; pPort: UInt16;
  const pClientID: string = ''; pWillQos: TQosLevel = qlAtMostOnceDelivery;
  pWillRetain: Boolean = False; const pWillTopic: string = '';
  const pWillMessage: string = ''; const pUsername: string = '';
  const pPassword: string = ''): Boolean;
begin
  fConnectParams := TConnectParams.Create(TQosLevel.qlAtMostOnceDelivery, False,
    pClientID, pWillTopic, pWillMessage, pUsername, pPassword);
  if (fState in [mcsConnecting, mcsConnected]) then
    Exit(False);
  Exit(fSocket.Connect(pHost, pPort));
  fHost := pHost;
  fPort := pPort;
end;

function TMQTTClient.Connect(const pHost: string; pPort: UInt16;
  const pUsername, pPassword: string): Boolean;
begin
  fConnectParams := TConnectParams.Create(TQosLevel.qlAtMostOnceDelivery, False,
    EmptyStr, EmptyStr, EmptyStr, pUsername, pPassword);
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
    fCurrentSubscriptions.Clear;
  end;
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

  // Need to know when to switch the duplicate flag
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
  fSocket.Send(fPacketBuilder.BuildConnectPacket(fKeepAliveSecondInterval, fConnectParams));
end;

procedure TMQTTClient.HandleSessionClosed(Sender: TObject);
begin
  fState := mcsDisconnected;
  if Assigned(fOnDisconnected) then
    fOnDisconnected(Self);
end;

procedure TMQTTClient.HandleDataAvailable(pData: TBytes);
begin
  fParser.InsertData(pData);
end;
{$ENDREGION}

{$REGION PROTOCOL HANDLERS}
procedure TMQTTClient.HandleConnack(pReturnCode: TConnackReturnCodes);
begin
  //Still needs to handle SessionPresent (page 32)
  case pReturnCode of
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

procedure TMQTTClient.HandlePuback(pPacketID: Word);
var
  vPair: TPair<UInt16, string>;
begin
  vPair := fPublishQosOneDict.ExtractPair(pPacketID);
  if vPair.Value <> Default(string) then
    if Assigned(fOnPuback) then
      fOnPuback(vPair.Key, vPair.Value);
end;

procedure TMQTTClient.HandlePubrec(pPacketID: Word);
var
  vValue: string;
begin
  if fPublishQosTwoDict.TryGetValue(pPacketID, vValue) then
    if vValue <> Default(string) then
      fSocket.Send(fPacketBuilder.BuildPubrelPacket(pPacketID));
end;

procedure TMQTTClient.HandlePubcomp(pPacketID: Word);
var
  vPair: TPair<UInt16, string>;
begin
  vPair := fPublishQosTwoDict.ExtractPair(pPacketID);
  if vPair.Value <> Default(string) then
    if Assigned(fOnPubComp) then
      fOnPubComp(vPair.Key, vPair.Value);
end;

procedure TMQTTClient.HandlePublish(pPacketID: UInt16; const pTopic: string;
  pPayload: TBytes);
begin
  if Assigned(fOnPublish) then
    fOnPublish(pPacketID, pTopic, pPayload);
end;

procedure TMQTTClient.HandleSuback(pPacketID: UInt16;
  pQosLevels: TArray<Integer>);
const
  c_SubackFailure = $80;
var
  vCount: Integer;
  vReturnCodes: TList<Integer>;
  vPair: TPair<UInt16, TArray<TTopicFilter>>;
begin
  vPair := fSubscriptionsDict.ExtractPair(pPacketID);

  if vPair.Value = Default(TArray<TTopicFilter>) then
    Exit;

  vReturnCodes := TList<Integer>.Create;
  try
    for vCount := 0 to High(pQosLevels) do
    begin
      if pQosLevels[vCount] = c_SubackFailure then
      begin
        if Assigned(fOnSubscribeFailure) then
          fOnSubscribeFailure(pPacketID, TQosLevel(pQosLevels[vCount]),
            vPair.Value[vCount].Topic);
      end
      else if TQosLevel(pQosLevels[vCount]) in [qlAtMostOnceDelivery..qlExactlyOnceDelivery] then
      begin
        vReturnCodes.Add(vCount);
        if Assigned(fOnSubscribeSuccess) then
          fOnSubscribeSuccess(pPacketID, TQosLevel(pQosLevels[vCount]),
            vPair.Value[vCount].Topic);
      end
      else
        // Subscribe error event would be nice "fOnSubscribeError"
        raise Exception.Create(Format('Bad return code on suback. PacketIdentifier [%d]',
          [pPacketID]));
    end;

    for vCount := 0 to vReturnCodes.Count - 1 do
      AddOrSetTopic(pPacketID, vPair.Value[vReturnCodes[vCount]]);
  finally
    FreeAndNil(vReturnCodes);
  end;
end;

procedure TMQTTClient.HandleUnsuback(pPacketID: Word);
var
  vPair: TPair<UInt16, TArray<string>>;
  vCount, vSubCount: Integer;
begin
  vPair := fUnsubscribeDict.ExtractPair(pPacketID);

  if vPair.Value <> Default(TArray<string>) then
    for vCount := 0 to High(vPair.Value) do
      for vSubCount := fCurrentSubscriptions.Count - 1 downto 0 do
        if vPair.Value[vCount] = fCurrentSubscriptions[vSubCount].TopicFilter.Topic then
        begin
          fCurrentSubscriptions.Delete(vSubCount);
          Break;
        end;

  if Assigned(fOnUnsubscribe) then
    fOnUnsubscribe(pPacketID);
end;

procedure TMQTTClient.HandlePing(pSender: TObject);
begin
  if Assigned(fOnPingResp) then
    fOnPingResp(Self);
end;
{$ENDREGION}

{$REGION UTILITIES}
function TMQTTClient.GetProtocolVersion: string;
begin
  Result := c_MQTTProtocolVersion311;
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