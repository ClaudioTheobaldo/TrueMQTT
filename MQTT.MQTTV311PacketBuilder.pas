unit MQTT.MQTTV311PacketBuilder;

interface

uses
  System.SysUtils,
  MQTT.Types, MQTT.IMQTTPacketBuilder;

type
  TMQTTV311PacketBuilder = class(TInterfacedObject, IMQTTPacketBuilder)
  strict private
    fPacketId: UInt16;
  public
    function BuildConnectPacket(pKeepAliveInterval: UInt16; const pUsername: string;
      const pPassword: string): TBytes;
    function BuildPublishPacket(const pTopic: string; pPayload: TBytes;
      pRetain, pDup: Boolean; pQosLevel: TQosLevel; var pPacketID: UInt16): TBytes;
    function BuildPubrelPacket(pPacketIdentifier: UInt16): TBytes;
    function BuildSubscribePacket(const pTopics: TArray<TTopicFilter>; var pPacketIdentifier: UInt16): TBytes;
    function BuildUnsubscribePacket(pTopics: TArray<string>; var pPacketIdentifier: UInt16): TBytes;
    function BuildPingReqPacket: TBytes;
    function BuildDisconnectPacket: TBytes;
  end;

implementation

uses
  MQTT.Utils;

const
  c_PubrelPacketSize = 4;

{ TMQTTV311PacketBuilder }

function TMQTTV311PacketBuilder.BuildConnectPacket(pKeepAliveInterval: UInt16;
  const pUsername: string; const pPassword: string): TBytes;
var
  vCount, vRemainingLength: Integer;
  vPacketTypeAndFlags, vConnectFlags: Byte;
  vFixedHeader, vEncodedRemainingLength,
    vUsernameAsBytes, vPasswordAsBytes: TBytes;
  vVariableHeader: array[0..9] of Byte;
  vClientLength, vUserNameLength, vPasswordLength: UInt16;
begin
  // WARNING INCOMPLETE FUNCTION, NEEDS TO VALIDATE USERNAME, PASSWORD, QoS AND
  // A BUNCH OF OTHER STUFF...

  vConnectFlags := 0;

  // 1 - Fixed header
  vPacketTypeAndFlags := Byte(TMQTTControlPacket.CONNECT) shl 4;

  // 2 - Variable header
  // 2.1 - Protocol Name
  vVariableHeader[0] := 0;
  vVariableHeader[1] := 4;
  vVariableHeader[2] := 77; // M
  vVariableHeader[3] := 81; // Q
  vVariableHeader[4] := 84; // T
  vVariableHeader[5] := 84; // T

  // 2.2 - Protocol Level (4) MQTT 3.1.1
  vVariableHeader[6] := 4;

  // 2.3 - Connect Flags

  if pUserName <> EmptyStr then
  begin
    vUsernameAsBytes := TEncoding.UTF8.GetBytes(pUserName);
    vUsernameLength := Length(vUsernameAsBytes);
    SetNthBit(vConnectFlags, 7); //Bit-7 = User-name
  end;
  if pPassword <> EmptyStr then
  begin
    vPasswordAsBytes := TEncoding.UTF8.GetBytes(pPassword);
    vPasswordLength := Length(vPasswordAsBytes);
    SetNthBit(vConnectFlags, 6); //Bit-6 = Password
  end;
  //SetNthBit(vConnectFlags, 5); //Bit-5 = Will-retain
  //SetNthBit(vConnectFlags, 4); //Bit-4 = Will-QoS(4)
  //SetNthBit(vConnectFlags, 3); //Bit-3 = Will-QoS(3)
  //SetNthBit(vConnectFlags, 2); //Bit-2 = Will-Flag

  // Let the server generate the ClientID.
  // WillQos must be 0, server must accept anonymous clients and
  // empty client-ids
  SetNthBit(vConnectFlags, 1); //Bit-1 = Clean Session
  //SetNthBit(vConnectFlags, 0); //Bit-0 = Reserved (must be zero)

  vVariableHeader[7] := vConnectFlags;

  // 2.4 - Keep alive
  vVariableHeader[8] := PByte(@pKeepAliveInterval)[1];
  vVariableHeader[9] := PByte(@pKeepAliveInterval)[0];

  // 3 - Payload
  // ClientId
  vClientLength := 0;
  // WillTopic
  // WillMessage
  // Username
  // Password

  vRemainingLength := Length(vVariableHeader) + 2; //Variable header len + Payload len
  vEncodedRemainingLength := EncodeVarInt32(vRemainingLength);
  SetLength(vFixedHeader, 1 + Length(vEncodedRemainingLength));
  vFixedHeader[0] := vPacketTypeAndFlags;
  Move(vEncodedRemainingLength[0], vFixedHeader[1], Length(vEncodedRemainingLength));

  // FixedHeaderLen + VariableHeaderLen + PayloadLen
  SetLength(Result, Length(vFixedHeader) + vRemainingLength);

  // 1 - FixedHeader
  vCount := 0;
  Move(vFixedHeader[0], Result[vCount], Length(vFixedHeader));
  Inc(vCount, Length(vFixedHeader));
  // 2 - VariableHeader
  Move(vVariableHeader[0], Result[vCount], Length(vVariableHeader));
  Inc(vCount, Length(vVariableHeader));
  // 3 - Payload
  Result[vCount] := PByte(@vClientLength)[1];
  Result[vCount + 1] := PByte(@vClientLength)[0];
  Inc(vCount, 2);
  if pUserName <> EmptyStr then
  begin
    Result[vCount] := PByte(@vUserNameLength)[1];
    Result[vCount + 1] := PByte(@vUserNameLength)[0];
    Inc(vCount, 2);
    Move(vUsernameAsBytes[0], Result[vCount], vUserNameLength);
    Inc(vCount, vUserNameLength);
  end;
  if pPassword <> EmptyStr then
  begin
    Result[vCount] := PByte(@vPasswordLength)[1];
    Result[vCount + 1] := PByte(@vPasswordLength)[0];
    Inc(vCount, 2);
    Move(vPasswordAsBytes[0], Result[vCount], vPasswordLength);
    //Inc(vCount, vPasswordLength);
  end;
end;

function TMQTTV311PacketBuilder.BuildPublishPacket(const pTopic: string; 
  pPayload: TBytes; pRetain, pDup: Boolean; pQosLevel: TQosLevel; 
  var pPacketID: UInt16): TBytes;
var
  vRemainingLength, vPacketLength, vIndex, vPacketIDSize, vPayloadLen: Integer;
  vPacketTypeAndFlags, vQos: Byte;
  vTopicByteLength: Word;
  vTopicAsBytes, vRemainingLengthEncoded: TBytes;
begin
  // 2.1 Topic
  vPacketLength := 0;
  vIndex := 0;
  vPacketIDSize := 0;

  vTopicAsBytes := TEncoding.UTF8.GetBytes(pTopic);
  if Length(vTopicAsBytes) > Word.MaxValue then
    raise Exception.Create(Format('Topic byte array length should''t be higher than %d', [Word.MaxValue]));
  vTopicByteLength := Length(vTopicAsBytes);

  // 1 - Fixed header flag
  vPacketTypeAndFlags := Byte(TMQTTControlPacket.PUBLISH) shl 4;
  if pRetain then
    SetNthBit(vPacketTypeAndFlags, 0); //Retain
  vQos := Byte(pQosLevel) shl 1;
  vPacketTypeAndFlags := vPacketTypeAndFlags or vQos;
  if pDup then
    SetNthBit(vPacketTypeAndFlags, 3); // DUP Flag

  // 2 - Variable header
  // 2.2 - PacketIdentifier
  if pQoSLevel in [qlAtLeastOnceDelivery..qlExactlyOnceDelivery] then
  begin
    Inc(vPacketIDSize, 2);
    AtomicIncrement(fPacketId);
    pPacketID := fPacketID;
  end
  else
    pPacketID := 0;

  vPayloadLen := Length(pPayload);
  vRemainingLength := Sizeof(Word) + vTopicByteLength + vPacketIDSize + vPayloadLen;
  vRemainingLengthEncoded := EncodeVarInt32(vRemainingLength);

  //Building packet
  Inc(vPacketLength, 1 + Length(vRemainingLengthEncoded) + vRemainingLength);
  SetLength(Result, vPacketLength);

  // 1 - Fixed header
  Result[vIndex] := vPacketTypeAndFlags;
  Inc(vIndex);
  Move(vRemainingLengthEncoded[0], Result[vIndex], Length(vRemainingLengthEncoded));
  Inc(vIndex, Length(vRemainingLengthEncoded));

  // 2 - Variable header
  Result[vIndex] := PByte(@vTopicByteLength)[1];
  Result[vIndex + 1] := PByte(@vTopicByteLength)[0];
  Inc(vIndex, 2);

  Move(vTopicAsBytes[0], Result[vIndex], vTopicByteLength);
  Inc(vIndex, vTopicByteLength);

  if pQoSLevel in [qlAtLeastOnceDelivery..qlExactlyOnceDelivery] then
  begin
    Result[vIndex] := PByte(@fPacketId)[1];
    Result[vIndex + 1] := PByte(@fPacketId)[0];
    Inc(vIndex, 2);
  end;

  // 3 - Payload
  Move(pPayload[0], Result[vIndex], vPayloadLen);
end;

function TMQTTV311PacketBuilder.BuildPubrelPacket(pPacketIdentifier: UInt16): TBytes;
var
  vReserved: Byte;
begin
  SetLength(Result, c_PubrelPacketSize);
  // 1 - Fixed header
  Result[0] := Byte(TMQTTControlPacket.PUBREL) shl 4;
  vReserved := 2;
  Result[0] := Result[0] or vReserved;
  Result[1] := 2;

  // 2 - Variable header
  Result[2] := PByte(@pPacketIdentifier)[1];
  Result[3] := PByte(@pPacketIdentifier)[0];
end;

function TMQTTV311PacketBuilder.BuildSubscribePacket(
  const pTopics: TArray<TTopicFilter>; var pPacketIdentifier: UInt16): TBytes;
var
  vCount, vRemainingLength: Integer;
  vPacketTypeAndFlags, vDummy, vReserved: Byte;
  vPacketIdentifier: array[0..1] of Byte;
  vTotalTopicsByteLength, vIndex: UInt32;
  vTopicsByteLength: TArray<Word>;
  vTopicsAsBytes: TArray<TArray<Byte>>;
  vPayload, vRemainingLengthEncoded: TBytes;
begin
  if pTopics = nil then
    Exit;

  vTotalTopicsByteLength := 0;
  SetLength(vTopicsAsBytes, Length(pTopics));
  SetLength(vTopicsByteLength, Length(pTopics));
  for vCount := 0 to Length(pTopics) - 1 do
  begin
    // 2.1 Topic
    vTopicsAsBytes[vCount] := TEncoding.UTF8.GetBytes(pTopics[vCount].Topic);
    if Length(vTopicsAsBytes[vCount]) > Word.MaxValue then
      raise Exception.Create(Format('Topic [%d] byte array length should''t be higher than %d', [vCount, Word.MaxValue]));
    vTopicsByteLength[vCount] := Length(vTopicsAsBytes[vCount]);
    vTotalTopicsByteLength := vTotalTopicsByteLength + vTopicsByteLength[vCount];
  end;

  vDummy := Byte(TMQTTControlPacket.SUBSCRIBE);
  vReserved := 2;
  vPacketTypeAndFlags := 0;

  // 1 - Fixed header byte-1 flag
  vDummy := vDummy shl 4;
  vPacketTypeAndFlags := vPacketTypeAndFlags or vDummy;
  vPacketTypeAndFlags := vPacketTypeAndFlags or vReserved;

  // 2 - Variable header
  AtomicIncrement(fPacketId);
  pPacketIdentifier := fPacketId;
  vPacketIdentifier[0] := PByte(@fPacketId)[1];
  vPacketIdentifier[1] := PByte(@fPacketId)[0];

  // 3 - Payload (Subscribe without payload is a protocol violation)
  // 3 bytes because it's UTF8Str size + QoS
  vIndex := 0;
  SetLength(vPayload, (Length(vTopicsAsBytes) * 3) + vTotalTopicsByteLength);
  for vCount := 0 to Length(vTopicsAsBytes) - 1 do
  begin
    vPayload[vIndex] := PByte(@vTopicsByteLength[vCount])[1];
    vPayload[vIndex + 1] := PByte(@vTopicsByteLength[vCount])[0];
    Inc(vIndex, 2);

    Move(vTopicsAsBytes[vCount][0], vPayload[vIndex],
      vTopicsByteLength[vCount]);
    Inc(vIndex, vTopicsByteLength[vCount]);

    Move(pTopics[vCount].Qos, vPayload[vIndex], 1);
    Inc(vIndex, Sizeof(TQosLevel));
  end;

  // |Variable Header Size| + |Payload Size|
  vRemainingLength := 2 + Length(vPayload);
  vRemainingLengthEncoded := EncodeVarInt32(vRemainingLength);

  vIndex := 0;

  SetLength(Result, 1 + Length(vRemainingLengthEncoded) + vRemainingLength);

  Move(vPacketTypeAndFlags, Result[vIndex], 1);
  Inc(vIndex);

  Move(vRemainingLengthEncoded[0], Result[vIndex], Length(vRemainingLengthEncoded));
  Inc(vIndex, Length(vRemainingLengthEncoded));

  Move(vPacketIdentifier[0], Result[vIndex], Length(vPacketIdentifier));
  Inc(vIndex, Length(vPacketIdentifier));

  Move(vPayload[0], Result[vIndex], Length(vPayload));
end;

function TMQTTV311PacketBuilder.BuildUnsubscribePacket(pTopics: TArray<string>;
  var pPacketIdentifier: UInt16): TBytes;
var
  vCount, vIndex, vRemainingLength, vPayloadLen: Integer;
  vPacketTypeAndFlags, vDummy, vReserved: Byte;
  vPacketIdentifier: array[0..1] of Byte;
  vTotalTopicsByteLength: UInt32;
  vTopicsAsBytes: TArray<TArray<Byte>>;
  vTopicsByteLength: TArray<Word>;
  vEncodedRemainingLength, vPayload: TBytes;
begin
  if pTopics = nil then
    Exit;

  // Unsub packet with no payload is a protocol violation
  if (Length(pTopics) = 1) and (pTopics[0] = EmptyStr) then
    Exit;

  // 3.1 Topic
  vTotalTopicsByteLength := 0;
  SetLength(vTopicsAsBytes, Length(pTopics));
  SetLength(vTopicsByteLength, Length(pTopics));
  for vCount := 0 to Length(pTopics) - 1 do
  begin
    vTopicsAsBytes[vCount] := TEncoding.UTF8.GetBytes(pTopics[vCount]);
    if Length(vTopicsAsBytes[vCount]) > Word.MaxValue then
      raise Exception.Create(Format('Topic [%d] byte array length should''t be higher than %d', [vCount, Word.MaxValue]));
    vTopicsByteLength[vCount] := Length(vTopicsAsBytes[vCount]);
    vTotalTopicsByteLength := vTotalTopicsByteLength + vTopicsByteLength[vCount];
  end;

  // 1 - Fixed header byte-1 flag ($A2, 162 or 0b10100010)
  vDummy := Byte(TMQTTControlPacket.UNSUBSCRIBE);
  vReserved := 2;
  vPacketTypeAndFlags := 0;
  vDummy := vDummy shl 4;
  vPacketTypeAndFlags := vPacketTypeAndFlags or vDummy;
  vPacketTypeAndFlags := vPacketTypeAndFlags or vReserved;

  // 2 - Variable header
  AtomicIncrement(fPacketId);
  pPacketIdentifier := fPacketId;
  vPacketIdentifier[0] := PByte(@fPacketId)[1];
  vPacketIdentifier[1] := PByte(@fPacketId)[0];

  // 3 - Payload
  vIndex := 0;
  vPayloadLen := (2 * Length(pTopics)) + vTotalTopicsByteLength;
  SetLength(vPayload,  vPayloadLen);
  for vCount := 0 to Length(vTopicsAsBytes) - 1 do
  begin
    vPayload[vIndex] := PByte(@vTopicsByteLength[vCount])[1];
    vPayload[vIndex + 1] := PByte(@vTopicsByteLength[vCount])[0];
    Inc(vIndex, 2);

    Assert(vTopicsByteLength[vCount] = Length(vTopicsAsBytes[vCount]));
    Move(vTopicsAsBytes[vCount][0], vPayload[vIndex],
      vTopicsByteLength[vCount]);
    Inc(vIndex, vTopicsByteLength[vCount]);
  end;

  vRemainingLength := Length(vPacketIdentifier) + vPayloadLen;
  vEncodedRemainingLength := EncodeVarInt32(vRemainingLength);

  SetLength(Result, 1 + Length(vEncodedRemainingLength) + vRemainingLength);

  vIndex := 0;

  // 1 - Fixed Header
  Result[0] := vPacketTypeAndFlags;
  Inc(vIndex);

  Move(vEncodedRemainingLength[0], Result[vIndex], Length(vEncodedRemainingLength));
  Inc(vIndex, Length(vEncodedRemainingLength));

  // 2 - Variable Header
  Move(vPacketIdentifier[0], Result[vIndex], Length(vPacketIdentifier));
  Inc(vIndex, Length(vPacketIdentifier));

  // 3 - Payload
  Move(vPayload[0], Result[vIndex], vPayloadLen);
end;

function TMQTTV311PacketBuilder.BuildPingReqPacket: TBytes;
begin
  SetLength(Result, 2);
  Result[0] := Byte(TMQTTControlPacket.PINGREQ) shl 4;
  Result[1] := 0;
end;

function TMQTTV311PacketBuilder.BuildDisconnectPacket: TBytes;
begin
  SetLength(Result, 2);
  Result[0] := Byte(TMQTTControlPacket.DISCONNECT) shl 4;
  Result[1] := 0;
end;

end.