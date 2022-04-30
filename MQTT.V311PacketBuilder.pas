unit MQTT.V311PacketBuilder;

interface

uses
  System.SysUtils,
  MQTT.Types, MQTT.IPacketBuilder;

type
  TMQTTV311PacketBuilder = class(TInterfacedObject, IPacketBuilder)
  strict private
    fPacketId: UInt16;
  public
    function BuildConnectPacket(pKeepAliveInterval: UInt16;
      pClientParams: TConnectParams): TBytes;
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
  pClientParams: TConnectParams): TBytes;
const
  c_CONNECTVariableHeaderSize = 10;
var
  vCount, vRemainingLength: Integer;
  vPacketTypeAndFlags, vConnectFlags: Byte;
  vEncodedRemainingLength, vClientIdAsBytes, vWillTopicAsBytes, vWillMessageAsBytes,
  vUsernameAsBytes, vPasswordAsBytes: TBytes;
  vClientLength, vWillTopicLength, vWillMessageLength,
  vUserNameLength, vPasswordLength: UInt16;
begin
  vClientLength := 0;
  vWillMessageLength := 0;
  vWillTopicLength := 0;
  vConnectFlags := 0;

  // 1 - Fixed header
  vPacketTypeAndFlags := Byte(TMQTTControlPacket.CONNECT) shl 4;

  // 2.3 - Connect Flags
  if pClientParams.WillRetain then
    SetNthBit(vConnectFlags, 5); //Bit-5 = Will-Retain
  vConnectFlags := vConnectFlags or (Byte(pClientParams.WillQos) shl 3);

  // Let the server generate the ClientID.
  // WillQos must be 0, server must accept anonymous clients and
  // empty client-ids
  SetNthBit(vConnectFlags, 1); //Bit-1 = Clean Session

  // 3 - Payload
  vRemainingLength := c_CONNECTVariableHeaderSize; //Variable header len + Payload len

  if pClientParams.ClientId <> EmptyStr then
  begin
    vClientIdAsBytes := TEncoding.UTF8.GetBytes(pClientParams.ClientId);
    vClientLength := Length(vClientIdAsBytes);
    Inc(vRemainingLength, 2 + vClientLength);
  end
  else
    Inc(vRemainingLength, Sizeof(Word));

  if (pClientParams.WillTopic <> EmptyStr) then
  begin
    vWillTopicAsBytes := TEncoding.UTF8.GetBytes(pClientParams.WillTopic);
    vWillTopicLength := Length(vWillTopicAsBytes);
    Inc(vRemainingLength, 2 + vWillTopicLength);
    SetNthBit(vConnectFlags, 2); //Bit-2 = Will-Flag

    vWillMessageAsBytes := TEncoding.UTF8.GetBytes(pClientParams.WillMessage);
    vWillMessageLength := Length(vWillMessageAsBytes);
    Inc(vRemainingLength, 2 + vWillMessageLength);
  end;

  if pClientParams.UserName <> EmptyStr then
  begin
    vUsernameAsBytes := TEncoding.UTF8.GetBytes(pClientParams.UserName);
    vUsernameLength := Length(vUsernameAsBytes);
    SetNthBit(vConnectFlags, 7); //Bit-7 = User-name
    Inc(vRemainingLength, 2 + vUsernameLength);
  end;

  if (pClientParams.Password <> EmptyStr) and (pClientParams.Username <> EmptyStr) then
  begin
    vPasswordAsBytes := TEncoding.UTF8.GetBytes(pClientParams.Password);
    vPasswordLength := Length(vPasswordAsBytes);
    SetNthBit(vConnectFlags, 6); //Bit-6 = Password
    Inc(vRemainingLength, 2 + vPasswordLength);
  end;
  vEncodedRemainingLength := EncodeVarInt32(vRemainingLength);

  // FixedHeaderLen + VariableHeaderLen + PayloadLen
  SetLength(Result, 1 + Length(vEncodedRemainingLength) + vRemainingLength);

  // 1 - FixedHeader
  vCount := 0;
  Result[vCount] := vPacketTypeAndFlags;
  Move(vEncodedRemainingLength[0], Result[vCount + 1], Length(vEncodedRemainingLength));
  Inc(vCount, 1 + Length(vEncodedRemainingLength));

  // 2 - VariableHeader
  // 2.1 - Protocol Name
  Result[vCount] := 0;
  Result[vCount + 1] := 4;
  Result[vCount + 2] := 77; // M
  Result[vCount + 3] := 81; // Q
  Result[vCount + 4] := 84; // T
  Result[vCount + 5] := 84; // T
  // 2.2 - Protocol Level (4) MQTT 3.1.1
  Result[vCount + 6] := 4;
  Result[vCount + 7] := vConnectFlags;
  // 2.4 - Keep alive
  Result[vCount + 8] := PByte(@pKeepAliveInterval)[1];
  Result[vCount + 9]  := PByte(@pKeepAliveInterval)[0];
  Inc(vCount, c_CONNECTVariableHeaderSize);

  // 3 - Payload
  Result[vCount] := PByte(@vClientLength)[1];
  Result[vCount + 1] := PByte(@vClientLength)[0];
  Inc(vCount, Sizeof(Word));

  // 3.1 - ClientId
  if pClientParams.ClientId <> EmptyStr then
  begin
    Move(vClientIdAsBytes[0], Result[vCount], vClientLength);
    Inc(vCount, vClientLength);
  end;

  if (pClientParams.WillTopic <> EmptyStr) then
  begin
    // 3.2 -  Will Topic
    Result[vCount] := PByte(@vWillTopicLength)[1];
    Result[vCount + 1] := PByte(@vWillTopicLength)[0];
    Inc(vCount, Sizeof(Word));
    Move(vWillTopicAsBytes[0], Result[vCount], vWillTopicLength);
    Inc(vCount, vWillTopicLength);

    // 3.3 - Will Message
    Result[vCount] := PByte(@vWillMessageLength)[1];
    Result[vCount + 1] := PByte(@vWillMessageLength)[0];
    Inc(vCount, Sizeof(Word));
    if Length(vWillMessageAsBytes) > 0 then
    begin
      Move(vWillMessageAsBytes[0], Result[vCount], vWillMessageLength);
      Inc(vCount, vWillMessageLength);
    end;
  end;

  if pClientParams.UserName <> EmptyStr then
  begin
    Result[vCount] := PByte(@vUserNameLength)[1];
    Result[vCount + 1] := PByte(@vUserNameLength)[0];
    Inc(vCount, Sizeof(Word));
    Move(vUsernameAsBytes[0], Result[vCount], vUserNameLength);
    Inc(vCount, vUserNameLength);
  end;

  if (pClientParams.Password <> EmptyStr) and (pClientParams.Username <> EmptyStr) then
  begin
    Result[vCount] := PByte(@vPasswordLength)[1];
    Result[vCount + 1] := PByte(@vPasswordLength)[0];
    Inc(vCount, Sizeof(Word));
    Move(vPasswordAsBytes[0], Result[vCount], vPasswordLength);
  end;
end;

function TMQTTV311PacketBuilder.BuildPublishPacket(const pTopic: string; 
  pPayload: TBytes; pRetain, pDup: Boolean; pQosLevel: TQosLevel; 
  var pPacketID: UInt16): TBytes;
var
  vRemainingLength, vIndex, vPacketIDSize, vPayloadLen: Integer;
  vPacketTypeAndFlags, vQos: Byte;
  vTopicByteLength: Word;
  vTopicAsBytes, vRemainingLengthEncoded: TBytes;
begin
  // 2.1 Topic
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
  SetLength(Result, 1 + Length(vRemainingLengthEncoded) + vRemainingLength);

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
  vPacketTypeAndFlags: Byte;
  vPacketIdentifier: array[0..1] of Byte;
  vTotalTopicsByteLength, vIndex: UInt32;
  vTopicsByteLength: TArray<Word>;
  vTopicsAsBytes: TArray<TArray<Byte>>;
  vRemainingLengthEncoded: TBytes;
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

  // 1 - Fixed header byte-1 flag
  vPacketTypeAndFlags := Byte(TMQTTControlPacket.SUBSCRIBE) shl 4;
  vPacketTypeAndFlags := vPacketTypeAndFlags or 2;

  // 2 - Variable header
  AtomicIncrement(fPacketId);
  pPacketIdentifier := fPacketId;
  vPacketIdentifier[0] := PByte(@fPacketId)[1];
  vPacketIdentifier[1] := PByte(@fPacketId)[0];

  // |Variable Header Size| + |Payload Size|
  // 3 bytes because it's UTF8Str size + QoS
  vRemainingLength := 2 + (Length(vTopicsAsBytes) * 3) + vTotalTopicsByteLength;
  vRemainingLengthEncoded := EncodeVarInt32(vRemainingLength);

  vIndex := 0;

  SetLength(Result, 1 + Length(vRemainingLengthEncoded) + vRemainingLength);

  Move(vPacketTypeAndFlags, Result[vIndex], 1);
  Inc(vIndex);

  Move(vRemainingLengthEncoded[0], Result[vIndex], Length(vRemainingLengthEncoded));
  Inc(vIndex, Length(vRemainingLengthEncoded));

  Move(vPacketIdentifier[0], Result[vIndex], Length(vPacketIdentifier));
  Inc(vIndex, Length(vPacketIdentifier));

  // 3 - Payload
  for vCount := 0 to Length(vTopicsAsBytes) - 1 do
  begin
    // 3.1 - Length
    Result[vIndex] := PByte(@vTopicsByteLength[vCount])[1];
    Result[vIndex + 1] := PByte(@vTopicsByteLength[vCount])[0];
    Inc(vIndex, 2);

    // 3.2 - Topic
    Move(vTopicsAsBytes[vCount][0], Result[vIndex],
      vTopicsByteLength[vCount]);
    Inc(vIndex, vTopicsByteLength[vCount]);

    // 3.3 - QoS
    Move(pTopics[vCount].Qos, Result[vIndex], 1);
    Inc(vIndex, Sizeof(TQosLevel));
  end;
end;

function TMQTTV311PacketBuilder.BuildUnsubscribePacket(pTopics: TArray<string>;
  var pPacketIdentifier: UInt16): TBytes;
var
  vCount, vIndex, vRemainingLength: Integer;
  vPacketTypeAndFlags: Byte;
  vPacketIdentifier: array[0..1] of Byte;
  vTotalTopicsByteLength: UInt32;
  vTopicsAsBytes: TArray<TArray<Byte>>;
  vTopicsByteLength: TArray<Word>;
  vEncodedRemainingLength: TBytes;
begin
  if pTopics = nil then
    Exit;

  // Unsub packet with no payload is a protocol violation
  if (Length(pTopics) = 1) and (pTopics[0] = EmptyStr) then
    Exit;

  // 3.1 - Topic
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
  vPacketTypeAndFlags := Byte(TMQTTControlPacket.UNSUBSCRIBE) shl 4;
  vPacketTypeAndFlags := vPacketTypeAndFlags or 2;

  // 2 - Variable header
  AtomicIncrement(fPacketId);
  pPacketIdentifier := fPacketId;
  vPacketIdentifier[0] := PByte(@fPacketId)[1];
  vPacketIdentifier[1] := PByte(@fPacketId)[0];

  vRemainingLength := Length(vPacketIdentifier) + (2 * Length(pTopics)) + vTotalTopicsByteLength;
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
  for vCount := 0 to Length(vTopicsAsBytes) - 1 do
  begin
    Result[vIndex] := PByte(@vTopicsByteLength[vCount])[1];
    Result[vIndex + 1] := PByte(@vTopicsByteLength[vCount])[0];
    Inc(vIndex, 2);

    Assert(vTopicsByteLength[vCount] = Length(vTopicsAsBytes[vCount]));
    Move(vTopicsAsBytes[vCount][0], Result[vIndex],
      vTopicsByteLength[vCount]);
    Inc(vIndex, vTopicsByteLength[vCount]);
  end;
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