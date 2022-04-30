unit MQTT.V311Parser;

interface

uses
  System.SysUtils, System.Classes,
  MQTT.Types, MQTT.IParser;

type
  TMQTTV311Parser = class(TInterfacedObject, IParser)
  strict private
    fBuffer: TBytes;

    fOnConnack: TConnackEvent;
    fOnPuback: TPacketIDEvent;
    fOnPubrec: TPacketIDEvent;
    fOnPubComp: TPacketIDEvent;
    fOnPublish: TPublishEvent;
    fOnSuback: TSubackEvent;
    fOnUnsuback: TPacketIDEvent;
    fOnPing: TNotifyEvent;

    procedure ParseConnack(pPacket: TBytes; pLen: Integer);
    procedure ParsePuback(pPacket: TBytes; pLen: Integer);
    procedure ParsePubrec(pPacket: TBytes; pLen: Integer);
    procedure ParsePubComp(pPacket: TBytes; pLen: Integer);
    procedure ParsePublish(pPacket: TBytes; pLen, pRemainingLengthSize: Integer);
    procedure ParseSuback(pPacket: TBytes; pLen, pRemainingLengthSize: Integer);
    procedure ParseUnsuback(pPacket: TBytes; pLen: Integer);
    procedure ParsePingResp;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure InsertData(pData: TBytes);

    procedure SetOnConnack(pEvent: TConnackEvent);
    procedure SetOnPuback(pEvent: TPacketIDEvent);
    procedure SetOnPubrec(pEvent: TPacketIDEvent);
    procedure SetOnPubcomp(pEvent: TPacketIDEvent);
    procedure SetOnPublish(pEvent: TPublishEvent);
    procedure SetOnSuback(pEvent: TSubackEvent);
    procedure SetOnUnsuback(pEvent: TPacketIDEvent);
    procedure SetOnPing(pEvent: TNotifyEvent);
  end;

implementation

uses
  MQTT.Utils;

{ TMQTTV311Parser }

constructor TMQTTV311Parser.Create;
begin
  fBuffer := [];
end;

destructor TMQTTV311Parser.Destroy;
begin
  inherited;
end;

procedure TMQTTV311Parser.ParseConnack(pPacket: TBytes; pLen: Integer);
begin
  if (pLen <> 4) then
    Exit;

  if Assigned(fOnConnack) then
    fOnConnack(TConnackReturnCodes(pPacket[3]));
end;

procedure TMQTTV311Parser.ParsePuback(pPacket: TBytes; pLen: Integer);
var
  vPacketID: UInt16;
begin
  if (pLen < 4)then
    Exit;

  PByte(@vPacketID)[0] := pPacket[3]; //LSB
  PByte(@vPacketID)[1] := pPacket[2]; //MSB
  if Assigned(fOnPuback) then
    fOnPuback(vPacketID);
end;

procedure TMQTTV311Parser.ParsePubrec(pPacket: TBytes; pLen: Integer);
var
  vPacketID: UInt16;
begin
  if (pLen <> 4)then
    Exit;

  PByte(@vPacketID)[0] := pPacket[3]; //LSB
  PByte(@vPacketID)[1] := pPacket[2]; //MSB
  if Assigned(fOnPubrec) then
    fOnPubrec(vPacketID);
end;

procedure TMQTTV311Parser.ParsePubComp(pPacket: TBytes; pLen: Integer);
var
  vPacketID: UInt16;
begin
  if (pLen < 4)then
    Exit;

  PByte(@vPacketID)[0] := pPacket[3]; //LSB
  PByte(@vPacketID)[1] := pPacket[2]; //MSB
  if Assigned(fOnPubComp) then
    fOnPubComp(vPacketID);
end;

procedure TMQTTV311Parser.ParsePublish(pPacket: TBytes; pLen, pRemainingLengthSize: Integer);
const
  c_RetainMask = 1;
  c_QosMask = 6;
  c_DupMask = 8;
  c_PacketTypeMask = 240;
var
  vQoS: TQoSLevel;
  //vRetain, vDup: Boolean;
  vPacketType: TMQTTControlPacket;
  vRemainingLength, vIndex, vPayloadSize: Integer;
  vTopicLen, vPacketID: UInt16;
  vTopicAsBytes, vPayload: TBytes;
  vTopic: string;
begin
  if pLen < 5 then // Not sure about this one...
    Exit;

  //vRetain := (pPacket[0] and c_RetainMask) = 1;
  vQos := TQoSLevel((pPacket[0] and c_QosMask) shr 1);
  //vDup := ((pPacket[0] and c_DupMask) shr 3) = 1;
  vPacketType := TMQTTControlPacket((pPacket[0] and c_PacketTypeMask) shr 4);

  if vPacketType <> TMQTTControlPacket.PUBLISH then
    Exit;

  vIndex := 1 + pRemainingLengthSize;
  vRemainingLength := pLen - vIndex;

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

procedure TMQTTV311Parser.ParseSuback(pPacket: TBytes; pLen, pRemainingLengthSize: Integer);
const
  c_SubackFailure = $80;
var
  vPacketIdentifier: UInt16;
  vCount, vIndex: Integer;
  vReturnCodeArr: TArray<Integer>;
begin
  if pLen <= 4 then
    Exit;

  vIndex := 1 + pRemainingLengthSize;

  PByte(@vPacketIdentifier)[1] := pPacket[vIndex]; // MSB
  PByte(@vPacketIdentifier)[0] := pPacket[vIndex + 1]; // LSB
  Inc(vIndex, 2);

  SetLength(vReturnCodeArr, pLen - vIndex);
  for vCount := vIndex to pLen - 1 do
    vReturnCodeArr[vCount - vIndex] := pPacket[vCount];

  if Assigned(fOnSuback) then
    fOnSuback(vPacketIdentifier, vReturnCodeArr);
end;

procedure TMQTTV311Parser.ParseUnsuback(pPacket: TBytes; pLen: Integer);
var
  vPacketIdentifier: UInt16;
begin
  if pLen < 4 then
    Exit;

  PByte(@vPacketIdentifier)[1] := pPacket[2]; // MSB
  PByte(@vPacketIdentifier)[0] := pPacket[3]; // LSB
  if Assigned(fOnUnsuback) then
    fOnUnsuback(vPacketIdentifier);
end;

procedure TMQTTV311Parser.SetOnConnack(pEvent: TConnackEvent);
begin
  fOnConnack := pEvent;
end;

procedure TMQTTV311Parser.SetOnPing(pEvent: TNotifyEvent);
begin
  fOnPing := pEvent;
end;

procedure TMQTTV311Parser.SetOnPuback(pEvent: TPacketIDEvent);
begin
  fOnPuback := pEvent;
end;

procedure TMQTTV311Parser.SetOnPubcomp(pEvent: TPacketIDEvent);
begin
  fOnPubcomp := pEvent;
end;

procedure TMQTTV311Parser.SetOnPublish(pEvent: TPublishEvent);
begin
  fOnPublish := pEvent;
end;

procedure TMQTTV311Parser.SetOnPubrec(pEvent: TPacketIDEvent);
begin
  fOnPubrec := pEvent;
end;

procedure TMQTTV311Parser.SetOnSuback(pEvent: TSubackEvent);
begin
  fOnSuback := pEvent;
end;

procedure TMQTTV311Parser.SetOnUnsuback(pEvent: TPacketIDEvent);
begin
  fOnUnsuback := pEvent;
end;

procedure TMQTTV311Parser.ParsePingResp;
begin
  if Assigned(fOnPing) then
    fOnPing(Self);
end;

procedure TMQTTV311Parser.InsertData(pData: TBytes);
var
  vPacketType: TMQTTControlPacket;
  vRemainingLength, vRemainingLengthSize, vDataLen: Integer;
begin
  if (pData = nil) then
    Exit;

  if Length(pData) < 2 then
    Exit;

  fBuffer := fBuffer + pData;
  vDataLen := Length(fBuffer);

  vPacketType := TMQTTControlPacket((fBuffer[0] and $F0) shr 4);
  vRemainingLength := DecodeVarInt32(@fBuffer[1], vRemainingLengthSize);

  if (1 + vRemainingLengthSize + vRemainingLength) < vDataLen then
    Exit; // Incomplete packet, wait for more data

  try
    case vPacketType of
      TMQTTControlPacket.CONNACK: ParseConnack(fBuffer, vDataLen);
      TMQTTControlPacket.PUBLISH: ParsePublish(fBuffer, vDataLen, vRemainingLengthSize);
      TMQTTControlPacket.PUBACK: ParsePuback(fBuffer, vDataLen);
      TMQTTControlPacket.PUBREC: ParsePubrec(fBuffer, vDataLen);
      TMQTTControlPacket.PUBCOMP: ParsePubComp(fBuffer, vDataLen);
      TMQTTControlPacket.SUBACK: ParseSuback(fBuffer, vDataLen, vRemainingLengthSize);
      TMQTTControlPacket.UNSUBACK: ParseUnsuback(fBuffer, vDataLen);
      TMQTTControlPacket.PINGRESP: ParsePingResp;
    end;
  finally
    fBuffer := [];
  end;

end;

end.