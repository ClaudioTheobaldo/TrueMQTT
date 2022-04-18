unit MQTT.Utils;

interface

uses
  System.SysUtils;

procedure SetNthBit(var pByte: Byte; pIndex: Integer); inline;
function EncodeVarInt32(pInt: Integer): TBytes;
function DecodeVarInt32(pBytes: PByte; var pSize: Integer): Integer;

implementation

procedure SetNthBit(var pByte: Byte; pIndex: Integer); inline;
begin
  pByte := pByte or (1 shl (pIndex));
end;

function EncodeVarInt32(pInt: Integer): TBytes;
var
  vValue, vInt: Integer;
begin
  vInt := 0;
  vValue := pInt;

  while vValue > 0 do
  begin
    vValue := vValue shr 7;
    Inc(vInt);
  end;

  SetLength(Result, vInt);

  vInt := 0;
  while True do
  begin
    vValue := pInt and 127;
    pInt := pInt shr 7;
    if pInt > 0 then
      Result[vInt] := vValue or 128
    else
    begin
      Result[vInt] := vValue;
      Break;
    end;
    Inc(vInt);
  end;
end;

function DecodeVarInt32(pBytes: PByte; var pSize: Integer): Integer;
var
  vEncodedByte: Byte;
  vMulti, vIndex: Integer;
begin
  vMulti := 1;
  Result := 0;
  vIndex := 0;

  while True do
  begin
    vEncodedByte := pBytes[vIndex];
    Result := Result + ((vEncodedByte and 127) * vMulti);

    vMulti := vMulti * 128;
    Inc(vIndex);

    if vMulti > 128*128*128 then
      raise Exception.Create('Malformed remaining length');

    if (vEncodedByte and 128) = 0 then
    begin
      pSize := vIndex;
      Break;
    end;
  end;
end;


end.
