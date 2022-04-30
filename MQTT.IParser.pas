unit MQTT.IParser;


interface

uses
  System.SysUtils, System.Classes,
  MQTT.Types;

type
  IParser = interface
  ['{40E28F2F-C25D-4774-959A-54FA4DF1B2CB}']
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

end.
