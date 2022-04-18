unit MQTT.ISocket;

interface

uses
  System.SysUtils, System.Classes;

type
  TOnDataHandler = procedure(pBytes: TBytes) of object;

  IMQTTSocket = interface
  ['{03FDDC77-B360-42BD-ADDF-0978016299F5}']
    function Connect(const pHost: string; pPort: UInt16): Boolean;
    procedure Disconnect;

    function IsConnected: Boolean;

    function Send(const pData: TBytes): Integer;

    procedure SetOnConnectedHandler(pHandler: TNotifyEvent);
    procedure SetOnDisconnectedHandler(pHandler: TNotifyEvent);
    procedure SetOnDataHandler(pHandler: TOnDataHandler);
    function GetOnConnectedHandler: TNotifyEvent;
    function GetOnDisconnectedHandler: TNotifyEvent;
    function GetOnDataHandler: TOnDataHandler;
  end;

implementation

end.
