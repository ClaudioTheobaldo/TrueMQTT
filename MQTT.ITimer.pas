unit MQTT.ITimer;

interface

uses
  System.SysUtils, System.Classes;

type
  IMQTTTimer = interface
  ['{0060813E-5F09-40D9-8B84-7BECF2614406}']
    procedure SetInterval(pMilliseconds: UInt32);
    function GetInterval: UInt32;
    procedure SetTimerHandler(pHandler: TNotifyEvent);
    function GetTimerHandler: TNotifyEvent;
    procedure Enable(pEnabled: Boolean);
  end;

implementation

end.