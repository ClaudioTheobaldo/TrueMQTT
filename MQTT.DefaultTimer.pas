unit MQTT.DefaultTimer;

interface

uses
  System.Classes,
  Vcl.ExtCtrls,
  MQTT.ITimer;

type
  TMQTTDefaultTimer = class(TInterfacedObject, IMQTTTimer)
  strict private
    fTimer: TTimer;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure SetInterval(pMilliseconds: UInt32);
    function GetInterval: UInt32;
    procedure SetTimerHandler(pHandler: TNotifyEvent);
    function GetTimerHandler: TNotifyEvent;
    procedure Enable(pEnabled: Boolean);
  end;

implementation

uses
  System.SysUtils;

{ TMQTTDefaultTimer }

constructor TMQTTDefaultTimer.Create;
begin
  fTimer := TTimer.Create(nil);
  fTimer.Enabled := False;
end;

destructor TMQTTDefaultTimer.Destroy;
begin
  fTimer.OnTimer := nil;
  FreeAndNil(fTimer);
  inherited;
end;

procedure TMQTTDefaultTimer.SetInterval(pMilliseconds: UInt32);
begin
  fTimer.Interval := pMilliseconds;
end;

function TMQTTDefaultTimer.GetInterval: UInt32;
begin
  Exit(fTimer.Interval);
end;

function TMQTTDefaultTimer.GetTimerHandler: TNotifyEvent;
begin
  Result := fTimer.OnTimer;
end;

procedure TMQTTDefaultTimer.SetTimerHandler(pHandler: TNotifyEvent);
begin
  fTimer.OnTimer := pHandler;
end;

procedure TMQTTDefaultTimer.Enable(pEnabled: Boolean);
begin
  fTimer.Enabled := pEnabled;
end;

end.