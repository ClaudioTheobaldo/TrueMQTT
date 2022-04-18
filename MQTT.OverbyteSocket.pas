unit MQTT.OverbyteSocket;

interface

uses
  System.SysUtils, System.Classes,
  OverbyteIcsWndControl, OverbyteIcsWSocket,
  MQTT.ISocket;

type
  TMQTTOverbyteSocket = class(TInterfacedObject, IMQTTSocket)
  strict private
    fSocket: TWSocket;

    fOnConnected: TNotifyEvent;
    fOnDisconnected: TNotifyEvent;
    fOnDataAvailable: TOnDataHandler;

    // Socket handlers
    procedure HandleDataAvailable(Sender: TObject; ErrCode: Word);
    procedure HandleSessionConnected(Sender: TObject; ErrCode: Word);
    procedure HandleSessionClosed(Sender: TObject; ErrCode: Word);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

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

{ TMQTTOverbyteSocket }

constructor TMQTTOverbyteSocket.Create;
begin
  fSocket := TWSocket.Create(nil);
  fSocket.OnSessionConnected := HandleSessionConnected;
  fSocket.OnSessionClosed := HandleSessionClosed;
  fSocket.OnDataAvailable := HandleDataAvailable;
end;

destructor TMQTTOverbyteSocket.Destroy;
begin
  FreeAndNil(fSocket);
  inherited;
end;

procedure TMQTTOverbyteSocket.HandleSessionConnected(Sender: TObject;
  ErrCode: Word);
begin
  if Assigned(fOnConnected) then
    fOnConnected(nil);
end;

procedure TMQTTOverbyteSocket.HandleSessionClosed(Sender: TObject;
  ErrCode: Word);
begin
  if Assigned(fOnDisconnected) then
    fOnDisconnected(nil);
end;

procedure TMQTTOverbyteSocket.HandleDataAvailable(Sender: TObject;
  ErrCode: Word);
var
  vClient: OverbyteIcsWSocket.TWSocket absolute Sender;
  vLen: Integer;
  vBytes: TBytes;
begin
  SetLength(vBytes, vClient.RcvdCount);
  vLen := vClient.Receive(vBytes, vClient.RcvdCount);
  if vLen > 0 then
  begin
    if Assigned(fOnDataAvailable) then
      fOnDataAvailable(vBytes);
  end
  else
    if Assigned(fOnDataAvailable) then
      fOnDataAvailable(nil);
end;

{ IMQTTSocket }

function TMQTTOverbyteSocket.Connect(const pHost: string;
  pPort: UInt16): Boolean;
begin
  if fSocket.State in [wsOpened..wsConnected] then
    Exit(True);

  fSocket.Addr := pHost;
  fSocket.Port := IntToStr(pPort);
  fSocket.Connect;
  Exit(True);
end;

procedure TMQTTOverbyteSocket.Disconnect;
begin
  if fSocket.State in [wsClosed] then
    Exit;
  fSocket.Close;
end;

function TMQTTOverbyteSocket.GetOnConnectedHandler: TNotifyEvent;
begin
  Result := fOnConnected;
end;

function TMQTTOverbyteSocket.GetOnDisconnectedHandler: TNotifyEvent;
begin
  Result := fOnDisconnected;
end;

function TMQTTOverbyteSocket.GetOnDataHandler: TOnDataHandler;
begin
  Result := fOnDataAvailable;
end;

function TMQTTOverbyteSocket.IsConnected: Boolean;
begin
  Result := fSocket.State = wsConnected;
end;

function TMQTTOverbyteSocket.Send(const pData: TBytes): Integer;
begin
  if (pData = nil) then
    Exit(0);
  if fSocket.State = wsConnected then
    Exit(fSocket.Send(@pData[0], Length(pData)))
  else
    Exit(0);
end;

procedure TMQTTOverbyteSocket.SetOnConnectedHandler(pHandler: TNotifyEvent);
begin
  fOnConnected := pHandler;
end;

procedure TMQTTOverbyteSocket.SetOnDisconnectedHandler(pHandler: TNotifyEvent);
begin
  fOnDisconnected := pHandler;
end;

procedure TMQTTOverbyteSocket.SetOnDataHandler(pHandler: TOnDataHandler);
begin
  fOnDataAvailable := pHandler;
end;

{ IMQTTSocket }

end.