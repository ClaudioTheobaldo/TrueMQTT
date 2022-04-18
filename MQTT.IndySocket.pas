unit MQTT.IndySocket;

interface

uses
  System.SysUtils, System.Classes,
  IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient,
  IdGlobal, IdThreadComponent,
  MQTT.ISocket;

type
  TMQTTIndySocket = class(TInterfacedObject, IMQTTSocket)
  strict private
    fSocket: TIdTCPClient;
    fComponent: TIdThreadComponent;

    fOnConnected: TNotifyEvent;
    fOnDisconnected: TNotifyEvent;
    fOnDataAvailable: TOnDataHandler;

    fConnected: Boolean;

    procedure HandleConnected(Sender: TObject);
    procedure HandleDisconnected(Sender: TObject);
    procedure HandleRun(Sender: TIdThreadComponent);
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

{ TMQTTIndySocket }

constructor TMQTTIndySocket.Create;
begin
  fSocket := TIdTCPClient.Create(nil);
  fSocket.OnConnected := HandleConnected;
  fSocket.OnDisconnected := HandleDisconnected;
  fSocket.UseNagle := True;
  fComponent := TIdThreadComponent.Create(nil);
  fComponent.OnRun := HandleRun;
end;

destructor TMQTTIndySocket.Destroy;
begin
  fSocket.Disconnect;
  FreeAndNil(fSocket);
  if fComponent.Active then
    fComponent.Stop;
  FreeAndNil(fComponent);
  inherited;
end;

function TMQTTIndySocket.Connect(const pHost: string; pPort: UInt16): Boolean;
begin
  fSocket.Connect(pHost, pPort);
  Exit(True);
end;

procedure TMQTTIndySocket.Disconnect;
begin
  fSocket.Disconnect;
end;

function TMQTTIndySocket.GetOnConnectedHandler: TNotifyEvent;
begin
  Result := fOnConnected;
end;

function TMQTTIndySocket.GetOnDisconnectedHandler: TNotifyEvent;
begin
  Result := fOnDisconnected;
end;

function TMQTTIndySocket.GetOnDataHandler: TOnDataHandler;
begin
  Result := fOnDataAvailable;
end;

procedure TMQTTIndySocket.HandleConnected(Sender: TObject);
begin
  fConnected := True;
  fComponent.Active := True;
  if Assigned(fOnConnected) then
    fOnConnected(nil);
end;

procedure TMQTTIndySocket.HandleDisconnected(Sender: TObject);
begin
  fConnected := False;
  fComponent.Active := False;
  if Assigned(fOnDisconnected) then
    fOnDisconnected(nil);
end;

procedure TMQTTIndySocket.HandleRun(Sender: TIdThreadComponent);
var
  vBytes: TBytes;
begin
  if fSocket.IOHandler.Readable then
  begin
    fSocket.IOHandler.CheckForDataOnSource(0);
    fSocket.IOHandler.ReadBytes(TIdBytes(vBytes), fSocket.IOHandler.InputBuffer.Size, True);
  end;
  if vBytes <> nil then
    if Assigned(fOnDataAvailable) then
      TThread.Queue(nil, procedure
        begin
          fOnDataAvailable(TBytes(vBytes));
        end);
end;

function TMQTTIndySocket.IsConnected: Boolean;
begin
  Result := fConnected;
end;

function TMQTTIndySocket.Send(const pData: TBytes): Integer;
begin
  Result := Length(pData);
  fSocket.IOHandler.Write(TIdBytes(pData));
end;

procedure TMQTTIndySocket.SetOnConnectedHandler(pHandler: TNotifyEvent);
begin
  fOnConnected := pHandler;
end;

procedure TMQTTIndySocket.SetOnDisconnectedHandler(pHandler: TNotifyEvent);
begin
  fOnDisconnected := pHandler;
end;

procedure TMQTTIndySocket.SetOnDataHandler(pHandler: TOnDataHandler);
begin
  fOnDataAvailable := pHandler;
end;

end.