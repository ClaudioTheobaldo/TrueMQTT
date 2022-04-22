unit uMQTTProtocol;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants,
  System.Classes, System.TypInfo,
  Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls,
  MQTT.Types, MQTT.Client;

type
  TfrmMQTT = class(TForm)
  published
    btnConnect: TButton;
    btnDisconnect: TButton;
    btnPub: TButton;
    btnSub: TButton;
    btnUnsub: TButton;
    btnGetCurrentSubscriptions: TButton;
    lblPubTopic: TLabel;
    lblPubPayload: TLabel;
    lblSubTopic: TLabel;
    lblUnsubTopic: TLabel;
    lblQoS: TLabel;
    lblClientId: TLabel;
    lblWillTopic: TLabel;
    lblWillMessage: TLabel;
    lblUsername: TLabel;
    lblPassword: TLabel;
    lblHost: TLabel;
    lblPort: TLabel;
    edtHost: TEdit;
    edtPort: TEdit;
    edtClientId: TEdit;
    edtWillTopic: TEdit;
    edtWillMessage: TEdit;
    edtUsername: TEdit;
    edtPassword: TEdit;
    edtPubTopic: TEdit;
    edtPubPayload: TEdit;
    edtSubTopic: TEdit;
    edtUnsubTopic: TEdit;
    cmbxQoS: TComboBox;
    redt: TRichEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnPubClick(Sender: TObject);
    procedure btnSubClick(Sender: TObject);
    procedure btnUnsubClick(Sender: TObject);
    procedure btnGetCurrentSubscriptionsClick(Sender: TObject);
  strict private
    fMQTTCli: TMQTTClient;
    procedure HandleConnected(Sender: TObject);
    procedure HandleDisconnected(Sender: TObject);
    procedure HandlePublishAcknowledge(pPacketIdentifier: UInt16; const pTopic: string);
    procedure HandlePublishComplete(pPacketIdentifier: UInt16; const pTopic: string);
    procedure HandlePublish(pPacketID: UInt16; const pTopic: string; pPayload: TBytes);
    procedure HandleSubscribeSuccess(pPacketIdentifier: UInt16; pQoSLevel: TQoSLevel; const pTopic: string);
    procedure HandleUnsubscribe(pPacketIdentifier: UInt16);
    procedure HandlePingResp(Sender: TObject);
  end;

var
  frmMQTT: TfrmMQTT;

implementation

uses
  MQTT.DefaultClientBuilder;

{$R *.dfm}

procedure TfrmMQTT.FormCreate(Sender: TObject);
var
  vQos: TQoSLevel;
begin
  Self.Constraints.MinHeight := Self.Height;
  Self.Constraints.MinWidth := Self.Width;
  Self.Constraints.MaxHeight := Self.Height;
  Self.Constraints.MaxWidth := Self.Width;

  for vQos := Low(TQosLevel) to High(TQoSLevel) do
    cmbxQoS.AddItem(GetEnumName(TypeInfo(TQoSLevel), Byte(vQoS)), nil);
  cmbxQos.ItemIndex := 1;

  fMQTTCli := TMQTTDefaultClientBuilder.NewDefaultClient(stOverbyte, 15);
  fMQTTCli.OnConnected := HandleConnected;
  fMQTTCli.OnDisconnected := HandleDisconnected;
  fMQTTCli.OnPuback := HandlePublishAcknowledge;
  fMQTTCli.OnPubcomp := HandlePublishComplete;
  fMQTTCli.OnPublish := HandlePublish;
  fMQTTCli.OnSubscribeSuccess := HandleSubscribeSuccess;
  fMQTTCli.OnUnsubscribe := HandleUnsubscribe;
  fMQTTCli.OnPingResp := HandlePingResp;
end;

procedure TfrmMQTT.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fMQTTCli);
end;

procedure TfrmMQTT.btnConnectClick(Sender: TObject);
begin
{
(const pHost: string; pPort: UInt16;
      const pClientID: string = ''; pWillQos: TQosLevel = qlAtMostOnceDelivery;
      pWillRetain: Boolean = False; const pWillTopic: string = '';
      const pWillMessage: string = ''; const pUsername: string = '';
      const pPassword: string = ''): Boolean; overload;
}
  if not fMQTTCli.Connect(edtHost.Text, StrToInt(edtPort.Text), edtClientId.Text,
    TQosLevel(cmbxQos.ItemIndex), False, edtWillTopic.Text, edtWillMessage.Text,
    edtUsername.Text, edtPassword.Text) then
    redt.Lines.Add('Already connected');
end;

procedure TfrmMQTT.btnDisconnectClick(Sender: TObject);
begin
  fMQTTCli.Disconnect;
end;

procedure TfrmMQTT.btnPubClick(Sender: TObject);
var
  vPayload: TBytes;
begin
  vPayload := TEncoding.UTF8.GetBytes(edtPubPayload.Text);
  fMQTTCli.Publish(edtPubTopic.Text, vPayload, False, TQosLevel(cmbxQos.ItemIndex));
end;

procedure TfrmMQTT.btnSubClick(Sender: TObject);
var
  vTopics: TArray<TTopicFilter>;
begin
  SetLength(vTopics, 1);
  vTopics[0].Topic := edtSubTopic.Text;
  vTopics[1].Qos := TQosLevel(cmbxQos.ItemIndex);
  fMQTTCli.Subscribe(vTopics);
end;

procedure TfrmMQTT.btnUnsubClick(Sender: TObject);
var
  vTopics: TArray<string>;
begin
  SetLength(vTopics, 1);
  vTopics[0] := edtUnsubTopic.Text;
  fMQTTCli.Unsubscribe(vTopics);
end;

procedure TfrmMQTT.btnGetCurrentSubscriptionsClick(Sender: TObject);
var
  vSList: TStringList;
  vCount: Integer;
begin
  vSList := fMQTTCli.GetCurrentSubscriptions;
  for vCount := 0 to vSList.Count - 1 do
    redt.Lines.Add(Format('Subscription[%d]: %s', [vCount, vSList[vCount]]));
  FreeAndNil(vSList);
end;

procedure TfrmMQTT.HandleConnected(Sender: TObject);
begin
  redt.Lines.Add(Format('Connected at: %s', [DateTimeToStr(now)]));
end;

procedure TfrmMQTT.HandleDisconnected(Sender: TObject);
begin
  redt.Lines.Add(Format('Disconnected at: %s', [DateTimeToStr(now)]));
end;

procedure TfrmMQTT.HandlePublishAcknowledge(pPacketIdentifier: UInt16;
  const pTopic: string);
begin
  redt.Lines.Add(Format('Publish packet [PacketID: %d | Topic: %s] has been acknowledged at %s',
    [pPacketIdentifier, pTopic, DateTimeToStr(now)]));
end;

procedure TfrmMQTT.HandlePublishComplete(pPacketIdentifier: UInt16;
  const pTopic: string);
begin
  redt.Lines.Add(Format('Publish packet [PacketID: %d | Topic: %s] has been completed at %s',
    [pPacketIdentifier, pTopic, DateTimeToStr(now)]));
end;

procedure TfrmMQTT.HandlePublish(pPacketID: UInt16; const pTopic: string;
  pPayload: TBytes);
var
  vPayload: string;
begin
  vPayload := TEncoding.UTF8.GetString(pPayload);
  redt.Lines.Add(Format('Publish packet [PacketID: %d | Topic: %s | Payload: %s]',
    [pPacketID, pTopic, vPayload]));
end;

procedure TfrmMQTT.HandleSubscribeSuccess(pPacketIdentifier: UInt16;
  pQoSLevel: TQoSLevel; const pTopic: string);
begin
  redt.Lines.Add(Format('Subscription successful! [PacketID: %d | QoS: %d | Topic: %s]',
    [pPacketIdentifier, Byte(pQoSLevel), pTopic]));
end;

procedure TfrmMQTT.HandleUnsubscribe(pPacketIdentifier: UInt16);
begin
  redt.Lines.Add(Format('Unsubscribe success. [PacketID: %d]',
    [pPacketIdentifier]));
end;

procedure TfrmMQTT.HandlePingResp(Sender: TObject);
begin
  redt.Lines.Add(Format('Ping received. Time: %s', [DateTimeToStr(now)]));
end;

end.
