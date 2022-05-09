unit MQTT.V311PacketBuilderTest;

interface

uses
  DUnitX.TestFramework,
  MQTT.V311PacketBuilder;

type
  [TestFixture]
  TV311PacketBuilderTest = class
  strict private
    fPacketBuilder: TMQTTV311PacketBuilder;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestBuildConnectPacket;
  end;

implementation

uses
  System.SysUtils, MQTT.Types;

procedure TV311PacketBuilderTest.Setup;
begin
  fPacketBuilder := TMQTTV311PacketBuilder.Create;
end;

procedure TV311PacketBuilderTest.TearDown;
begin
  FreeAndNil(fPacketBuilder);
end;

procedure TV311PacketBuilderTest.TestBuildConnectPacket;
var
  vPacket: TBytes;
begin
  // Arrange
  vPacket := fPacketBuilder.BuildConnectPacket(10,
    TConnectParams.Create(qlAtLeastOnceDelivery, False, EmptyStr,
    EmptyStr, EmptyStr, 'username', 'password'));

  //Act

  //Assert
  Assert.AreEqual(Length(vPacket), 34);
end;

initialization
  TDUnitX.RegisterTestFixture(TV311PacketBuilderTest);

end.
