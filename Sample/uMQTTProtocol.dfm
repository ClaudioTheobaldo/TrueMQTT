object frmMQTT: TfrmMQTT
  Left = 0
  Top = 0
  Caption = 'frmMQTT'
  ClientHeight = 659
  ClientWidth = 614
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblSubTopic: TLabel
    Left = 448
    Top = 7
    Width = 43
    Height = 13
    Caption = 'SubTopic'
  end
  object lblPubTopic: TLabel
    Left = 256
    Top = 103
    Width = 43
    Height = 13
    Caption = 'PubTopic'
  end
  object lblHost: TLabel
    Left = 112
    Top = 7
    Width = 22
    Height = 13
    Caption = 'Host'
  end
  object lblPort: TLabel
    Left = 112
    Top = 53
    Width = 20
    Height = 13
    Caption = 'Port'
  end
  object lblUnsubTopic: TLabel
    Left = 448
    Top = 53
    Width = 55
    Height = 13
    Caption = 'UnsubTopic'
  end
  object lblQoS: TLabel
    Left = 16
    Top = 264
    Width = 116
    Height = 13
    Caption = 'QoS (Quality of Service)'
  end
  object lblPubPayload: TLabel
    Left = 256
    Top = 149
    Width = 56
    Height = 13
    Caption = 'PubPayload'
  end
  object lblUsername: TLabel
    Left = 256
    Top = 7
    Width = 48
    Height = 13
    Caption = 'Username'
  end
  object lblPassword: TLabel
    Left = 256
    Top = 53
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object lblWillTopic: TLabel
    Left = 112
    Top = 149
    Width = 41
    Height = 13
    Caption = 'WillTopic'
  end
  object lblWillMessage: TLabel
    Left = 112
    Top = 195
    Width = 58
    Height = 13
    Caption = 'WillMessage'
  end
  object lblClientId: TLabel
    Left = 112
    Top = 103
    Width = 38
    Height = 13
    Caption = 'ClientID'
  end
  object btnSub: TButton
    Left = 16
    Top = 91
    Width = 75
    Height = 25
    Caption = 'Sub'
    TabOrder = 0
    OnClick = btnSubClick
  end
  object btnPub: TButton
    Left = 16
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Pub'
    TabOrder = 1
    OnClick = btnPubClick
  end
  object redt: TRichEdit
    Left = 8
    Top = 343
    Width = 601
    Height = 308
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Zoom = 100
  end
  object edtSubTopic: TEdit
    Left = 448
    Top = 26
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'dev/random'
  end
  object edtPubTopic: TEdit
    Left = 256
    Top = 122
    Width = 121
    Height = 21
    TabOrder = 4
    Text = 'dev/seventh'
  end
  object btnConnect: TButton
    Left = 16
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 5
    OnClick = btnConnectClick
  end
  object btnDisconnect: TButton
    Left = 16
    Top = 166
    Width = 75
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 6
    OnClick = btnDisconnectClick
  end
  object edtHost: TEdit
    Left = 112
    Top = 26
    Width = 121
    Height = 21
    TabOrder = 7
    Text = '127.0.0.1'
  end
  object edtPort: TEdit
    Left = 112
    Top = 72
    Width = 121
    Height = 21
    TabOrder = 8
    Text = '1833'
  end
  object btnUnsub: TButton
    Left = 16
    Top = 135
    Width = 75
    Height = 25
    Caption = 'Unsub'
    TabOrder = 9
    OnClick = btnUnsubClick
  end
  object edtUnsubTopic: TEdit
    Left = 448
    Top = 72
    Width = 121
    Height = 21
    TabOrder = 10
    Text = 'dev/random'
  end
  object cmbxQoS: TComboBox
    Left = 16
    Top = 283
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 11
  end
  object edtPubPayload: TEdit
    Left = 256
    Top = 168
    Width = 177
    Height = 21
    TabOrder = 12
    Text = 'Silly rabbit, tricks are for kids'
  end
  object btnGetCurrentSubscriptions: TButton
    Left = 16
    Top = 310
    Width = 153
    Height = 25
    Caption = 'GetCurrentSubscriptions'
    TabOrder = 13
    OnClick = btnGetCurrentSubscriptionsClick
  end
  object edtUsername: TEdit
    Left = 256
    Top = 26
    Width = 121
    Height = 21
    TabOrder = 14
    Text = 'claudio'
  end
  object edtPassword: TEdit
    Left = 256
    Top = 72
    Width = 121
    Height = 21
    TabOrder = 15
    Text = 'seventh'
  end
  object edtWillTopic: TEdit
    Left = 112
    Top = 168
    Width = 121
    Height = 21
    TabOrder = 16
    Text = 'dev/iwillbehere'
  end
  object edtWillMessage: TEdit
    Left = 112
    Top = 214
    Width = 121
    Height = 21
    TabOrder = 17
    Text = 'IAMHERE'
  end
  object edtClientId: TEdit
    Left = 112
    Top = 122
    Width = 121
    Height = 21
    TabOrder = 18
    Text = 'RandomClientID99'
  end
end
