object frmMQTT: TfrmMQTT
  Left = 0
  Top = 0
  Caption = 'frmMQTT'
  ClientHeight = 484
  ClientWidth = 632
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
    Left = 488
    Top = 7
    Width = 43
    Height = 13
    Caption = 'SubTopic'
  end
  object lblPubTopic: TLabel
    Left = 280
    Top = 7
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
    Left = 488
    Top = 53
    Width = 55
    Height = 13
    Caption = 'UnsubTopic'
  end
  object lblQoS: TLabel
    Left = 112
    Top = 104
    Width = 116
    Height = 13
    Caption = 'QoS (Quality of Service)'
  end
  object lblPubPayload: TLabel
    Left = 280
    Top = 53
    Width = 56
    Height = 13
    Caption = 'PubPayload'
  end
  object btnSub: TButton
    Left = 16
    Top = 70
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
    Top = 221
    Width = 609
    Height = 255
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
    Left = 488
    Top = 26
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'dev/random'
  end
  object edtPubTopic: TEdit
    Left = 280
    Top = 26
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
    Top = 132
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
    Top = 101
    Width = 75
    Height = 25
    Caption = 'Unsub'
    TabOrder = 9
    OnClick = btnUnsubClick
  end
  object edtUnsubTopic: TEdit
    Left = 488
    Top = 72
    Width = 121
    Height = 21
    TabOrder = 10
    Text = 'dev/random'
  end
  object cmbxQoS: TComboBox
    Left = 112
    Top = 123
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 11
  end
  object edtPubPayload: TEdit
    Left = 280
    Top = 72
    Width = 177
    Height = 21
    TabOrder = 12
    Text = 'Silly rabbit, tricks are for kids'
  end
  object btnGetCurrentSubscriptions: TButton
    Left = 16
    Top = 190
    Width = 153
    Height = 25
    Caption = 'GetCurrentSubscriptions'
    TabOrder = 13
    OnClick = btnGetCurrentSubscriptionsClick
  end
end
