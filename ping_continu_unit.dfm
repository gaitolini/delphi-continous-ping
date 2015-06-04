object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Ping Continu'
  ClientHeight = 749
  ClientWidth = 747
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object pbxPing: TPaintBox
    Left = 24
    Top = 75
    Width = 690
    Height = 340
    OnPaint = pbxPingPaint
  end
  object edIP: TEdit
    Left = 24
    Top = 25
    Width = 300
    Height = 32
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Text = '91.126.241.136'
  end
  object lbEvents: TListBox
    Left = 24
    Top = 448
    Width = 681
    Height = 240
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Lucida Console'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 1
  end
  object btnStartPing: TButton
    Left = 424
    Top = 8
    Width = 137
    Height = 49
    Caption = 'StartPing'
    TabOrder = 2
    OnClick = btnStartPingClick
  end
  object sbPing: TStatusBar
    Left = 0
    Top = 712
    Width = 747
    Height = 37
    Panels = <
      item
        Text = 'Ping Status'
        Width = 400
      end
      item
        Text = 'Timestamp'
        Width = 150
      end
      item
        Text = 'not used yet'
        Width = 50
      end>
  end
  object TimerPing: TTimer
    OnTimer = TimerPingTimeout
    Left = 576
    Top = 24
  end
  object icmpClient: TIdIcmpClient
    OnStatus = IdcmpClientStatus
    Protocol = 1
    ProtocolIPv6 = 58
    IPVersion = Id_IPv4
    OnReply = IdcmpClientReply
    Left = 360
    Top = 24
  end
  object TimerMostrar: TTimer
    OnTimer = TimerMostrarTimeout
    Left = 672
    Top = 24
  end
end
