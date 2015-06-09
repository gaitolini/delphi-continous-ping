object frmPing: TfrmPing
  Left = 0
  Top = 0
  Caption = 'Ping Continu'
  ClientHeight = 749
  ClientWidth = 890
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
    Height = 302
    OnPaint = pbxPingPaint
  end
  object edIP: TEdit
    Left = 24
    Top = 8
    Width = 300
    Height = 44
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -30
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Text = '91.126.241.136'
  end
  object lbEvents: TListBox
    Left = 24
    Top = 383
    Width = 833
    Height = 305
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Lucida Console'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 0
  end
  object btnStartPing: TButton
    Left = 344
    Top = 8
    Width = 137
    Height = 61
    Caption = 'Start/stop Ping'
    TabOrder = 2
    OnClick = btnStartPingClick
  end
  object sbPing: TStatusBar
    Left = 0
    Top = 712
    Width = 890
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
    ExplicitWidth = 747
  end
  object btnSave2File: TButton
    Left = 504
    Top = 8
    Width = 209
    Height = 25
    Caption = 'Save To File'
    TabOrder = 4
    OnClick = btnSave2FileClick
  end
  object btnLoad4File: TButton
    Left = 504
    Top = 40
    Width = 209
    Height = 25
    Caption = 'Load From File'
    TabOrder = 5
    OnClick = btnLoad4FileClick
  end
  object TimerPing: TTimer
    OnTimer = TimerPingTimeout
    Left = 792
    Top = 144
  end
  object icmpClient: TIdIcmpClient
    OnStatus = IdcmpClientStatus
    Protocol = 1
    ProtocolIPv6 = 58
    IPVersion = Id_IPv4
    OnReply = IdcmpClientReply
    Left = 792
    Top = 80
  end
  object TimerMostrar: TTimer
    OnTimer = TimerMostrarTimeout
    Left = 792
    Top = 216
  end
  object opnLoadFile: TOpenDialog
    InitialDir = 'c:\Sebas\delphi\Ping_Continu\Win32\Debug'
    Left = 792
    Top = 288
  end
end
