unit ping_continu_unit;

// github : https://github.com/sebastianet/delphi-continous-ping
// versions
//  1.1.a 04 Juny 2015 - do ping on timeout schedule
//  1.1.b 05 Juny 2015 - save data to file, load data from file (and display)

{$M+}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.DateUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  IdIcmpClient, IdBaseComponent, IdComponent, IdRawBase, IdRawClient,
  typinfo,
  Vcl.ComCtrls ;

const

	ksz_Id       = 'Ping Continu' ;
	ksz_Versio   = 'v 1.1.b, 04 Juny 2015' ;
	ksz_Autor    = 'Sebastia Altemir Gubankov' ;
	ksz_Emilio   = 'sag@tinet.cat' ;

	kTimeout_ICMP     = 1500 ;    // period of the timer to send a ping()
	kTimeoutMostrar   = 60000 ;   // we update the canvas every minute
	kTimeoutTimerPing = 5000 ;
	kLlindar          = 10 ;      // si de 12 pings hi ha 10 o mes ecos, then GREEN

	dotsize = 10 ;
	margin  = 20 ;

	bVerbose = false ;

type

  TdayPings = array [ 0..23, 0..59 ] of integer ; // [hora, minut]

  TForm1 = class(TForm)
    TimerPing: TTimer;
    edIP: TEdit;
    lbEvents: TListBox;
    btnStartPing: TButton;
    icmpClient: TIdIcmpClient;
    pbxPing: TPaintBox;
    TimerMostrar: TTimer;
    sbPing: TStatusBar;
    btnSave2File: TButton;
    btnLoad4File: TButton;

    procedure FormCreate(Sender: TObject);
    procedure TimerPingTimeout(Sender: TObject);
    procedure btnStartPingClick(Sender: TObject);
    procedure IdcmpClientReply(ASender: TComponent;
      const AReplyStatus: TReplyStatus);
    procedure IdcmpClientStatus(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);
    procedure pbxPingPaint(Sender: TObject);
    procedure TimerMostrarTimeout(Sender: TObject);
    procedure btnSave2FileClick(Sender: TObject);
    procedure btnLoad4FileClick(Sender: TObject);

  private
    { Private declarations }
    dayPings : TdayPings ; // 0 = unassigned, <0 = error codes, >0 = ping delay

  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  iEco, iError, iTimeout : integer ;


implementation

{$R *.dfm}

procedure debugMsg ( s:string ) ;
const
  kMaxNumofItems = 100 ;
  kNumItemstoDelete = 25 ;
var
  i : integer ;

begin

//  if not bLB_Trassa_Enabled then Exit ;

  with Form1.lbEvents do begin

    Items.add ( dateTimeToStr ( now ) + ' ' + s ) ;
      ItemIndex := Count - 1 ; // focus on last item
      ItemIndex := -1 ; // no focus

// delete if too many elements :
    if Items.Count > kMaxNumofItems then begin
      for i:=1 to kNumItemstoDelete do
        Items.Delete(0);
    end;

  end;
end; // debugMsg


procedure TForm1.btnLoad4FileClick(Sender: TObject);
var
  fnIn: TextFile;
  szFN : string ;
  iData: integer ;
  ho, mi : integer ;

begin

  szFN := '150605141816.cpd' ;

  if FileExists( szFN ) then begin

    AssignFile( fnIn, szFN );
    Reset( fnIn ); // open file to read
    ho := 0 ;
    mi := 0 ;

    while not Eof( fnIn) do
    begin
      ReadLn( fnIn, iData );
      dayPings [ ho, mi ] := iData ;
      mi := mi + 1 ;
      if mi > 59  then begin
        mi := 0 ;
        ho := ho + 1 ;
      end ;

    end; // while not EOF()

    CloseFile( fnIn );

    debugMsg ( '+++ Input ('+ IntToStr(ho)+ ') files.' ) ;

    pbxPing.Repaint ; // update PaintBox

  end
  else
    debugMsg ( '--- Input filename ('+szFN+ ') not found.' ) ;

end; // btnLoad4FileClick - load data from file - selected filename


procedure TForm1.btnSave2FileClick(Sender: TObject);
var
  fOut: TextFile;
  szFN : string ;
  ho, mi : integer ;

begin

  DateTimeToString ( szFN, 'yymmddhhnnss', now ) ;
  szFN := szFN + '.cpd' ; // Continous Ping Data
  debugMsg ( '>>> Output Filename will be ('+szFN+ ').' ) ;

  AssignFile( fOut, szFN ) ; // assign filename to file
  Rewrite( fOut ) ;          // open file to write

  for ho := 0 to 23 do
    for mi := 0 to 59 do
      WriteLn( fout, dayPings [ ho, mi ] ) ;

  CloseFile( fOut ) ; // 4.320 char file in \\delphi\Ping_Continu\Win32\Debug

end; // btnSave2FileClick - write data to file - calculated filename


procedure TForm1.btnStartPingClick(Sender: TObject);
var szIP : string ;
begin

  debugMsg ( 'Button Start/Stop Ping pushed.' ) ;

  szIP := edIP.Text ;
  debugMsg ( 'Set IP (' + szIP + '). Timeout "Ping Msg" is (' + IntToStr( TimerPing.Interval ) +') msg.' ) ;
  icmpClient.Host := szIP ;
  icmpClient.ReceiveTimeout := kTimeout_ICMP ;

  if TimerPing.Enabled then begin
    TimerPing.Enabled := false ;
    btnStartPing.Caption := 'Start Timer Ping' ;
  end
  else begin // disabled
    TimerPing.Enabled := true ;
    btnStartPing.Caption := 'Stop Timer Ping' ;
  end;

  TimerMostrar.Enabled := true ;
  debugMsg ( 'Timeout "Display Data" is (' + IntToStr( TimerMostrar.Interval ) +') msg.' ) ;

end; // btnStartPingClick


procedure TForm1.FormCreate(Sender: TObject);
var
  ho, mi : integer ;
begin

  TimerPing.Enabled  := false ; // wait for button
  TimerPing.Interval := kTimeoutTimerPing ;

  TimerMostrar.Enabled  := false ;
  TimerMostrar.Interval := kTimeoutMostrar ;

  for ho := 0 to 23 do
    for mi := 0 to 59 do
      dayPings [ ho, mi ] := 0 ; // init array [ hora, minut ]

end;


procedure TForm1.pbxPingPaint(Sender: TObject);
var m, h : integer ;
begin

  debugMsg ( '*** PBX Paint event.' ) ;

  with pbxPing do begin

    Canvas.brush.style := bsClear ;
    Canvas.Font.Size   := 6 ;

    for m := 0 to 60 do
      if (m=0) or (m=15) or (m=30) or (m=45) or (m=60) then
        Canvas.Textout( margin+m*(dotsize+1), 1, inttostr(m) ) ; // minutes in horizontal

    for h := 0 to 24 do
      if (h=0) or (h=6) or (h=12) or (h=18) or (h=24) then
        Canvas.Textout( 1, margin+h*(dotsize+1), inttostr(h) ) ; // hours in vertical

    for h := 0 to 23 do
    begin
      for m := 0 to 59 do
      begin
        Canvas.brush.style := bsSolid ;
        Canvas.Pen.Style   := psClear ;

        if dayPings [h,m] = 0 then Canvas.brush.color := clWhite ; // unassigned
        if dayPings [h,m] > 0 then Canvas.brush.color := clGreen ; // there is an answer, with a  delay
        if dayPings [h,m] < 0 then Canvas.brush.color := clRed ;   // no answer but error code

        Canvas.Rectangle( margin+m*(dotsize+1), margin+h*(dotsize+1), margin-1+(m+1)*(dotsize+1), margin-1+(h+1)*(dotsize+1) ) ;

      end; // m (0..59)
    end; // h (0..23)

  end; // with PaintBox

end;


procedure TForm1.IdcmpClientStatus( ASender: TObject; const AStatus: TIdStatus; const AStatusText: string );
begin

  debugMsg ( '+++ Ping() Status ('+ AStatusText + ').' ) ;

end; // IdcmpClientStatus


procedure TForm1.IdcmpClientReply( ASender: TComponent; const AReplyStatus: TReplyStatus );
// Type ptypeinfo = ^ttypeinfo;
var
  iSeqId, iBytesRcvd : integer ;
  szRspIP   : string ;

  iRepQ      : integer ;
  pInfo      : PTypeInfo ; // requires "uses typinfo"
  szEnumName : string ;

begin
  iSeqId      := AReplyStatus.SequenceId ;
  iBytesRcvd  := AReplyStatus.BytesReceived ;
  szRspIP     := AReplyStatus.FromIpAddress ;

  iRepQ      := ord( AReplyStatus.ReplyStatusType ) ;
  pInfo      := System.TypeInfo( TReplyStatus ) ; // returns a pointer to a TTypeInfo record.
  szEnumName := '-' ; // set default value
  if pInfo <> nil then // there is no RTTI attached for some types like Records ...
  begin
//    szEnumName := TypInfo.GetEnumName( pInfo, iRepQ ) ; // runtime error
  end;

// S := typinfo.getenuname( system.typeinfo( tstatustype), ord(replystatus) ) );

  if bVerbose then debugMsg ( '+++ Ping() reply, status (' + szEnumName + ').' ) ;
  sbPing.Panels[0].Text := TimeToStr ( now ) + '+++ Ping() reply, status ordinal (' + IntToStr( iRepQ ) + ').' ;

  case ( AReplyStatus.ReplyStatusType ) of  // ms-help://embarcadero.rs_xe7/Indy/IdIcmpClient_TReplyStatusTypes.html
                                            // (rsEcho, rsError, rsTimeOut, rsErrorUnreachable, rsErrorTTLExceeded);
    rsEcho : begin
      iEco := iEco + 1 ;
      debugMsg ( '+++ Ping() reply rsECHO {'+IntToStr(iEco)+'}, seq [' + IntToStr(iSeqId) + '], bytes ['+IntToStr(iBytesRcvd)+'].' ) ;
    end ;
    rsError: begin
      iError := iError + 1 ;
      debugMsg ( '+++ Ping() reply rsERROR {'+IntToStr(iError)+'}.' ) ;
    end;
    rsTimeOut: begin
      iTimeout := iTimeout + 1 ;
      debugMsg ( '+++ Ping() reply rsTIMEOUT {'+IntToStr(iTimeout)+'}.' ) ;
    end;

  end; // case

end; // IdcmpClientReply


procedure TForm1.TimerMostrarTimeout(Sender: TObject);
var h, m : integer ;

begin

  debugMsg ( '*** +++ Timer mostrar resultat ultim periode a PaintBox.' ) ;
  h := HourOf ( Now ) ;   // 0..23
  m := MinuteOf ( Now ) ; // 0..59

  with pbxPing do begin

    if iEco >= kLlindar then begin
      dayPings [h,m] := 1 ; // maybe delay, but always >0
//      Canvas.brush.color := clGreen
    end
    else begin
      dayPings [h,m] := -1 ; // maybe RC, but always <0
//      Canvas.brush.color := clRed ;
    end ;

//    Canvas.Rectangle( margin+m*(dotsize+1), margin+h*(dotsize+1), margin-1+(m+1)*(dotsize+1), margin-1+(h+1)*(dotsize+1) ) ;
    pbxPing.Repaint ; // update PaintBox

  end ; // with PaintBox

  iEco := 0 ;
  iError := 0 ;
  iTimeout := 0 ;

  sbPing.Panels[1].Text := DateTimeToStr ( now ) ; // update timestamp

end;

procedure TForm1.TimerPingTimeout(Sender: TObject);
begin

  if bVerbose then debugMsg ( '*** +++ Timer Ping timeout - Ping() comença.' ) ;
  try
    icmpClient.Ping() ;
  except
    on e:Exception do
      debugMsg ( '--- Ping exception,, msg {' + e.Message + '}.' ) ;
  end;
  if bVerbose then debugMsg ( '*** --- Timer Ping timeout - Ping() acaba.' ) ;

end;

end.
