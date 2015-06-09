program ping_continu_project;

uses
  Vcl.Forms,
  ping_continu_unit in 'ping_continu_unit.pas' {frmPing};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPing, frmPing);
  Application.Run;
end.
