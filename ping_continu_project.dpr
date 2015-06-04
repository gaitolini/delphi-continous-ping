program ping_continu_project;

uses
  Vcl.Forms,
  ping_continu_unit in 'ping_continu_unit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
