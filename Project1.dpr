program Project1;

uses
  Forms,
  SVNGI in 'SVNGI.pas' {Form_main},
  Unit2 in 'Unit2.pas' {Form_login};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm_main, Form_main);
  Application.CreateForm(TForm_login, Form_login);
  Application.Run;
end.
