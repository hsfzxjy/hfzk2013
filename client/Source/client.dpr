program client;

uses
  Forms,
  Main in 'UI\Main.pas' {Form2},
  web_connect in 'Kernel\web_connect.pas',
  Cache in 'Kernel\Cache.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
