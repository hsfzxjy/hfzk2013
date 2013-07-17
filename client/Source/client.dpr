program client;

uses
  Forms,
  Main in 'UI\Main.pas' {Form2},
  EditEx in 'UI\EditEx.pas' {RichEditEx: TFrame},
  LinkLabel in '..\Three-Part\LinkLabel\LinkLabel.pas',
  Cache in 'Kernel\Cache.pas',
  Data_Intf in 'Kernel\Data_Intf.pas',
  Global in 'Kernel\Global.pas',
  User_Intf in 'Kernel\User_Intf.pas',
  web_connect in 'Kernel\web_connect.pas',
  XMLObj in 'Kernel\XMLObj.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
