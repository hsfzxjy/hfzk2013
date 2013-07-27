program client;

uses
  Forms,
  Main in 'UI\Main.pas' {MainForm},
  CustomView in 'UI\CustomView.pas' {CustomViewer: TFrame},
  SNSView in 'UI\SNSView.pas' {SNSViewer: TFrame},
  Cache_ in 'Kernel\Cache_.pas',
  Data_Intf in 'Kernel\Data_Intf.pas',
  Global in 'Kernel\Global.pas',
  User_Intf in 'Kernel\User_Intf.pas',
  web_connect in 'Kernel\web_connect.pas',
  XMLObj in 'Kernel\XMLObj.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
