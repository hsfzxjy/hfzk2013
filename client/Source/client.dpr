program client;

uses
  Forms,
  Main in 'UI\Main.pas' {MainForm},
  Cache in 'Kernel\Cache.pas',
  Data_Intf in 'Kernel\Data_Intf.pas',
  Global in 'Kernel\Global.pas',
  User_Intf in 'Kernel\User_Intf.pas',
  web_connect in 'Kernel\web_connect.pas',
  XMLObj in 'Kernel\XMLObj.pas',
  SNSView in 'UI\SNSView.pas' {SNSViewer: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
