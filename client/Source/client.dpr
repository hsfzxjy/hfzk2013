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
  XMLObj in 'Kernel\XMLObj.pas',
  AccountsView in 'UI\AccountsView.pas' {AccountsViewer: TFrame},
  MainHelper in 'UI\MainHelper.pas',
  FrmLargeImage in 'UI\Dialogs\FrmLargeImage.pas' {LargeImageForm},
  ViewerFactory in 'UI\ViewerFactory.pas',
  CustomViewFrame in 'UI\CustomViewFrame.pas' {CustomViewerFrame: TFrame},
  Test in 'Test.pas' {Form1},
  UserView in 'UI\UserView.pas' {UserViewer: TFrame},
  WBViewer in 'UI\WBViewer.pas' {CustomViewerFrame1: TFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True; 
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  // Application.CreateForm(TLargeImageForm, LargeImageForm);
 // Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
