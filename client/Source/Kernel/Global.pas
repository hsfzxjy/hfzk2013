unit Global;

interface

uses IniFiles, Sysutils, MSHtml, OleCtrls, SHDocVw;

const
  Ini_Name :string = '.\Main.INI';

var
  Host_URL: string;
  User_Query_URL: string;
  User_Login_URL: string;
  User_Operate_URL: string;
  Intf_Sina_URL: string;
  Intf_Call_URL: string;

function GetTextFromWebBrowser(wb: TWebBrowser): string;


implementation

function GetTextFromWebBrowser(wb: TWebBrowser): string;
begin
  result := (wb.Document as IHTMLDocument2).body.innerText;
end;

procedure Init;
var
  ini: TINIFile;
begin
  ini := TINIFile.Create(Ini_Name);
  Host_URL := ini.ReadString('URL', 'Host_URL', 'https://hfzkdebug.appspot.com');
  User_Query_URL := ini.ReadString('URL', 'User_Query_URL', Host_URL+'/user/query');
  User_Login_URL := ini.ReadString('URL', 'User_Login_URL', Host_URL+'/user/login');
  User_Operate_URL := ini.ReadString('URL', 'User_Operate_URL', Host_URL+'/user/operate');
  Intf_Sina_URL := ini.ReadString('URL', 'Intf_Sina_URL', Host_URL+'/intf/sina');
  Intf_Call_URL := ini.ReadString('URL','Intf_Call_URL', Host_URL+'/intf/call');
  ini.Free;
end;

procedure Uninit;
var
  ini: TINIFile;
begin
  ini := TINIFile.Create(Ini_Name);
  ini.WriteString('URL', 'Host_URL', Host_URL);
  ini.WriteString('URL', 'User_Query_URL', User_Query_URL);
  ini.WriteString('URL', 'User_Login_URL', User_Login_URL);
  ini.WriteString('URL', 'User_Operate_URL', User_Operate_URL);
  ini.WriteString('URL', 'Intf_Sina_URL', Intf_Sina_URL);
  ini.WriteString('URL','Intf_Call_URL', Intf_Call_URL);
  ini.Free;
end;

initialization
  Init;

finalization
   uninit;

end.
