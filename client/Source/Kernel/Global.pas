unit Global;

interface

uses
  Classes, IniFiles, Sysutils, MSHtml, OleCtrls, SHDocVw, NativeXML, IdURI,
  Forms, Graphics, frmWaiting;

type
  TAccount_Type = (atSina, atTwitter, atFacebook);

  TCustomContext = class(TPersistent)
  public
    procedure LoadFromStream(s: TStream); dynamic; abstract;
    procedure SaveToStream(s: TStream); dynamic; abstract;
  end;

  TCustomContextClass = class of TCustomContext;

const
  Ini_Name :string = '.\Main.INI';
  TypeStrings : array [atSina..atFacebook] of String = ('sina','twitter','facebook');

var
  Host_URL: string;

  User_Query_URL: string;
  User_Login_URL: string;
  User_Operate_URL: string;

  Account_Query_URL: string;
  Account_Modify_URL: string;

  Intf_Sina_URL: string;
  Intf_Twitter_URL: string;
  Intf_Facebook_URL: string;
  Intf_Call_URL: string;
  Intf_Sep_URL: string;
  Intf_Smart_URL: string;

  Emotion_Sina_Path: string;
  Image_Path: string;
  Cache_Path: string;

  ExePath: string;

//WebBrowser functions
function GetHTMLFromWebBrowser(wb: TWebBrowser): string;
function GetTextFromWebBrowser(wb: TWebBrowser): string;

//String Encode-Decode functions
function Utf8ToWide(s: UTF8String): WideString;
function WideToUtf8(s: WideString): UTF8String;

//URLs functions
function URLParamEncode(param: string): string;
function GetPinCode(s: string): string;
function GetFullURLPath(url: string): string;

//Images functions
function LoadPicture(ms: TStream): TGraphic;
function GetPictureFromPath(path: string): TGraphic;

//Hash functions
function Hash(s: string): string;
function Unhash(s: string): string;

implementation

uses PerlRegEx, pngimage, gifimg, jpeg;

function Hash(s: string): string;
var
  ch: char;
begin
  result := '';
  for ch in s do
    result := result+IntToHex(Ord(ch), 2);
end;

function Unhash(s: string): string;
var
  str: char;
  i: integer;
begin
  result := '';
  for i := 1 to length(s) div 2 do
    result := result + Chr(StrToInt('$'+Copy(s,i*2-1,2)));
end;

function GetPictureFromPath(path: string): TGraphic;
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    ms.LoadFromFile(path);
    result := LoadPicture(ms);
  finally
    ms.Free;
  end;
end;

function LoadPicture(ms: TStream): TGraphic;
var
  Buffer: Word;
begin
  result := nil;
  if ms.Size = 0 then exit;
  ms.Position := 0;
  ms.ReadBuffer(Buffer, 2);

  case Buffer of
    $4D42: result := TBitmap.Create;
    $D8FF: result := TJPEGImage.Create;
    $4947: result := TGifImage.Create;
    $5089: result := TPNGObject.Create;
  end;
  ms.Position := 0;
  
  if Assigned(result) then
    result.LoadFromStream(ms);
end;

function GetTextFromWebBrowser(wb: TWebBrowser): string;
begin
  result := (wb.Document as IHTMLDocument2).body.innerText;
end;

function GetFullURLPath(url: string): string;
var
  p: Integer;
begin
  p := Pos('?', url);
  if p <= 0 then
    result := url
  else
    result := Copy(url, 1, p-1);
end;

function GetPinCode(s: string): string;
const
  exp = '<code>[0-9]{7}</code>';
  exp2 = '[0-9]{7}';
var
  reg: TPerlRegEx;
begin
  result := '';
  reg := TPerlRegEx.Create;
  reg.RegEx := exp;
  reg.Subject := s;
  if not reg.Match then exit;
  reg.Subject := reg.Groups[0];
  reg.RegEx := exp2;
  if not reg.Match then exit;
  result := reg.Groups[0];
  reg.Free;
end;

function URLParamEncode(param: string): string;
begin
  result := TIdURI.ParamsEncode(param);
end;

function WideToUtf8(s: WideString): UTF8String;
begin
  result := TXMLNode.WideToUtf8(s);
end;

function Utf8ToWide(s: UTF8String): WideString;
begin
  result := TXMLNode.Utf8ToWide(s);
end;

function GetHTMLFromWebBrowser(wb: TWebBrowser): string;
begin
  result := (wb.Document as IHTMLDocument2).body.outerHTML;
end;

procedure Init;
var
  ini: TINIFile;
begin
  ExePath := ExtractFilePath(Application.ExeName);
  ini := TINIFile.Create(Ini_Name);
  Host_URL := ini.ReadString('URL', 'Host_URL', 'https://hfzkdebug.appspot.com');
  User_Query_URL := ini.ReadString('URL', 'User_Query_URL', Host_URL+'/user/queryuser');
  User_Login_URL := ini.ReadString('URL', 'User_Login_URL', Host_URL+'/user/login');
  User_Operate_URL := ini.ReadString('URL', 'User_Operate_URL', Host_URL+'/user/operate');
  Account_Query_URL := ini.ReadString('URL', 'Account_Query_URL', Host_URL+'/user/queryaccount');
  Account_Modify_URL := ini.ReadString('URL', 'Account_Modify_URL', Host_URL+'/user/modifyaccount');
  Intf_Sina_URL := ini.ReadString('URL', 'Intf_Sina_URL', Host_URL+'/intf/sina');
  Intf_Twitter_URL := ini.ReadString('URL', 'Intf_Twitter_URL', Host_URL+'/intf/twitter');
  Intf_Facebook_URL := ini.ReadString('URL', 'Intf_Facebook_URL', Host_URL+'/intf/facebook');
  Intf_Call_URL := ini.ReadString('URL','Intf_Call_URL', Host_URL+'/intf/call');
  Intf_Sep_URL := ini.ReadString('URL','Intf_Sep_URL', Host_URL+'/intf/sep');
  Intf_Smart_URL := ini.ReadString('URL', 'Intf_Smart_URL', Host_URL+'/intf/smart');
  Emotion_Sina_Path := ini.ReadString('Path','Emotion_Sina_Path', '..\Res\Images\Emotions\Sina');
  Image_Path := ini.ReadString('Path','Image_Path', '..\Res\Images');
  Cache_Path := ini.ReadString('Path', 'Cache_Path', '..\Cache');
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
  ini.WriteString('URL', 'Account_Query_URL', Account_Query_URL);
  ini.WriteString('URL', 'Account_Modify_URL', Account_Modify_URL);
  ini.WriteString('URL', 'Intf_Sina_URL', Intf_Sina_URL);
  ini.WriteString('URL', 'Intf_Twitter_URL', Intf_Twitter_URL);
  ini.WriteString('URL', 'Intf_Facebook_URL', Intf_Facebook_URL);
  ini.WriteString('URL','Intf_Call_URL', Intf_Call_URL);
  ini.WriteString('URL','Intf_Sep_URL', Intf_Sep_URL);
  ini.WriteString('URL','Intf_Smart_URL',Intf_Smart_URL);
  ini.WriteString('Path','Emotion_Sina_Path', Emotion_Sina_Path);
  ini.WriteString('Path', 'Cache_Path', Cache_Path);
  ini.WriteString('Path','Image_Path', Image_Path);
  ini.Free;
end;

initialization
  Init;

finalization
   uninit;

end.
