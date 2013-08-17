unit Global;

interface

uses
  Classes, IniFiles, Sysutils, MSHtml, OleCtrls, SHDocVw, NativeXML, IdURI,
  Forms, Graphics, frmWaiting, EmbeddedWB;

type
  TAccount_Type = (atSina, atTwitter, atFacebook);

  TImageURLs = record
    Large, Small: string;
  end;

  TCustomContext = class(TPersistent)
  public
    procedure LoadFromStream(s: TStream); dynamic; abstract;
    procedure SaveToStream(s: TStream); dynamic; abstract;
  end;

  TCustomContextClass = class of TCustomContext;

  EConnectError = class(Exception)
  private
    FURL: string;
  published
  public
    property URL: string read FURL;

    constructor Create(AURL: string);
  end;

  EAccountExpiredError = class(Exception)
  public

  end;

const
  Ini_Name :string = '.\Main.INI';
  TypeStrings : array [atSina..atFacebook] of String = ('sina','twitter','facebook');
  Err_Connect: string = '网络连接失败，无法连接到网址：'#13#10'%s'#13#10'请检查网络连接！';
  Err_Expired: string = '当前账户的授权已过期！请切换用户或者重新登录！';
  Broken_Img_Path: string = '..\res\images\broken_img.jpg';

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
function GetHTMLFromWebBrowser(wb: TEmbeddedWB): string;
function GetTextFromWebBrowser(wb: TEmbeddedWB): string;

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
function GraphicToBmp(Graphic: TGraphic;Free: boolean = True): TBitmap;

//Hash functions
function Hash(s: string): string;
function Unhash(s: string): string;

//Memory functions
procedure DeepDestroy(obj: TStringList);

//INI File functions
function GetINIUser(): string;
procedure SetINIUser(ID: string);

implementation

uses PerlRegEx, pngimage, gifimg, jpeg, EncdDecd;

var IniFile: TIniFile;

function GraphicToBmp(Graphic: TGraphic;Free: boolean = true): TBitmap;
begin
  result := TBitmap.Create;
  result.Assign(Graphic);
  if Free then
    Graphic.Free;
end;

procedure SetINIUser(ID: string);
begin
  INIFile.WriteString('Data', 'User', ID);
end;

function GetINIUser(): string;
begin
  result := INIFile.ReadString('Data','User','');
end;

procedure DeepDestroy(obj: TStringList);
var
  i: Integer;
begin
  for i:=0 to obj.Count -1 do
  if Assigned(obj.Objects[i]) then
  begin
    obj.Objects[i].Free;
    obj := nil;
  end;
//  obj.Clear;
  FreeAndNil(obj);
end;

function Hash(s:string):string;
const
  InvalidChars = ['\','/',':','*','?','"','<','>','|','%'];
var
  ch: Char;
begin
  result := '';
  for ch in s do
    if ch in InvalidChars then
      result := result + '%' + IntToHex(Ord(ch), 2)
    else
      result := result + ch;
end;

function UnHash(s: string):string;
var
  ch: Char;
  i: Integer;
begin
  result := '';
  i := 1;
  while i <=Length(s) do
  begin
    ch := s[i];
    if ch = '%' then
    begin
      result := result + chr(StrToInt('$'+Copy(s,i+1,2)));
      Inc(i, 3);
    end
    else
    begin
      result := result + s[i];
      Inc(i);
    end;
  end;
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

function GetTextFromWebBrowser(wb: TEmbeddedWB): string;
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
  try
    reg.RegEx := exp;
    reg.Subject := s;
    if not reg.Match then exit;
    reg.Subject := reg.Groups[0];
    reg.RegEx := exp2;
    if not reg.Match then exit;
    result := reg.Groups[0];
  finally
    reg.Free;
  end;
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

function GetHTMLFromWebBrowser(wb: TEmbeddedWB): string;
begin
  result := (wb.Document as IHTMLDocument2).body.outerHTML;
end;

procedure Init;
begin
  ExePath := ExtractFilePath(Application.ExeName);
  INIFile := TINIFile.Create(Ini_Name);
  Host_URL := INIFile.ReadString('URL', 'Host_URL', 'https://hfzkdebug.appspot.com');
  User_Query_URL := INIFile.ReadString('URL', 'User_Query_URL', Host_URL+'/user/query');
  User_Login_URL := INIFile.ReadString('URL', 'User_Login_URL', Host_URL+'/user/login');
  User_Operate_URL := INIFile.ReadString('URL', 'User_Operate_URL', Host_URL+'/user/operate');
  Account_Query_URL := INIFile.ReadString('URL', 'Account_Query_URL', Host_URL+'/account/query');
  Account_Modify_URL := INIFile.ReadString('URL', 'Account_Modify_URL', Host_URL+'/user/modifyaccount');
  Intf_Sina_URL := INIFile.ReadString('URL', 'Intf_Sina_URL', Host_URL+'/intf/sina');
  Intf_Twitter_URL := INIFile.ReadString('URL', 'Intf_Twitter_URL', Host_URL+'/intf/twitter');
  Intf_Facebook_URL := INIFile.ReadString('URL', 'Intf_Facebook_URL', Host_URL+'/intf/facebook');
  Intf_Call_URL := INIFile.ReadString('URL','Intf_Call_URL', Host_URL+'/intf/call');
  Intf_Sep_URL := INIFile.ReadString('URL','Intf_Sep_URL', Host_URL+'/intf/sep');
  Intf_Smart_URL := INIFile.ReadString('URL', 'Intf_Smart_URL', Host_URL+'/intf/smart');
  Emotion_Sina_Path := INIFile.ReadString('Path','Emotion_Sina_Path', '..\Res\Images\Emotions\Sina');
  Image_Path := INIFile.ReadString('Path','Image_Path', '..\Res\Images');
  Cache_Path := INIFile.ReadString('Path', 'Cache_Path', '..\Cache');
end;

procedure Uninit;
begin
  INIFile.WriteString('URL', 'Host_URL', Host_URL);
  INIFile.WriteString('URL', 'User_Query_URL', User_Query_URL);
  INIFile.WriteString('URL', 'User_Login_URL', User_Login_URL);
  INIFile.WriteString('URL', 'User_Operate_URL', User_Operate_URL);
  INIFile.WriteString('URL', 'Account_Query_URL', Account_Query_URL);
  INIFile.WriteString('URL', 'Account_Modify_URL', Account_Modify_URL);
  INIFile.WriteString('URL', 'Intf_Sina_URL', Intf_Sina_URL);
  INIFile.WriteString('URL', 'Intf_Twitter_URL', Intf_Twitter_URL);
  INIFile.WriteString('URL', 'Intf_Facebook_URL', Intf_Facebook_URL);
  INIFile.WriteString('URL','Intf_Call_URL', Intf_Call_URL);
  INIFile.WriteString('URL','Intf_Sep_URL', Intf_Sep_URL);
  INIFile.WriteString('URL','Intf_Smart_URL',Intf_Smart_URL);
  INIFile.WriteString('Path','Emotion_Sina_Path', Emotion_Sina_Path);
  INIFile.WriteString('Path', 'Cache_Path', Cache_Path);
  INIFile.WriteString('Path','Image_Path', Image_Path);
  INIFile.Free;
end;

{ EConnectError }

constructor EConnectError.Create(AURL: string);
begin
  FURL := AURL;
end;

initialization
  Init;

finalization
   uninit;

end.
