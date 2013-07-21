unit web_connect;

interface

uses
  Sysutils, NativeXML, XMLObj, Classes, WinINet, Windows,
  IdURI, JPEG, GifImg, Graphics;

type

EURLGetError = class (Exception);

function GetStringFromURL(url: string): WideString;
function GetDataFromURL(url: string): TNode;
procedure Get(url: string;res: TStream); overload;
procedure Post(url, data:string;res:TStream); overload;
function Get(url: string): string; overload;
function Post(url, data: string): string; overload;
function GetPictureFromURL(url: string): TGraphic;

implementation

function GetPictureFromURL(url: string): TGraphic;
var
  jpg: TJpegImage;
  gif: TGifImage;
  m: TMemoryStream;
  uri: TIDURI;
  ext: string;
begin
  uri := TIDURI.Create(url);
  ext := LowerCase(ExtractFileExt(uri.Document));
  uri.Free;
  m := TMemoryStream.Create;
  Get(url, m);
  m.Position := 0;
  if ext = '.gif' then
  begin
    gif := TGifImage.Create;
    gif.LoadFromStream(m);
    gif.Animate := true;
    result := gif;
  end
  else
  begin
    jpg := TJPEGImage.Create;
    jpg.LoadFromStream(m);
    result := jpg
  end;  
end;

function Get(url: string): string;
var
  s: TStringStream;
begin
  s := TStringStream.Create('');
  try
    Get(url, s);
    result := s.DataString;
  finally
    s.Free;
  end;
end;

function Post(url, data: string): string;
var
  s: TStringStream;
begin
  s := TStringStream.Create('');
  try
    Get(url, s);
    result := s.DataString;
  finally
    s.Free;
  end;
end;

procedure Post(url, data:string;res:TStream);
var
  hInt,hConn,hreq:HINTERNET;
  buffer:PChar;
  dwRead, dwFlags:cardinal;
  port: Word;
  uri: TIdURI;
  proto, host, path: string;
begin
  uri := TIdURI.Create(url);
  host := uri.Host;
  path := uri.Path + uri.Document;
  proto := uri.Protocol;
  uri.Free;
  if UpperCase(proto) = 'HTTPS' then
  begin
    port := INTERNET_DEFAULT_HTTPS_PORT;
    dwFlags := INTERNET_FLAG_SECURE;
  end
  else
  begin
    port := INTERNET_INVALID_PORT_NUMBER;
    dwFlags := INTERNET_FLAG_RELOAD;
  end;
  hInt := InternetOpen('Delphi',INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0);
  hConn := InternetConnect(hInt,PChar(host),port,nil,nil,INTERNET_SERVICE_HTTP,0,0);
  hreq := HttpOpenRequest(hConn,'POST',PChar(Path),'HTTP/1.1',nil,nil,dwFlags,0);
  GetMem(buffer, 65536);
  if HttpSendRequest(hReq,nil,0,PChar(data),Length(data)) then
  begin
    dwRead:=0;
    repeat
      InternetReadFile(hreq,buffer,65536,dwRead);
      if dwRead<>0 then
        res.Write(buffer^, dwRead);
    until dwRead=0;
  end;
 InternetCloseHandle(hreq);
 InternetCloseHandle(hConn);
 InternetCloseHandle(hInt);
 FreeMem(buffer);
end;

procedure Get(url: string;res: TStream);
var
  hInt,hUrl:HINTERNET;
  buffer:PChar;
  dwRead:cardinal;
begin
 GetMem(buffer, 65536);
 hInt := InternetOpen('Delphi',INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0);
 dwRead:=0;
 hurl:=InternetOpenUrl(hInt,PChar(url),nil,0,INTERNET_FLAG_RELOAD,0);
 repeat
   InternetReadFile(hUrl,buffer,1000,dwRead);
   if dwRead<>0 then
     res.Write(buffer^, dwRead);
 until dwRead=0;
 InternetCloseHandle(hUrl);
 InternetCloseHandle(hInt);
 FreeMem(buffer);
end;

function GetStringFromURL(url: string): WideString;
var
  xml: TNativeXML;
begin
  xml := TNativeXML.Create(nil);
  result := '';
  try
    xml.LoadFromURL(url);
    result := xml.WriteToLocalUnicodeString;
  except
    xml.Free;
    raise EURLGetError.Create('Data get failed!');
    exit;
  end;
  xml.Free;
end;

function GetDataFromURL(url: string): TNode;
var
  xml: TNativeXML;
  node: TNode;
begin
  xml := TNativeXML.Create(nil);
  node := TNode.Create(nil);
  try
    xml.LoadFromURL(url);
    node := parse(xml);
  except
    xml.Free;
    FreeAndNil(node);
    result := node;
    raise EURLGetError.Create('Data get failed!');
    exit;
  end;
  xml.Free;
  result := node;
end;

end.
