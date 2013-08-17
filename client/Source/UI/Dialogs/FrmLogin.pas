unit FrmLogin;

interface

uses
  Windows, Messages, SysUtils, Variants, Controls, Forms,
  Dialogs, Global, XMLObj, OleCtrls, SHDocVw, ComCtrls, Classes, SHDocVw_EWB,
  EwbCore, EmbeddedWB, IECache, ExtCtrls;

type
  TLoginForm = class(TForm)
    WBLogin: TEmbeddedWB;
    IECache: TIECache;
    StatusPanel: TPanel;
    procedure WBLoginDownloadComplete(Sender: TObject);
  private
    FURL: string;

    procedure DeleteCookie(KeyWord: string);
  public
    Msg: string;
    
    constructor Create(AOwner: TComponent;URL: string); overload;
  end;

var
  LoginForm: TLoginForm;

implementation

{$R *.dfm}

uses web_connect;

{ TLoginForm }

constructor TLoginForm.Create(AOwner: TComponent; URL: string);
begin
  inherited Create(AOwner);
  StatusPanel.Visible := False;
  if Pos('facebook', URL) > 0 then
    DeleteCookie('facebook');
  FURL := URL;
  WBLogin.Navigate(URL);
end;

procedure TLoginForm.DeleteCookie(KeyWord: string);
var
  str: string;
begin
  IECache.FindFirstEntry(0);
  repeat
    str := IECache.EntryInfo.SourceUrlName;
    if pos(KeyWord, str) > 0 then
      IECache.DeleteEntry(str);
  until IECache.FindNextEntry > 0;
end;

procedure TLoginForm.WBLoginDownloadComplete(Sender: TObject);
const
  TwitterURL: WideString = 'https://api.twitter.com/oauth/authorize';
var
  pincode, url, FullURL, msg: string;
begin
  
  FullURL := GetFullURLPath(WBLogin.LocationURL);
  msg := '';

  if FullURL = TwitterURL then
  begin
    pincode := GetPinCode(GetHTMLFromWebBrowser(WBLogin));
    if pincode = '' then exit;
    url := Format('%s/callback?pincode=%s', [Intf_Twitter_URL, pincode]);
    msg := Get(url);
  end;

  if FullURL = User_Login_URL then
  begin
    msg := GetTextFromWebBrowser(WBLogin);
  end;

  if Pos('_error',Msg)>0 then
  begin
    StatusPanel.Visible := True;
    WBLogin.Navigate(FURL);
    exit;
  end;

  if msg <> '' then
  begin
    self.ModalResult := mrOK;
    self.Msg := msg;
  end;
end;

end.
