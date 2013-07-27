unit FrmLogin;

interface

uses
  Windows, Messages, SysUtils, Variants, Controls, Forms,
  Dialogs, Global, XMLObj, OleCtrls, SHDocVw, ComCtrls, Classes;

type
  TLoginForm = class(TForm)
    WBLogin: TWebBrowser;
    procedure WBLoginDownloadComplete(Sender: TObject);
  private
    { Private declarations }
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
  WBLogin.Navigate(URL);
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

  if msg <> '' then
  begin
    self.ModalResult := mrOK;
    self.Msg := msg;
  end;
end;

end.
