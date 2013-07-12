unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, NativeXML,Global, User_Intf, Data_Intf, StdCtrls, OleCtrls,
  SHDocVw, ExtCtrls;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Splitter1: TSplitter;
    wb: TWebBrowser;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    User: TUser;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses mshtml, web_connect;

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  User.DeleteAccount(atFacebook, '100006035650605');
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  wb.Navigate(Host_URL);
  User := TUser.Create(7);
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  User.Free;
end;

end.
