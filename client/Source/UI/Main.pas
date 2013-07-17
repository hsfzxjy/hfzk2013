unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, NativeXML,Global, User_Intf, Data_Intf, StdCtrls, OleCtrls,
  SHDocVw, ExtCtrls, EditEx;

type
  TForm2 = class(TForm)
    ScrollBox1: TScrollBox;
    procedure FormCreate(Sender: TObject);
  private
    User: TUser;
    re: TRichEditEx;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses web_connect,Jpeg;

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
var
  api:TAPICall;
begin
  re := TRichEditEx.Create(nil);
  re.Parent := ScrollBox1;
  re.Height := 400;
  re.Align := alTop;
  re.Text := 'ÄãºÃhello'#13'sgdas#23#';
  re.AccountName := 'HSFZXJY';
  re.ImageURL := 'http://ww4.sinaimg.cn/thumbnail/afbd4572jw1e6i2jvea15j20dw08i75w.jpg';
  re.Done;
end;

end.
