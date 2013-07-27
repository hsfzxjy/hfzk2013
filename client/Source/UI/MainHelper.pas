unit MainHelper;

interface

uses
  {System}
  Forms, Windows, Graphics, Sysutils, Controls,
  {Custom}
  User_Intf, Data_Intf, XMLObj, Global, web_connect, Main, SNSView,
  {Third-Part}
  RichView, RVTable, RVStyle, RVData, RVItem, RVTypes, NativeXML;

function Warn(text: string;button: Integer): Integer;
function Info(text: string;button: Integer): Integer;
function GetLoginMsg(_type: TAccount_Type;var Msg: string): Integer;

procedure UpdateRVManagement(re: TRichView);
function StatusToSNSContext(status: TNode): TSNSContext;
procedure UpdateTimeline(re: TSNSViewer);
implementation

uses FrmLogin;

const
  HeaderStrs: array [atSina..atFacebook] of string =
    ('新浪微博', 'twitter', 'Facebook');

procedure UpdateTimeline(re: TSNSViewer);
var
  node, res: TNode;
  sns: TSNSContext;
  i: Integer;
begin
  res := Main.CurrentAccount.Timeline['result'];

  for i := 1 to res.Count do
  begin
    node := res[i];
    sns := StatusToSNSContext(node);
    re.AddContext(sns);
    sns.Free;
  end;
end;

function StatusToSNSContext(status: TNode): TSNSContext;
var
  sns: TSNSContext;
begin
  sns := TSNSContext.Create;
 // sns.Time := status['time'].Value;
  sns.ImageURL := status['image_url'].Value;
  sns.ProfileImageURL := status['user']['profile_image_url'].Value;
  sns.Text := status['text'].Value;
  sns.Counts.Comment := status['comment_count'].Value;
  result := sns;
end;

function GetLoginMsg(_type: TAccount_Type;var Msg: string): Integer;
var
  s: string;
  frm: TLoginForm;
begin
  case _type of
    atSina:     s := Intf_Sina_URL;
    atTwitter:  s := Intf_Twitter_URL;
    atFacebook: s := Intf_Facebook_URL;
  end;
  s := s + '/oauth';

  frm := TLoginForm.Create(Application, s);
  try
    if frm.ShowModal = mrOK then
    begin
      result := mrOK;
      Msg := frm.Msg;
    end
    else
      result := mrCancel;
  finally
    frm.Free;
  end;
end;

procedure UpdateRVManagement(re: TRichView);

  procedure AddTable(_type: TAccount_Type);
  var
    table: TRVTableItemInfo;
    i: Integer;
  begin
    table := TRVTableItemInfo.CreateEx(User.AccountsCount(_type)+1, 1, re.RVData);
    table.BestWidth := -80;
    table.ParaNo := 1;
    table.Color := $ADF8F0;
    table.Cells[0,0].Clear;
    table.Cells[0,0].AddNL(HeaderStrs[_type],1,1);
    table.Cells[0,0].AddBreak;
    //table.Cells[0,0].Color := HEADCOLOR;
    for i := 0 to User.AccountsCount(_type)-1 do
    begin
      table.Cells[i+1,0].Clear;
      table.Cells[i+1,0].AddHotPicture('pic',
        GetPictureFromURL(User.Accounts[_type,i].ProfileImageURL),
        0, rvvaMiddle);
      table.Cells[i+1,0].AddNL('  '+User.Accounts[_type, i].ScreenName,0,-1);
      table.Cells[i+1,0].AddHotPicture('delete',
        GetPictureFromPath(Image_Path+'\delete.png'), -1, rvvaBaseLine);
    end;
    table.BorderVSpacing := 5;
    table.BorderHSpacing := 10;
    table.CellPadding := 4;
    table.BorderWidth := 2;
    table.CellBorderWidth := 0;
    table.BorderStyle := rvtbColor;
    table.BorderWidth := 0;
    table.CellBorderStyle := rvtbColor;
    re.AddItem(TypeStrings[_type], table);
  end;

var
  type_ :TAccount_Type;
begin
  if not Assigned(User) then exit;
  re.Clear;
  for type_ := atSina to atFacebook do
  begin
    AddTable(type_);
    re.AddNL('', 0, 0);
  end;
  re.Reformat;
end;

function Warn(text: string;button: Integer): Integer;
begin
  result := Application.MessageBox(PChar(text), '警告', MB_ICONWARNING or button);
end;

function Info(text: string;button: Integer): Integer;
begin
  result := Application.MessageBox(PChar(text), '提示', MB_ICONQUESTION or button);
end;

end.
