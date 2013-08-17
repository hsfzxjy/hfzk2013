unit AccountsView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RVStyle, RVTable, RVData, RVScroll, RichView,CRVFData,
  User_Intf, Global;

type
  TAccountsViewer = class(TFrame)
    re: TRichView;
    rvstManagement: TRVStyle;
  private
    Sina, Twitter, Facebook: TRVTableItemInfo;
    User: TUser;

    procedure DrawTable(table: TRVTableItemInfo;_type: TAccount_Type);
  public
    procedure Init;
    procedure SetUser(Auser: TUser);
    procedure Update;
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses web_connect;

{ TAccountsViewsr }

procedure TAccountsViewer.DrawTable(table: TRVTableItemInfo;
  _type: TAccount_Type);
var
  i: integer;
begin
  for i := 0 to User.AccountsCount(_type)-1 do
  begin
    Table.InsertRows(Table.RowCount, 1, -1, False);
    table.Cells[i+1,0].Clear;
    table.Cells[i+1,0].AddHotPicture('delete',
    GetPictureFromPath(Image_Path+'\delete.png'), -1, rvvaBaseLine);
    table.Cells[i+1,0].AddHotPicture('pic',
    GetPictureFromURL(User.Accounts[_type,i].Info.profile_image.Small),
      0, rvvaMiddle);
    table.Cells[i+1,0].AddNL('  '+User.Accounts[_type, i].Info.screen_name,0,-1);
  end;
end;

procedure TAccountsViewer.Init;
begin
  if Assigned(Sina) then
    Sina.Free;
  if Assigned(Twitter) then
    Twitter.Free;
  if Assigned(Facebook) then
    Facebook.Free;
  Twitter := TRVTableItemInfo.CreateEx(1,1,re.RVData);
  Sina := TRVTableItemInfo.CreateEx(1,1,re.RVData);
  Facebook := TRVTableItemInfo.CreateEx(1,1,re.RVData);
  Sina.Cells[0,0].AddNL('����΢��',1,1);
  Twitter.Cells[0,0].AddNL('Twitter',1,1);
  Facebook.Cells[0,0].AddNL('Facebook', 1,1);
  Sina.Cells[0,0].AddBreak;
  twitter.Cells[0,0].AddBreak;
  facebook.Cells[0,0].AddBreak;
  re.AddItem('sina', Sina);
  re.AddItem('twitter',Twitter);
  re.AddItem('facebook',Facebook);

  re.Reformat;
end;

procedure TAccountsViewer.SetUser(Auser: TUser);
var
  i: Integer;
begin
  self.User := AUser;
  Update;
end;

procedure TAccountsViewer.Update;
begin
  if not Assigned(User) then exit;
  Init;
  DrawTable(Sina, atSina);
  DrawTable(Twitter, atTwitter);
  DrawTable(Facebook, atFacebook);
  re.Reformat;
end;

end.
