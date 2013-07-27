unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, NativeXML, Global, User_Intf, Data_Intf, RVStyle,
  ComCtrls, CustomView, RVScroll, RichView, RxGIF, cxImage,  dxRibbonForm,
  dxBar, dxBarExtItems, dxRibbon, cxContainer, cxEdit, cxClasses,
  dxGDIPlusClasses, cxPC, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, SNSView;

type
  TMainForm = class(TdxRibbonForm)
    PageControl: TcxPageControl;
    tabLogin: TcxTabSheet;
    tabManagement: TcxTabSheet;
    SinaLogin: TcxImage;
    TwitterLogin: TcxImage;
    FacebookLogin: TcxImage;
    rvManagement: TRichView;
    rvstManagement: TRVStyle;
    tabTimeline: TcxTabSheet;
    TimelineViewer: TSNSViewer;
    Ribbon: TdxRibbon;
    BarManager: TdxBarManager;
    AppBar: TdxBar;
    dxBarButton1: TdxBarButton;
    dxBarLargeButton1: TdxBarLargeButton;
    dxBarListItem1: TdxBarListItem;
    dxBarFontNameCombo1: TdxBarFontNameCombo;

    procedure LoginClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ItemLoginClick(Sender: TObject);
    procedure ItemManagementClick(Sender: TObject);
    procedure btnLoginOpClick(Sender: TObject);
    procedure btnManageClick(Sender: TObject);
    procedure ItemTimelineClick(Sender: TObject);
    procedure dxBarLargeButton1Click(Sender: TObject);
    procedure RibbonTabChanged(Sender: TdxCustomRibbon);
  private
    Logined: boolean;

    procedure UpdateTimeline;
    procedure Logout;
    procedure Login;
    procedure SetUser(const Msg: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  User: TUser;
  CurrentAccount: TAccount;
  
implementation

uses web_connect, XMLObj, MainHelper, Cache_;

{$R *.dfm}

{ TMainForm }

procedure TMainForm.btnLoginOpClick(Sender: TObject);
begin
  if not Logined then
    self.PageControl.ActivePage := self.tabLogin
  else
    ;
end;

procedure TMainForm.btnManageClick(Sender: TObject);
begin
  PageControl.ActivePage := tabManagement;
end;

procedure TMainForm.dxBarLargeButton1Click(Sender: TObject);
begin
  PageControl.ActivePage := tabLogin;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Logout;
end;

procedure TMainForm.ItemLoginClick(Sender: TObject);
begin
  PageControl.ActivePage := tabLogin;
end;

procedure TMainForm.ItemManagementClick(Sender: TObject);
begin
  PageControl.ActivePage := tabManagement;
end;

procedure TMainForm.ItemTimelineClick(Sender: TObject);
begin
  UpdateTimeline;
  PageControl.ActivePage := tabTimeline;
end;

procedure TMainForm.Login;
begin

  Logined := True;
  CurrentAccount := User.FirstAccount;
  Cache.ID := User.ID;
  Cache.Account_Type := CurrentAccount.Account_Type;
  Cache.Account_Name := CurrentAccount.Account_Name;
  UpdateRVManagement(rvManagement);
  self.UpdateTimeline;
end;

procedure TMainForm.LoginClick(Sender: TObject);
var
  msg: string;
begin
  if GetLoginMsg(TAccount_Type((Sender as TControl).Tag), msg) = mrOK then
    SetUser(Msg);
end;

procedure TMainForm.Logout;
begin
  if Assigned(User) then
  begin
    User.Free;
    CurrentAccount.Free;
  end;
  Logined := False;
end;

procedure TMainForm.RibbonTabChanged(Sender: TdxCustomRibbon);
begin
  if Ribbon.ActiveTab.Index = 0 then
    PageControl.ActivePage:=tabManagement;
end;

procedure TMainForm.SetUser(const Msg: string);
const
  strError = '此账号已被其他用户关联，请 切换用户 或者 重新登录账号！';
  strSuccess1 = '登录成功！';
  strSuccess2 = '账户添加成功！';
var
  node: TNode;
  id: string;
begin
  node := parse(Msg);
  id := QueryAccount(string(node['account_type'].Value), node['account_name'].Value);
  if (not Logined) and (id = '') then
  begin
    User := CreateUser;
    User.AddAccount(Msg);
    Info(strSuccess1, MB_OK);
    Login;
  end
  else
  if Logined and (id = '') then
  begin
    User.AddAccount(Msg);
    Info(strSuccess2, MB_OK);
  end
  else
  if (not Logined) and (id <> '') then
  begin
    ModifyAccount(node);
    User := TUser.Create(id);
    Info(strSuccess1, MB_OK);
    Login;
  end
  else
  begin
    Info(strError, MB_OK);
  end;
  node.Free;    
end;

procedure TMainForm.UpdateTimeline;
var
  sns:TSNSContext;
begin
  MainHelper.UpdateTimeline(self.TimelineViewer);
end;

end.
