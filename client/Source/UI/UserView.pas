unit UserView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CustomViewFrame, SkinCtrls, StdCtrls, ExtCtrls, XMLObj, jpeg,
  DynamicSkinForm, dxGDIPlusClasses, SkinExCtrls, SkinBoxCtrls, CustomView,
  SNSView;

type
  TUserViewer = class(TCustomViewerFrame)
    pnlHeader: TspSkinPanel;
    spSkinPanel2: TspSkinPanel;
    imgProfile: TspSkinLinkImage;
    lblAccountName: TspSkinStdLabel;
    spSkinPanel1: TspSkinPanel;
    lblFriendsCount: TspSkinShadowLabel;
    lblFollowersCount: TspSkinShadowLabel;
    imgVerified: TImage;
    spSkinPanel3: TspSkinPanel;
    spSkinShadowLabel2: TspSkinShadowLabel;
    mmoDescription: TspSkinMemo;
    lblGender: TspSkinShadowLabel;
    lblLocation: TspSkinShadowLabel;
    TimelineViewer: TSNSViewer;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent; Node: TNode); override;
    { Public declarations }
  end;

var
  UserViewer: TUserViewer;

implementation

uses web_connect, MainHelper;

{$R *.dfm}

constructor TUserViewer.Create(AOwner: TComponent; Node: TNode);
var
  Gra: TGraphic;
  gender: string;
begin
  inherited;
  Gra := GetPictureFromURL(Node['profile_image']['large_url'].Value);
  if not Node['verified'].Value then
    imgVerified.Visible := False;
  lblAccountName.Caption := Node['screen_name'].Value;
  self.lblFriendsCount.Caption := self.lblFriendsCount.Caption + IntToStr(Node['follow_count'].Value);
  self.lblFollowersCount.Caption := self.lblFollowersCount.Caption + IntToStr(Node['follower_count'].Value);
  self.mmoDescription.Text := Node['description'].Value;
  imgProfile.Picture.Bitmap.Assign(Gra);
  Gra.Free;
  gender := Node['gender'].Value;
  case gender[1] of
    'm': lblGender.Caption := '��';
    'f': lblGender.Caption := 'Ů';
    'n': lblGender.Caption := '�Ա���';
  end;
  lblLocation.Caption := Node['location'].Value;
  Application.ProcessMessages;
  MainHelper.UpdateTimeline(TimelineViewer, Node['user_name'].Value);
end;

initialization
  RegisterClass(TUserViewer);

finalization
  UnRegisterClass(TUserViewer);

end.
