unit EditEx;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, LinkLabel, RxRichEd, StdCtrls, ExtCtrls, RVStyle,
  RVScroll, RichView, User_Intf, web_connect, Global, XMLObj;

type
  TRichEditEx = class;
  TreButtonTypeElement = (btReply, btComment, btRead, btRetweet,
    btGood, btFlavor, btDelete); 
  TreButtonType = set of TreButtonTypeElement;
  TButtonClickEvent = procedure(Sender: TObject;
    const ButtonType: TreButtonTypeElement) of object;

  TRichEditEx = class(TFrame)
    RVStyle: TRVStyle;
    reText: TRichView;
    pnlControl: TPanel;
    pnlRetweet: TPanel;
    procedure reTextMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    FAccountName: WideString;
    FAccount_Type: TAccount_Type;
    FText: String;
    FPicture: TGraphic;
    FAutoDraw: boolean;
    FButtonType: TreButtonType;
    FImageURL: string;
    FRichEdit: TRichEditEx;
    FBkColor: TColor;
    FRetweetColor: TColor;
    FRetweet: TRichEditEx;
    FOnButtonClick: TButtonClickEvent;
    FRetweetCount: Integer;
    FReadCount: Integer;
    FGoodCount: Integer;
    FCommentCount: Integer;
    FImgProfileURL: string;

    procedure CalcSize;
    procedure DrawButton;
    procedure Init(AOwner: TComponent);
    procedure SetFAccountName(const Value: WideString);
    procedure SetText(const Value: String);
    procedure DoDraw;
    function GetText: String;
    procedure SetPicture(const Value: TGraphic);
    procedure SetButtonType(const Value: TreButtonType);
    procedure ButtonClick(Sender: TObject);
    procedure SetImageURL(const Value: string);
    procedure SetBkColor(const Value: TColor);
    procedure SetRetweetColor(const Value: TColor);
    function GetHasRetweet: boolean;
    function GetIsRetweet: boolean;
    procedure SetCommentCount(const Value: Integer);
    procedure SetGoodCount(const Value: Integer);
    procedure SetReadCount(const Value: Integer);
    procedure SetRetweetCount(const Value: Integer);
    function GetButton(ty: TreButtonTypeElement): TLinkLabel;
    procedure SetImgProfileURL(const Value: string);
  published
    property ImgProfileURL: string read FImgProfileURL write SetImgProfileURL;
    property CommentCount: Integer read FCommentCount write SetCommentCount;
    property ReadCount: Integer read FReadCount write SetReadCount;
    property RetweetCount: Integer read FRetweetCount write SetRetweetCount;
    property GoodCount: Integer read FGoodCount write SetGoodCount;
    property OnButtonClick: TButtonClickEvent read FOnButtonClick write FOnButtonClick;
    property BkColor: TColor read FBkColor write SetBkColor;
    property RetweetColor: TColor read FRetweetColor write SetRetweetColor;
    property HasRetweet: boolean read GetHasRetweet;
    property IsRetweet: boolean read GetIsRetweet;
    property ImageURL: string read FImageURL write SetImageURL;
    property ButtonType: TreButtonType read FButtonType write SetButtonType
      default [btComment, btRetweet, btFlavor, btGood];
    property AutoDraw: boolean read FAutoDraw write FAutoDraw default false;
    property Account_Type: TAccount_Type read FAccount_Type write FAccount_Type;
    property AccountName: WideString read FAccountName write SetFAccountName;
    property Text: String read GetText write SetText;
    property Picture: TGraphic read FPicture write SetPicture;
  public
    constructor Create(AOwner: TComponent;
       AAccount_Type: TAccount_Type); overload;
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    function AddRetweet: TRichEditEx;
    procedure Reset;
    procedure Done;
  end;

implementation

{$R *.dfm}

const
  ButtonTitle: array [btReply..btDelete] of string =
    ('»Ø¸´', 'ÆÀÂÛ', 'ÔÄ¶Á', '×ª·¢', 'ÔÞ','ÊÕ²Ø', 'É¾³ý');

{ TRichEditEx }

constructor TRichEditEx.Create(AOwner: TComponent;
  AAccount_Type: TAccount_Type);
begin
  inherited Create(AOwner);
  Account_Type := AAccount_Type;
  Init(AOwner);
end;

function TRichEditEx.AddRetweet: TRichEditEx;
begin
  result := nil;
  if HasRetweet or IsRetweet then
    exit;
  result := TRichEditEx.Create(self, FAccount_Type);
  FRetweet := result;
end;

procedure TRichEditEx.CalcSize;
begin
  if IsRetweet then
  begin
    FRichEdit.pnlRetweet.Height := FRichEdit.pnlRetweet.Height +
      reText.DocumentHeight - reText.Height + 9;
    FRichEdit.CalcSize;
  end
  else
    Height := Height+reText.DocumentHeight-reText.Height+5;
end;

procedure TRichEditEx.ButtonClick(Sender: TObject);
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(self,
                   TreButtonTypeElement((Sender as TLinkLabel).Tag));
end;

constructor TRichEditEx.Create(AOwner: TComponent);
begin
  inherited;
  Init(AOwner);
  DoubleBuffered := True;
end;

destructor TRichEditEx.Destroy;
var
  comp: TComponent;
begin
  if Parent is TCustomRichView then
    for comp in pnlRetweet do
      comp.Free;
  inherited;
end;

procedure TRichEditEx.DoDraw;
var
  node: TNode;
  url: string;
  i, index: Integer;
begin
  if FImgProfileURL <> '' then
    reText.AddPictureEx('profile', GetPictureFromURL(FImgProfileURL),
      0, rvvaBaseLine);
  url := Format('%s?sns=%s&text=%s',[Intf_Sep_URL,
    TypeStrings[FAccount_Type], URLParamEncode(FText)]);
  node := web_connect.GetDataFromURL(url)['result'];
  reText.Clear;
  index := 0;
  reText.AddText(FAccountName+#13#10, 3);
  for i := 1 to Node.Count do
  begin
    if node[i]['texttype'].Value = 'normal' then
      index := 0
    else if node[i]['texttype'].Value = 'link' then
      index := 1
    else if node[i]['texttype'].Value = 'emotion' then
      index := 2;
    reText.AddText(node[i]['text'].Value, index);
  end;
  node.Free;
  if Assigned(FPicture) then
    reText.AddPictureEx('pic', FPicture, 0, rvvaBaseLine);
  if HasRetweet then
    reText.AddControlEx('pnlRetweet', pnlRetweet, 0, rvvaBaseLine);
  reText.AddControlEx('pnlControl', pnlControl, 0, rvvaBaseLine);
  reText.Reformat;
  CalcSize;
end;

procedure TRichEditEx.Done;
begin
  DoDraw;
end;

procedure TRichEditEx.DrawButton;

  procedure AddButton(button: TreButtonTypeElement);
  begin
    With TLinkLabel.Create(self.pnlControl) do
    begin
      Parent := self.pnlControl;
      Align := alRight;
      AlignWithMargins := True;
      Margins.Top := 8;
      Margins.Bottom := 8;
      Margins.Right := 3;
      Margins.Left := 3;
      Caption := ButtonTitle[button];
      Tag := Ord(button);
      Font.Name := 'Segoe UI';
      OnClick := ButtonClick;
    end;
  end;

  procedure Clear;
  var
    i: Integer;
  begin
    for i := 0 to pnlControl.ComponentCount - 1 do
      if pnlControl.Components[i] is TLinkLabel then
        pnlControl.Components[i].Free;
  end;

var
  btn: TreButtonTypeElement;

begin
  Clear;
  for btn := btReply to btDelete do
    if btn in FButtonType then
      AddButton(btn);
end;

function TRichEditEx.GetButton(ty: TreButtonTypeElement): TLinkLabel;
var
  comp: TComponent;
  btn: TLinkLabel;
begin
  result := nil;
  for comp in pnlControl do
  begin
    if not (comp is TLinkLabel) then
      continue;
    btn := comp as TLinkLabel;
    if btn.Tag = Ord(ty) then
    begin
      result := btn;
      break;
    end;
  end;
end;

function TRichEditEx.GetHasRetweet: boolean;
begin
  result := Assigned(FRetweet);
end;

function TRichEditEx.GetIsRetweet: boolean;
begin
  result := Assigned(FRichEdit);
end;

function TRichEditEx.GetText: String;
begin
  result := Utf8ToWide(FText);
end;

procedure TRichEditEx.Init(AOwner: TComponent);
begin
  FRichEdit := nil;
  FRetweet := nil;
  ButtonType := [btComment, btRetweet, btFlavor, btGood];
  if AOwner is TRichEditEx then
  begin
    Parent := (AOwner as TRichEditEx).pnlRetweet;
    FRichEdit := AOwner as TRichEditEx;
    FRichEdit.pnlRetweet.Visible := True;
    self.Align := alClient;
    BkColor := FRichEdit.RetweetColor;
  end
  else
  begin
    Parent := AOwner as TWinControl;
  end;
end;

procedure TRichEditEx.Reset;
begin
  Text := '';
end;

procedure TRichEditEx.reTextMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  pa: TWinControl;
begin
{  if IsRetweet then
    pa := FRichEdit.Parent
  else
    pa := Parent; }
 { if WheelDelta<0 then
    pa.Perform(WM_VSCROLL, SB_LINEDOWN, 2)
  else
    pa.Perform(WM_VSCROLL, SB_LINEUP, 2);}
end;

procedure TRichEditEx.SetBkColor(const Value: TColor);
begin
  FBkColor := Value;
  self.Color := Value;
  reText.Color := Value;
end;

procedure TRichEditEx.SetButtonType(const Value: TreButtonType);
begin
  FButtonType := Value;
  DrawButton;
end;

procedure TRichEditEx.SetCommentCount(const Value: Integer);
var
  btn: TLinkLabel;
begin
  FCommentCount := Value;
  btn := GetButton(btComment);
  if Assigned(btn) then
    btn.Count := Value;
end;

procedure TRichEditEx.SetFAccountName(const Value: WideString);
begin
  FAccountName := Value;
  if FAutoDraw then
    DoDraw;
end;

procedure TRichEditEx.SetGoodCount(const Value: Integer);
var
  btn: TLinkLabel;
begin
  FGoodCount := Value;
  btn := GetButton(btGood);
  if Assigned(btn) then
    btn.Count := Value;
end;

procedure TRichEditEx.SetImageURL(const Value: string);
begin
  FImageURL := Value;
  if Value = '' then
  begin
    if Assigned(FPicture) then
      FreeAndNil(FPicture);
    exit;
  end;
  Picture := GetPictureFromURL(Value);
end;

procedure TRichEditEx.SetImgProfileURL(const Value: string);
begin
  FImgProfileURL := Value;
 { if Value <> '' then
    ImgProfile.Picture.Graphic.Assign(GetPictureFromURL(Value));}
end;

procedure TRichEditEx.SetPicture(const Value: TGraphic);
begin
  FPicture := Value;
  if FAutoDraw then
    DoDraw;
end;

procedure TRichEditEx.SetReadCount(const Value: Integer);
var
  btn: TLinkLabel;
begin
  FReadCount := Value;
  btn := GetButton(btRead);
  if Assigned(btn) then
    btn.Count := Value;
end;

procedure TRichEditEx.SetRetweetColor(const Value: TColor);
begin
  FRetweetColor := Value;
  if HasRetweet then
    FRetweet.BkColor := Value;
end;

procedure TRichEditEx.SetRetweetCount(const Value: Integer);
var
  btn: TLinkLabel;
begin
  FRetweetCount := Value;
  btn := GetButton(btRetweet);
  if Assigned(btn) then
    btn.Count := Value;
end;

procedure TRichEditEx.SetText(const Value: String);
begin
  FText := WideToUtf8(Value);
  if FAutoDraw then
    DoDraw;
end;

end.
