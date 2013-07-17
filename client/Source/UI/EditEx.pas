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

  TRichEditEx = class(TFrame)
    pnlText: TPanel;
    pnlControl: TPanel;
    pnlRetweet: TPanel;
    reText: TRichView;
    RVStyle: TRVStyle;
  private
    FAccountName: WideString;
    FAccount_Type: TAccount_Type;
    FText: String;
    FPicture: TGraphic;
    FAutoDraw: boolean;
    FButtonType: TreButtonType;
    FImageURL: string;

    procedure DrawButton;
    procedure Init;
    procedure SetFAccountName(const Value: WideString);
    procedure SetText(const Value: String);
    procedure DoDraw;
    function GetText: String;
    procedure SetPicture(const Value: TGraphic);
    procedure SetButtonType(const Value: TreButtonType);
    procedure ButtonClick(Sender: TObject);
    procedure SetImageURL(const Value: string);
  published
    property ImageURL: string read FImageURL write SetImageURL;
    property ButtonType: TreButtonType read FButtonType write SetButtonType
      default [btComment, btRetweet, btFlavor, btGood];
    property AutoDraw: boolean read FAutoDraw write FAutoDraw default false;
    property Account_Type: TAccount_Type read FAccount_Type write FAccount_Type;
    property AccountName: WideString read FAccountName write SetFAccountName;
    property Text: String read GetText write SetText;
    property Picture: TGraphic read FPicture write SetPicture;
  public
    constructor Create(AOwner: TComponent;AAccount_Type: TAccount_Type); overload;
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    procedure Reset;
    procedure Done;
  end;

implementation

{$R *.dfm}

const
  ButtonTitle: array [btReply..btDelete] of string =
    ('»Ø¸´', 'ÆÀÂÛ', 'ÔÄ¶Á', '×ª·¢', 'ÔÞ','ÊÕ²Ø', 'É¾³ý');


{ TRichEditEx }

constructor TRichEditEx.Create(AOwner: TComponent;AAccount_Type: TAccount_Type);
begin
  inherited Create(AOwner);
  Account_Type := AAccount_Type;
  Init;
end;

procedure TRichEditEx.ButtonClick(Sender: TObject);
begin

end;

constructor TRichEditEx.Create(AOwner: TComponent);
begin
  inherited;
  Init;
end;

destructor TRichEditEx.Destroy;
begin
  FreeAndNil(FPicture);
  inherited;
end;

procedure TRichEditEx.DoDraw;
var
  node: TNode;
  url: string;
  i, index: Integer;
begin
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
  reText.Reformat;
  Height := Height+reText.DocumentHeight-reText.Height+5;
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
  for btn in FButtonType do
    AddButton(btn);
end;

function TRichEditEx.GetText: String;
begin
  result := Utf8ToWide(FText);
end;

procedure TRichEditEx.Init;
begin
  FPicture := TGraphic.Create;
  ButtonType := [btComment, btRetweet, btFlavor, btGood];
end;

procedure TRichEditEx.Reset;
begin
  Text := '';
end;

procedure TRichEditEx.SetButtonType(const Value: TreButtonType);
begin
  FButtonType := Value;
  DrawButton;
end;

procedure TRichEditEx.SetFAccountName(const Value: WideString);
begin
  FAccountName := Value;
  if FAutoDraw then
    DoDraw;
end;

procedure TRichEditEx.SetImageURL(const Value: string);
begin
  FImageURL := Value;
  Picture := GetPictureFromURL(Value);
end;

procedure TRichEditEx.SetPicture(const Value: TGraphic);
begin
  FPicture := Value;
  if FAutoDraw then
    DoDraw;
end;

procedure TRichEditEx.SetText(const Value: String);
begin
  FText := WideToUtf8(Value);
  if FAutoDraw then
    DoDraw;
end;

end.
