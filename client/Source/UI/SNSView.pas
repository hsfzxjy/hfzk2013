unit SNSView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CustomView, RVStyle, RVScroll, RichView, Global, XMLObj, web_connect,
  CRVFData, RVTable;

type
  TreButtonTypeElement = (btReply, btComment, btRead, btRetweet,
    btGood, btFlavor, btDelete);
  TreButtonType = set of TreButtonTypeElement;

  TSNSCount = record
    Reply, Comment, Read, Retweet, Good: Integer;
  end;

  TSNSContext = class;
  TSNSContextClass = class of TSNSContext;

  TSNSContext = class(TCustomContext)
  private
    FIsRetweet: Boolean;
    FHasRetweet: boolean;
    FText: string;
    FNode: TNode;

    procedure ConvertText;
    procedure SetHasRetweet(const Value: boolean);
    function GetText: string;
    procedure SetText(const Value: string);
    procedure SetNode(const Value: TNode);
  published
  public
    Time: TDateTime;
    AccountName,
    ImageURL,
    ProfileImageURL: string;
    Counts: TSNSCount;
    IsFlavor: Boolean;
    Buttons: TreButtonType;
    Retweet: TSNSContext;
    Account_Type: TAccount_Type;

    property Node: TNode read FNode write SetNode;
    property Text: string read GetText write SetText;
    property HasRetweet: boolean read FHasRetweet write SetHasRetweet;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  end;

  TSNSViewer = class(TCustomViewer)
  private
    FRetweetColor: TColor;
    FAccount_Type: TAccount_Type;

    procedure AddPicture(cell: TRVTableCellData;ImgUrl: string;
      ParaNo: Integer;VAlign: TRVVAlign);
    function AdjustButtonText(button: TreButtonTypeElement;
      context: TSNSContext): string;
    procedure SetRetweetColor(const Value: TColor);
  protected
    procedure DoAddCell(Index: Integer;AContext: TCustomContext); override;
  published
    property Account_Type: TAccount_Type read FAccount_Type write FAccount_Type;
    property RetweetColor: TColor read FRetweetColor write SetRetweetColor;
  public
    constructor Create(AOwner: TComponent); override;
  end;

const
  ButtonTitle: array [btReply..btDelete] of string =
    ('»Ø¸´', 'ÆÀÂÛ', 'ÔÄ¶Á', '×ª·¢', 'ÔÞ','ÊÕ²Ø', 'É¾³ý');

var
  SNSViewer: TSNSViewer;

implementation

{$R *.dfm}

uses GifImg;

function EncodeButtonTag(button: TreButtonTypeElement;Index: Integer): string;
begin
  result := Format('%d_%d', [Ord(button), Index]);
end;

procedure DecodeButtonTag(Tag: string;var button: TreButtonTypeElement;
var Index: Integer);
var
  i: integer;
begin
  i := Pos('_', Tag);
  button := TreButtonTypeElement(StrToInt(Copy(Tag, 1, i-1)));
  Index := StrToInt(Copy(Tag, i+1, Length(Tag)));
end;

{ TSNSContext }

procedure TSNSContext.Assign(Source: TPersistent);
var
  context: TSNSContext;
begin
  if Source is TSNSContext then
  begin
    context := Source as TSNSContext;
    self.AccountName := context.AccountName;
    self.Time := context.Time;
    self.Text := context.Text;
    self.ImageURL := context.ImageURL;
    self.ProfileImageURL := context.ProfileImageURL;
    self.Counts := context.Counts;
    self.IsFlavor := context.IsFlavor;
    self.Buttons := context.Buttons;
    self.HasRetweet := context.HasRetweet;
    self.Account_Type := context.Account_Type;
    self.Node.Assign(context.Node);
    if (not FIsRetweet) and self.HasRetweet then
      self.Retweet.Assign(context.Retweet);
    exit;
  end;
  inherited;
end;

procedure TSNSContext.ConvertText;
var
  url: string;
begin
  url := Format('%s?sns=%s&text=%s',[Intf_Sep_URL,
    TypeStrings[Account_Type], URLParamEncode(FText)]);
  Node := web_connect.GetDataFromURL(url)['result'];
end;

constructor TSNSContext.Create;
begin
  FHasRetweet := False;
  FIsRetweet := False;
end;

destructor TSNSContext.Destroy;
begin
  if Assigned(Retweet) then
    Retweet.Free;
  if Assigned(FNode) then
    FNode.Free;
  inherited;
end;

function TSNSContext.GetText: string;
begin
  result := UTF8ToWide(FText);
end;

procedure TSNSContext.SetHasRetweet(const Value: boolean);
begin
  FHasRetweet := Value;
  if Value then
  begin
    Retweet := TSNSContext.Create;
    Retweet.FIsRetweet := True;
  end;
end;

procedure TSNSContext.SetNode(const Value: TNode);
begin
  FNode := Value;
end;

procedure TSNSContext.SetText(const Value: string);
begin
  FText := WideToUTF8(Value);
  ConvertText;
end;

{ TSNSViewer }

procedure TSNSViewer.AddPicture(cell: TRVTableCellData; ImgUrl: string;
  ParaNo: Integer; VAlign: TRVVAlign);
var
  Gra: TGraphic;
begin
  if ImgUrl = '' then exit;
  Gra := GetPictureFromURL(ImgUrl);
  cell.AddPictureEx('image', Gra, ParaNo, VAlign);
end;

function TSNSViewer.AdjustButtonText(button: TreButtonTypeElement;
  context: TSNSContext): string;
const
  FmtStr = '%s';
  FmtStr2 = '%s(%d)';

  procedure MakeText(var text: string;count: Integer);
  begin
    if Count = 0 then
      text := Format(FmtStr, [text])
    else
      text := Format(FmtStr2, [text, Count]);
  end;

begin
  result := ButtonTitle[button];
  case button of
    btReply: MakeText(result, context.Counts.Reply);
    btComment: MakeText(result, context.Counts.Comment);
    btRead: MakeText(result,context.Counts.Read);
    btGood: MakeText(result,context.Counts.Good);
    btRetweet: MakeText(result, context.Counts.Retweet);
  end;
end;

constructor TSNSViewer.Create(AOwner: TComponent);
begin
  inherited;
  RetweetColor := $F2F2F2;
  FContextClass := TSNSContext;
end;

procedure TSNSViewer.DoAddCell(Index: Integer; AContext: TCustomContext);
var
  cell: TRVTableCellData;
  context: TSNSContext;

  procedure DrawButtons(buttons: TreButtonType;
    cell: TRVTableCellData;IsRetweet: boolean);
  var
    button: TreButtonTypeElement;
    i, ParaNo1, ParaNo: Integer;
    text: string;
    con: TSNSContext;
    Tag: string;
  begin
    if buttons = [] then
      buttons := [btComment, btRetweet, btFlavor, btGood];
    i := 0;
    for button in buttons do
    begin
      ParaNo1 := 3 - Ord(IsRetweet);
      if i = 0 then
        ParaNo := ParaNo1
      else
      begin
        ParaNo := -1;
        cell.AddTextNL(' | ', 5, ParaNo, ParaNo1);
      end;
      if IsRetweet then
        con := context.Retweet
      else
        con := context;
      text := ' '+ButtonTitle[button];
      Tag := EncodeButtonTag(button, Index);
      cell.AddTextNLW(AdjustButtonText(button, con),
                     3, ParaNo, ParaNo1, False);//Integer(@Tag));
      Inc(i);
    end;
  end;

  function DrawEmotion(cell: TRVTableCellData;EmotionName: string): boolean;
  var
    gif: TGifImage;
    FileName, _type: string;
  begin
    result := false;
    _type := Emotion_Sina_Path;
    FileName := Format('%s\%s.gif', [_type, EmotionName]);
    if not FileExists(FileName) then exit;
    gif := TGifImage.Create;
    gif.LoadFromFile(FileName);
    cell.AddPictureEx('emotion', gif, -1, rvvaBaseLine);
    result := true;
  end;

  procedure DrawText(cell: TRVTableCellData;
    IsRetweet: boolean;node: TNode);
  var
    ParaNo, ParaNo1, i, index: Integer;
  begin
    ParaNo1 := Ord(IsRetweet);
    index := 1;
    for i := 1 to Node.Count do
    begin
      if i = 1 then
        ParaNo := ParaNo1
      else
        ParaNo := -1;
      if node[i]['texttype'].Value = 'normal' then
        index := 1
      else if node[i]['texttype'].Value = 'link' then
        index := 2
      else if node[i]['texttype'].Value = 'emotion' then
      begin
        index := 4;
        if DrawEmotion(cell, node[i]['text'].Value) then continue;
      end;
      cell.AddTextNL(node[i]['text'].Value, index, ParaNo, ParaNo1);
    end;
  end;

begin
  context := AContext as TSNSContext;
  Table.InsertRows(0, 1, -1, False);
  cell := Table.Cells[0,0];
  AddPicture(cell, context.ProfileImageURL, 0, rvvaAbsMiddle);
  cell.AddTextNL(Context.AccountName, 0, -1, 0);
  DrawText(cell, false, context.Node);
  AddPicture(cell, context.ImageURL, 0, rvvaBaseLine);
  if Context.HasRetweet then
  begin
    cell.AddTextNL(Context.Retweet.AccountName, 0, 1, 1);
    DrawText(cell, True, context.Retweet.Node);
    AddPicture(cell, context.Retweet.ImageURL, 1, rvvaBaseLine);
    DrawButtons(Context.Retweet.Buttons, cell, True);
  end;
  DrawButtons(Context.Buttons, cell, False);
end;

procedure TSNSViewer.SetRetweetColor(const Value: TColor);
begin
  FRetweetColor := Value;
  RVStyle.ParaStyles[1].Background.Color := Value;
  RVStyle.ParaStyles[2].Background.Color := Value;
end;

end.
