unit SNSVie;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, RVStyle, RVScroll, RichView, RVTable, Contnrs, Global,
  XMLObj, web_connect, User_Intf, RVItem, CRVData, RVTypes, CRVFData,
  Clipbrd;

type
  TreButtonTypeElement = (btReply, btComment, btRead, btRetweet,
    btGood, btFlavor, btDelete);
  TreButtonType = set of TreButtonTypeElement;

  TSNSCount = record
    Reply, Comment, Read, Retweet, Good: Integer;
  end;

  TSNSContext = class(TPersistent)
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

  TSNSViewe = class(TFrame)
    reText: TRichView;
    RVStyle: TRVStyle;
    procedure reTextJump(Sender: TObject; id: Integer);
    procedure reTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    Table: TrvTableItemInfo;
    FRetweetColor: TColor;
    Updating: boolean;
    ContextList: TObjectList;
    FAccount_Type: TAccount_Type;

    procedure AddPicture(cell: TRVTableCellData;ImgUrl: string;
      ParaNo: Integer;VAlign: TRVVAlign);
    function AdjustButtonText(button: TreButtonTypeElement;
      context: TSNSContext): string;
    procedure DoAddCell(index: Integer;context: TSNSContext);
    procedure InitRe;
    procedure InitTable;
    procedure SetRetweetColor(const Value: TColor);
  published
    property RetweetColor: TColor read FRetweetColor write SetRetweetColor;
    property Account_Type: TAccount_Type read FAccount_Type write FAccount_Type;
  public
    procedure DoDraw;
    procedure Update;
    function AddContext(context: TSNSContext): boolean;
    function DeleteContext(Index: Integer): boolean;
    function DeleteLast: boolean;
    procedure Copy;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

const
  ButtonTitle: array [btReply..btDelete] of string =
    ('»Ø¸´', 'ÆÀÂÛ', 'ÔÄ¶Á', '×ª·¢', 'ÔÞ','ÊÕ²Ø', 'É¾³ý');

implementation

uses GifImg;

{$R *.dfm}

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

{ TSNSViewer }

function TSNSViewe.AddContext(context: TSNSContext): boolean;
var
  con: TSNSContext;
begin
  result := False;
  if Updating then exit;
  con := TSNSContext.Create;
  con.Assign(context);
  ContextList.Add(con);
  if Assigned(Table) then
  begin
    DoAddCell(ContextList.Count-1, con);
    reText.Reformat;
  end
  else
    DoDraw;
end;

procedure TSNSViewe.AddPicture(cell: TRVTableCellData; ImgUrl: string;
  ParaNo: Integer;VAlign: TRVVAlign);
var
  Gra: TGraphic;
begin
  if ImgUrl = '' then exit;
  Gra := GetPictureFromURL(ImgUrl);
  cell.AddPictureEx('image', Gra, ParaNo, VAlign);
end;

function TSNSViewe.AdjustButtonText(button: TreButtonTypeElement;
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

procedure TSNSViewe.Copy;
begin
  
end;

constructor TSNSViewe.Create(AOwner: TComponent);
begin
  inherited;
  Updating := False;
  RetweetColor := $F2F2F2;
  ContextList := TObjectList.Create;
end;

function TSNSViewe.DeleteContext(Index: Integer): boolean;
begin
  result := False;
  if Updating then exit;
  if (Index < 0) or (Index >= ContextList.Count) then exit;
  ContextList.Delete(Index);
  Table.DeleteRows(Index, 1, True);
  reText.Reformat;
  result := True;
end;

function TSNSViewe.DeleteLast: boolean;
begin
  result := DeleteContext(ContextList.Count-1);
end;

destructor TSNSViewe.Destroy;
begin
  ContextList.Free;
  inherited;
end;

procedure TSNSViewe.DoAddCell(Index: Integer;Context: TSNSContext);

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

var
  cell: TRVTableCellData;
begin
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

procedure TSNSViewe.DoDraw;
var
  i: Integer;
  context: TSNSContext;
begin
  InitRe;
  InitTable;
  for I := 0 to ContextList.Count - 1 do
  begin
    context := ContextList.Items[i] as TSNSContext;
    DoAddCell(i, context);
  end;
  reText.AddItem('table', Table);
  reText.Reformat;
end;

procedure TSNSViewe.InitRe;
begin
  reText.Clear;
end;

procedure TSNSViewe.InitTable;
begin
  Table := TRVTableItemInfo.Create(reText.RVData);
  Table.CellPadding := 0;
  Table.CellBorderWidth := 0;
  Table.CellHPadding := 0;
  Table.CellVPadding := 0;
  Table.CellVSpacing := 0;
  Table.CellHSpacing := 0;
end;

procedure TSNSViewe.reTextJump(Sender: TObject; id: Integer);
var
  info: TCustomRVFormattedData;
  ItemNo: Integer;
begin
  reText.GetJumpPointLocation(id, info, ItemNo);
  ShowMessage(inttostr(ItemNo));
end;

procedure TSNSViewe.reTextKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Updating then exit;
  if (Shift = [ssCtrl]) and (Key = Ord('C')) then //Ctrl+C
  begin
    reText.CopyTextW;
  end;
end;

procedure TSNSViewe.SetRetweetColor(const Value: TColor);
begin
  FRetweetColor := Value;
  RVStyle.ParaStyles[1].Background.Color := Value;
  RVStyle.ParaStyles[2].Background.Color := Value;
end;

procedure TSNSViewe.Update;
begin
  if Updating then exit;
  Updating := True;
  try
    DoDraw;
  finally
    Updating := False;
  end;
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

end.
