unit Data_Intf;

interface

uses Classes, XMLObj, web_connect, Global, Sysutils, Variants, IdURI;

type

  TAPICall =class;

  TAPICall = class
  private
    Keys: TStringList;
    Values: array of Variant;
    AFreeOnCall: boolean;
    Asns: string;
    AAccess_Token: string;
    AAccess_Secret: string;
    AFunc_Name: string;
    FCategory: string;
    function GetArgs(key: string): Variant;
    procedure SetArgs(key: string; const Value: Variant);
    procedure SetSns(const Value: string);
    procedure SetFunc_Name(const Value: string);
    procedure SetAccess_Token(const Value: string);
    function _Query: TNode;
    procedure SetAccess_Secret(const Value: string);
    procedure SetCategory(const Value: string);
  public
    property Category: string read FCategory write SetCategory;
    property Access_Token: string read AAccess_Token write SetAccess_Token;
    property Access_Secret: string read AAccess_Secret write SetAccess_Secret;
    property Func_Name: string read AFunc_Name write SetFunc_Name;
    property SNS: string read ASns write SetSns;
    property FreeOnCall: Boolean read AFreeOnCall write AFreeOnCall default true;
    property Args[key:string]:Variant read GetArgs write SetArgs;
    procedure AddArg(key: string;value: Variant);
    procedure RemoveArg(key: string);
    procedure ClearArg;
    function Query: TNode;
    function QueryURL: string;
    function QuerySmartURL: string;
    function QuerySmart: TNode;
    function ArgCount: Integer;
    constructor Create(access_token: string;access_secret: string);
    destructor Destroy; override;
  end;

implementation

{ TAPICall }

procedure TAPICall.AddArg(key: string; value: Variant);
begin
  Keys.Add(key);
  SetLength(Values, Keys.Count);
  Values[High(Values)] := Value;
end;

function TAPICall.ArgCount: Integer;
begin
  result := Keys.Count;
end;

procedure TAPICall.ClearArg;
begin
  Keys.Clear;
  Keys.Add('sns');
  Keys.Add('access_token');
  if AAccess_Secret <> '' then
  begin
    Keys.Add('access_secret');
    SetLength(Values, 3);
    Values[2] := AAccess_Secret;
  end
  else
    SetLength(Values, 2);
  Values[0] := ASNS;
  Values[1] := AAccess_Token;
end;

constructor TAPICall.Create(access_token: string;access_secret: string);
begin
  AAccess_Token := access_token;
  AAccess_Secret := access_secret;
  Keys := TStringList.Create;
  AFreeOnCall := True;
  SetLength(Values, 0);
  ClearArg;
end;

destructor TAPICall.Destroy;
begin
  Keys.Free;
  inherited;
end;

function TAPICall.GetArgs(key: string): Variant;
var
  i: Integer;
begin
  i := Keys.IndexOf(Key);
  if i<0 then
    result := UnAssigned
  else
    result := Values[i];
end;

function TAPICall._Query: TNode;
begin
  result := web_connect.SafeGetDataFromURL(QueryURL());
  if AFreeOnCall then
    self.ClearArg;
end;

function TAPICall.QueryURL: string;
var
  url, str: string;
  i: Integer;
begin
  url := Global.Intf_Call_URL+'?';
  for i:= 0 to High(Values) do
  begin
    if Variants.VarIsEmpty(Values) then exit;
    str := Format('%s=%s',[Keys[i], Values[i]]);
    url := url + str;
    if i <> High(Values) then
      url := url + '&';
  end;
  result := TIDURI.URLDecode(url);
end;

procedure TAPICall.RemoveArg(key: string);
var
  i,j:integer;
begin
  i := Keys.IndexOf(key);
  if i<0 then exit;
  Keys.Delete(i);
  for j := i to High(Values)-1 do
    Values[j] := Values[j+1];
  SetLength(Values, High(Values));
end;

procedure TAPICall.SetAccess_Secret(const Value: string);
var
  i: integer;
begin
  AAccess_Secret := Value;
  if Value = '' then exit;
  i := Keys.IndexOf('access_secret');
  if i < 0 then
    self.AddArg('access_secret', Value)
  else
    self.Args['access_secret'] := Value;
end;

procedure TAPICall.SetAccess_Token(const Value: string);
begin
  AAccess_Token := Value;
  self.Args['access_token'] := Value;
end;

procedure TAPICall.SetArgs(key: string; const Value: Variant);
var
  i: Integer;
begin
  i := -1;
  i := keys.IndexOf(Key);
  if i<0 then exit;
  Values[i] := Value;
end;

procedure TAPICall.SetCategory(const Value: string);
begin
  FCategory := Value;
  self.AddArg('category', Value);
end;

procedure TAPICall.SetFunc_Name(const Value: string);
begin
  AFunc_Name := Value;
  self.AddArg('func_name', Value);
end;

procedure TAPICall.SetSns(const Value: string);
begin
  ASNS := Value;
  self.Args['sns'] := Value;
end;

function TAPICall.Query: TNode;
begin
  result := self._Query;
end;

function TAPICall.QuerySmart: TNode;
begin
  result := GetDataFromURL(QuerySmartURL);
end;

function TAPICall.QuerySmartURL: string;
var
  url, str: string;
  i: Integer;
begin
  self.RemoveArg('func_name');
  url := Global.Intf_Smart_URL+'?';
  for i:= 0 to High(Values) do
  begin
    if Variants.VarIsEmpty(Values) then exit;
    str := Format('%s=%s',[Keys[i], Values[i]]);
    url := url + str;
    if i <> High(Values) then
      url := url + '&';
  end;
  result := TIDURI.URLDecode(url);
end;

end.
