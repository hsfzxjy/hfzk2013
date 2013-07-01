unit User_Intf;

interface

uses Sysutils, Global, XMLObj, IDHttp, NativeXML, Classes, Variants;

type
  TUser =class;
  TAccount = class;
  TAccount_Type = (atSina, atTwitter, atFacebook);

  TUser = class
  private
    http: TIdHttp;
    AID: Int64;
    AccountsList: TStringList;
    TwitterList, SinaList, FacebookList: TStringList;
    function GetAccounts(node: TAccount_Type; name: Variant): TAccount;
  public
    property ID: int64 read AID;
    property Accounts[node:TAccount_Type;name: Variant]: TAccount read GetAccounts;

    function AccountsCount(at: TAccount_Type): Integer;
    procedure Update;
    constructor Create(ID: int64);
    destructor Destroy;override;
  end;

  TAccount = class
  private
    http: TIdHttp;
    AOwner: TUser;
    AAccess_Token: string;
    AAccount_Name: string;
    AAccount_Type: TAccount_Type;
    AExpire_In: Int64;
    procedure Init;
    procedure Update(node: TNode);
  public
    property Owner: TUser read AOwner;
    property Access_Token: string read AAccess_Token;
    property Account_Name: string read AAccount_Name;
    property Account_Type: TAccount_Type read AAccount_Type;
    property Expire_In: Int64 read AExpire_In;

    constructor Create(Owner: TUser;access_token, account_name, account_type:string;
      expire_in: int64);overload;
    constructor Create(Owner: TUser;data: TNode);overload;
    destructor Destroy;override;
  end;

implementation

function get_type(str: string): TAccount_Type;
begin
  if str = 'sina' then result := atSina;
  if str = 'twitter' then result := atTwitter;
  if str = 'facebook' then result := atFaceBook;
end;

{ TAccount }

constructor TAccount.Create(Owner: TUser; access_token, account_name,
  account_type: string; expire_in: int64);
begin
  Init;
  self.AOwner :=Owner;
  AAccess_Token := access_token;
  AAccount_Name := account_name;
  AAccount_Type := get_type(account_type);
  AExpire_In := expire_in;
end;

constructor TAccount.Create(Owner: TUser; data: TNode);
begin
  Init;
  AOwner := owner;
  Update(data);
end;

destructor TAccount.Destroy;
begin
  Http.Free;
  inherited;
end;

procedure TAccount.Init;
begin
  Http := TIdHttp.Create(nil);
end;

procedure TAccount.Update(node: TNode);
begin
  self.AAccess_Token := node['access_token'].Value;
  self.AAccount_Name := node['account_name'].Value;
  self.AAccount_Type := get_type(node['account_type'].Value);
  self.AExpire_In := node['expire_in'].Value;
end;

{ TUser }

function TUser.AccountsCount(at: TAccount_Type): Integer;
begin
  result := TStringList(AccountsList.Objects[ord(at)]).Count;
end;

constructor TUser.Create(ID: int64);
begin
  AID := ID;
  AccountsList := TStringList.Create;
  SinaList := TStringList.Create;
  TwitterList := TStringList.Create;
  FacebookList := TStringList.Create;
  AccountsList.AddObject('sina', SinaList);
  AccountsList.AddObject('twitter', TwitterList);
  AccountsList.AddObject('facebook', FacebookList);
  http := TIdHttp.Create(nil);
  Update;
end;

destructor TUser.Destroy;
begin
  AccountsList.Free;
  SinaList.Free;
  TwitterList.Free;
  FacebookList.Free;
  http.Free;
  inherited;
end;

function TUser.GetAccounts(node: TAccount_Type; name: Variant): TAccount;
var
  accList: TStringList;
  i: Integer;
begin
  result := nil;
  accList := TStringList(AccountsList.Objects[Ord(node)]);
  try
    if Variants.VarIsStr(name) then
      i := accList.IndexOf(name)
    else if Variants.VarIsNumeric(name) then
      i := name;
    if i>=0 then
      result := TAccount(accList.Objects[i]);
  except

  end;
end;

procedure TUser.Update;
var
  str: string;
  xml: TNativeXML;
  node, tmp: TNode;
  i: Integer;
  acc: TStringList;
  account: TAccount;
begin
  str := http.Get(Format('%s?type=xml&ID=%d',[global.User_Query_URL, AID]));
  xml := TNativeXML.Create(nil);
  xml.ReadFromString(str);
  node := parse(xml)['accounts'];
  xml.Free;
  for i := 1 to node.Count do
  begin
    tmp := node[i];
    account := self.Accounts[get_type(tmp['account_type'].Value),tmp['account_name'].Value];
    if account <> nil then
    begin
      account.Update(tmp);
      continue;
    end;
    acc := TStringList(AccountsList.Objects[ord(get_type(tmp['account_type'].Value))]);
    acc.AddObject(tmp['account_name'].Value, TAccount.Create(self,tmp));
  end;
  node.Free;
end;

end.
