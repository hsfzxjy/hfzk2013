unit User_Intf;

interface

uses Sysutils, Global, XMLObj, web_connect, Classes, Variants, Data_Intf, NativeXML;

type
  TUser =class;
  TAccount = class;
  TAccount_Type = (atSina, atTwitter, atFacebook);

  TUser = class
  private
    AID: string;
    AccountsList: TStringList;
    TwitterList, SinaList, FacebookList: TStringList;
    function GetAccounts(node: TAccount_Type; name: Variant): TAccount;
  public
    property ID: string read AID;
    property Accounts[node:TAccount_Type;name: Variant]: TAccount read GetAccounts;

    function Combine(ID2: string): boolean; overload;
    function Combine(User2: TUser): boolean; overload;
    function DeleteAccount(AAccount_Type: TAccount_Type;AAccount_Name: string)
      :boolean; overload;
    function DeleteAccount(account: TAccount): boolean; overload;
    function AddAccount(AAccess_Token, AAccount_Name: String;
      AAccount_Type: TAccount_Type;AExpire_In: Longint): boolean;overload;
    function AddAccount(xml: string):boolean; overload;
    function AddAccount(xml: TNativeXML): boolean; overload;
    function AccountsCount(at: TAccount_Type): Integer;
    procedure Update;
    constructor Create(ID: string);
    destructor Destroy;override;
  end;

  TAccount = class
  private
    AOwner: TUser;
    AAccess_Token: string;
    AAccount_Name: string;
    AAccount_Type: TAccount_Type;
    AExpire_In: Longint;
    mAPI: TAPICall;
    procedure Init;
    procedure Update(node: TNode);
  public
    property Owner: TUser read AOwner;
    property Access_Token: string read AAccess_Token;
    property Account_Name: string read AAccount_Name;
    property Account_Type: TAccount_Type read AAccount_Type;
    property Expire_In: Longint read AExpire_In;
    property API: TAPICall read mAPI write mAPI;

    constructor Create(Owner: TUser;access_token, account_name, account_type:string;
      expire_in: Longint);overload;
    constructor Create(Owner: TUser;data: TNode);overload;
    destructor Destroy;override;
  end;

function CreateUser: TUser;
function get_type(str: string): TAccount_Type;

const
  TypeStrings : array [atSina..atFacebook] of String = ('sina','twitter','facebook');

implementation

var
  Create_URL:string;
  Delete_URL:string;
  Add_URL: string;
  Add_URL2: string;
  Combine_URL: string;

function CreateUser: TUser;
var
  node: TNode;
begin
  try
    node := GetDataFromURL(Create_URL);
    result := TUser.Create(node['ID'].Value);
  finally
    node.Free;
    result := nil;
  end;
end;

procedure Init;
begin
  Create_URL := User_Operate_URL+'?method=create';
  Delete_URL := User_Operate_URL+'?method=delete&ID=%s&account_type=%s&account_name=%s';
  Add_URL := User_Operate_URL+'?method=add&ID=%s&account_type=%s&account_name=%s' +
                             '&access_token=%s&expire_in=%d';
  Add_URL2 := User_Operate_URL+'?method=add&ID=%s&account_type=%s&account_name=%s' +
                             '&access_token=%s&access_secret=%s&expire_in=%d';
  Combine_URL := User_Operate_URL+'?method=combine&ID=%s&ID2=%s';
end;

function get_type(str: string): TAccount_Type;
begin
  if str = 'sina' then result := atSina;
  if str = 'twitter' then result := atTwitter;
  if str = 'facebook' then result := atFaceBook;
end;

{ TAccount }

constructor TAccount.Create(Owner: TUser; access_token, account_name,
  account_type: string; expire_in: Longint);
begin
  Init;
  self.AOwner := Owner;
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
  mAPI.Free;
  inherited;
end;

procedure TAccount.Init;
begin
  mAPI := TAPICall.Create(AAccess_Token);
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

function TUser.AddAccount(AAccess_Token, AAccount_Name: String;
  AAccount_Type: TAccount_Type; AExpire_In: Longint): boolean;
var
  ty, url: string;
  node: TNode;
  list: TStringList;
begin
  ty := TypeStrings[AAccount_Type];
  url := Format(Add_URL, [AID, ty, AAccount_Name,
                 AAccess_Token, AExpire_In]);
  result := True;
  try
    node := GetDataFromURL(url);
    if node.HasKey('_error') then result := False;
    if Result then
    begin
      list := TStringList(AccountsList.Objects[Ord(AAccount_Type)]);
      list.AddObject(AAccount_Name,TAccount.Create(
        self,
        AAccess_Token,
        AAccount_Name,
        TypeStrings[AAccount_Type],
        AExpire_In));
    end;
  finally
    node.Free;
  end;
end;

function TUser.AddAccount(xml: string): boolean;
var
  x: TNativeXML;
begin
  x := TNativeXML.Create(nil);
  x.ReadFromString(xml);
  result := AddAccount(x);
  x.Free;
end;

function TUser.AddAccount(xml: TNativeXML): boolean;
var
  node: TNode;
begin
  node := parse(xml);
  result := AddAccount(node['access_token'].Value, node['account_name'].Value,
      get_type(node['account_type'].Value), node['expire_in'].Value);
end;

function TUser.Combine(ID2: string): boolean;
var
  url: string;
  node: TNode;
begin
  url := Format(Combine_URL, [AID, ID2]);
  result := True;
  try
    node := web_connect.GetDataFromURL(url);
    result := not node.HasKey('_error');
    if result then
      self.Update;
  finally
    node.Free;
  end;
end;

function TUser.Combine(User2: TUser): boolean;
begin
  result := Combine(User2.ID);
  FreeAndNil(User2);
end;

constructor TUser.Create(ID: string);
begin
  AID := ID;
  AccountsList := TStringList.Create;
  SinaList := TStringList.Create;
  TwitterList := TStringList.Create;
  FacebookList := TStringList.Create;
  AccountsList.AddObject('sina', SinaList);
  AccountsList.AddObject('twitter', TwitterList);
  AccountsList.AddObject('facebook', FacebookList);
  Update;
end;

function TUser.DeleteAccount(AAccount_Type: TAccount_Type;
  AAccount_Name: string): boolean;
var
  node: TNode;
  url: string;
  list: TStringList;
begin
  url := Format(Delete_URL, [AID, TypeStrings[AAccount_Type],
                AAccount_Name]);
  result := true;
  try
    node := web_connect.GetDataFromURL(url);
    result := not node.HasKey('_error');
    if result then
    begin
      list := TStringList(AccountsList.Objects[Ord(AAccount_Type)]);
      list.Delete(list.IndexOf(AAccount_Name));
    end;
  finally
    node.Free;
  end;
end;

function TUser.DeleteAccount(account: TAccount): boolean;
begin
  result := DeleteAccount(account.Account_Type, account.Account_Name);
end;

destructor TUser.Destroy;
begin
  AccountsList.Free;
  SinaList.Free;
  TwitterList.Free;
  FacebookList.Free;
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
  xml: TNativeXML;
  node, tmp: TNode;
  i: Integer;
  acc: TStringList;
  account: TAccount;
begin
  node := web_connect.GetDataFromURL(
    Format('%s?type=xml&ID=%d',[global.User_Query_URL, AID]))['accounts'];
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

initialization

  Init;

end.
