unit XMLObj;

interface

uses Sysutils, Classes, Contnrs, NativeXML;

type
  TNodeType = (ntString, ntInteger, ntNull, ntSimple, ntBoolean, ntList, ntDict);

  TNodeDict = class;
  TNodeList = class;
  TNode = class;

  ENodeError = class(Exception)

  end;

  TNode = class(TPersistent)
  private
    AValue: Variant;
    AParent: TNode;
    ANodeType: TNodeType;
    AName: String;
  protected
    function GetValues(str: String): TNode;virtual;abstract;
    function GetItems(Index: Integer): TNode;virtual;abstract;
    function GetNode(Index: Variant): TNode;virtual;abstract;
    procedure SetNode(Index: Variant;const Value: TNode);virtual;abstract;
  public
    property Value: Variant read AValue write AValue;
    property Values[str: String]:TNode read GetValues;
    property Items[Index: Integer]:TNode read GetItems;
    property Parent: TNode read AParent;
    property Name: String read AName write AName;
    property NodeType: TNodeType read ANodeType write ANodeType;
    property Node[index: Variant]: TNode read GetNode write SetNode;default;

    procedure Assign(source: TPersistent); override;
    function HasChildren: Boolean; virtual;
    function Count: Integer; virtual; abstract;
    function HasKey(key: string):boolean; virtual; abstract;
    procedure Add(Key: string;Value: TNode);overload;virtual;abstract;
    procedure Add(Key, Name: string;NodeType: TNodeType;Value: Variant);
      overload;virtual;abstract;
    procedure Add(Value: TNode);overload;virtual;abstract;
    procedure Add(Name: String;NodeType: TNodeType;Value: Variant);
      overload;virtual;abstract;
    procedure Remove(Key: String);overload;virtual;abstract;
    procedure Remove(Index: Integer);overload;virtual;abstract;
    constructor Create(parent: TNode); virtual;
  end;

  TNodeDict = class(TNode)
  private
    ADict: TStringList;
  protected
    function GetNode(Index: Variant): TNode; override;
    procedure SetNode(Index: Variant;const Value: TNode); override;
    function GetValues(str: String): TNode;override;
  public
    procedure Assign(source: TPersistent); override;
    function HasKey(key: string): boolean; override;
    function Count: Integer; override;
    function HasChildren: Boolean; override;
    procedure Add(Key: string;Value: TNode);override;
    procedure Add(Key, Name: string;NodeType: TNodeType;Value: Variant);
      override;
    procedure Remove(Key: String); override;
    constructor Create(parent: TNode);override;
    destructor Destroy;override;
  end;

  TNodeList = class(TNode)
  private
    AList: TObjectList;
  protected
    function GetNode(Index: Variant): TNode; override;
    procedure SetNode(Index: Variant;const Value: TNode); override;
    function GetItems(index: Integer): TNode;override;
  public
    procedure Assign(source: TPersistent); override;
    function Count: Integer; override;
    function HasChildren: Boolean; override;
    procedure Add(Value: TNode);override;
    procedure Add(Name: String;NodeType: TNodeType;Value: Variant);override;
    procedure Remove(index: integer);override;
    constructor Create(parent: TNode);override;
    destructor Destroy; override;
  end;

function parse(xml: TNativeXML): TNode; overload;
function parse(xml: string): TNode; overload;

implementation

function do_list(xml: TXMLNode;parent: TNode):TNode;forward;
function do_dict(xml: TXMLNode;parent: TNode):TNode;forward;
function do_else(xml: TXMLNode;parent: TNode):TNode;forward;

function get_type(node: TXMLNode):TNodeType;
  var
    str: string;
  begin
    str := node.AttributeValueByName['type'];
    result := ntNull;
    if str = 'str' then result := ntString;
    if str = 'dict' then result := ntDict;
    if str = 'list' then result := ntList;
    if str = 'int' then result := ntInteger;
    if str = 'bool' then result := ntBoolean;
  end;

function do_else(xml: TXMLNode;parent: TNode):TNode;
var
  value: Variant;
  res: TNode;
  attr: TNodeType;
  text: String;
begin
  res := TNode.Create(parent);
  attr := get_type(xml);
  res.NodeType := attr;
  text := xml.Value;
  case attr of
    ntInteger: value := StrToInt64(text);
    ntBoolean: value := StrToBool(text);
    ntString: value := xml.Utf8ToWide(text);
  end;
  res.Name := xml.Name;
  res.Value := value;
  result := res;
end;

function do_dict(xml: TXMLNode;parent: TNode):TNode;
var
  res, value: TNode;
  i: Integer;
  item: TXMLNode;
  attr: TNodeType;
  name: string;
begin
  res := TNodeDict.Create(parent);
  res.Name := xml.Name;
  res.NodeType := ntDict;
  for i := 0 to xml.NodeCount-1 do
  begin
    item := xml.Nodes[i];
    attr := get_type(item);
    name := item.Name;
    case attr of
      ntList: value := do_list(item, res);
      ntDict: value := do_dict(item, res);
    else
      value := do_else(item, res);
    end;
    res.Add(name, value);
  end;
  result := res;
end;

function do_list(xml: TXMLNode;parent: TNode):TNode;
var
  res, value: TNode;
  i: Integer;
  item: TXMLNode;
  attr: TNodeType;
  name: string;
begin
  res := TNodeList.Create(parent);
  res.NodeType := ntList;
  for i := 0 to xml.NodeCount-1 do
  begin
    item := xml.Nodes[i];
    attr := get_type(item);
    name := item.Name;
    case attr of
      ntList: value := do_list(item, res);
      ntDict: value := do_dict(item, res);
    else
      value := do_else(item, res);
    end;
    value.Name := name;
    res.Add(value);
  end;
  result := res;
end;

function parse(xml: string): TNode;
var
  x: TNativeXML;
begin
  x := TNativeXML.Create(nil);
  x.ReadFromString(xml);
  result := parse(x);
end;

function parse(xml: TNativeXML): TNode;
var
  root: TXMLNode;
begin
  root := xml.Root;
  result := do_dict(root, nil);
end;

{ TNode }

procedure TNode.Assign(source: TPersistent);
var
  node: TNode;
begin
  if source is TNode then
  begin
    node := source as TNode;
    self.Value := node.Value;
    self.AParent := node.AParent;
    self.AName := node.AName;
    self.ANodeType := node.ANodeType;
    exit;
  end;
  inherited;
end;

constructor TNode.Create(parent: TNode);
begin
  AParent := parent;
end;

function TNode.HasChildren: Boolean;
begin
  result := False;
  if Ord(ANodeType) <= Ord(ntSimple) then exit;
end;

{ TNodeDict }

procedure TNodeDict.Add(Key: string; Value: TNode);
var
  i: Integer;
begin
  inherited;
  i := ADict.IndexOf(Key);
  if i >= 0 then
    ADict.Objects[i] := Value
  else
    ADict.AddObject(Key, Value);
end;

procedure TNodeDict.Add(Key, Name: string; NodeType: TNodeType;
  Value: Variant);
var
  node: TNode;
begin
  inherited;
  node := TNode.Create(self);
  node.NodeType := NodeType;
  node.Name := Name;
  node.Value := Value;
  Add(Key, node);
end;

procedure TNodeDict.Assign(source: TPersistent);
var
  node: TNodeDict;
begin
  if source is TNodeDict then
  begin
    node := source as TNodeDict;
    self.ADict.Assign(node.ADict);
    exit;
  end;
  inherited;
end;

function TNodeDict.Count: Integer;
begin
  result := ADict.Count;
end;

constructor TNodeDict.Create(parent: TNode);
begin
  inherited;
  NodeType := ntDict;
  ADict := TStringList.Create;
end;

destructor TNodeDict.Destroy;
begin
  ADict.Free;
  inherited;
end;

function TNodeDict.GetNode(Index: Variant): TNode;
begin
  result := Values[Index];
end;

function TNodeDict.GetValues(str: String): TNode;
var
  i: Integer;
begin
  result := nil;
  i := ADict.IndexOf(str);
  if i < 0 then exit;
  result := TNode(ADict.Objects[i]);
  if not Assigned(result) then
    raise ENodeError.Create('Key illegal!');
end;

function TNodeDict.HasChildren: Boolean;
begin
  result := ADict.Count <> 0;
end;

function TNodeDict.HasKey(key: string): boolean;
begin
  if ADict.IndexOf(key) < 0 then
    result := false
  else
    result := true;
end;

procedure TNodeDict.Remove(Key: String);
begin
  inherited;
  try
    ADict.Delete(ADict.IndexOf(Key));
  except
    raise ENodeError.Create('Key illegal!');
  end;
end;

procedure TNodeDict.SetNode(Index: Variant; const Value: TNode);
begin
  Add(Index, Value);
end;

{ TNodeList }

procedure TNodeList.Add(Name: String; NodeType: TNodeType; Value: Variant);
var
  node: TNode;
begin
  inherited;
  node := TNode.Create(self);
  node.NodeType := NodeType;
  node.Name := Name;
  node.Value := Value;
  AList.Add(node);
end;

procedure TNodeList.Assign(source: TPersistent);
var
  node: TNodeList;
begin
  if source is TPersistent then
  begin
    node := source as TNodeList;
    node.AList.Assign(node.AList);
    exit;
  end;
  inherited;
end;

procedure TNodeList.Add(Value: TNode);
begin
  inherited;
  AList.Add(Value);
end;

function TNodeList.Count: Integer;
begin
  result := AList.Count - 1;
end;

constructor TNodeList.Create(parent: TNode);
begin
  inherited;
  AList := TObjectList.Create;
  NodeType := ntList;
end;

destructor TNodeList.Destroy;
begin
  AList.Free;
  inherited;
end;

function TNodeList.GetItems(index: Integer): TNode;
begin
  result := nil;
  try
    result := TNode(AList.Items[index]);
  except

  end;
  if not Assigned(result) then
    raise ENodeError.Create('Index illegal!');
end;

function TNodeList.GetNode(Index: Variant): TNode;
begin
  result := Items[Index];
end;

function TNodeList.HasChildren: Boolean;
begin
  result := AList.Count <> 0;
end;

procedure TNodeList.Remove(index: integer);
begin
  try
    AList.Delete(index);
  except
    raise ENodeError.Create('Index illegal!');
  end;
end;

procedure TNodeList.SetNode(Index: Variant; const Value: TNode);
begin
  Add(Index, Value);
end;

end.
