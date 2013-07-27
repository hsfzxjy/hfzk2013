unit CustomView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, RVStyle, RVScroll, RichView, Contnrs, RVTable, Global;

type

  TCustomViewer = class(TFrame)
    reText: TRichView;
    RVStyle: TRVStyle;
  private
    FUpdating: boolean;

    procedure DoDraw;
    procedure InitRe;
    procedure InitTable;
  protected
    FContextClass: TCustomContextClass;
    ContextList: TObjectList;
    Table: TRVTableItemInfo;

    procedure DoAddCell(Index: Integer;AContext: TCustomContext); virtual; abstract;
  public
    function AddContext(context: TCustomContext): boolean;
    function DeleteContext(Index: Integer): boolean;
    function DeleteLast: boolean;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

{ TCustomViewer }

function TCustomViewer.AddContext(context: TCustomContext): boolean;
var
  con: TCustomContext;
begin
  result := False;
  if FUpdating then exit;
  con := FContextClass.Create;
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

constructor TCustomViewer.Create(AOwner: TComponent);
begin
  inherited;
  FUpdating := False;
  ContextList := TObjectList.Create;
end;

function TCustomViewer.DeleteContext(Index: Integer): boolean;
begin
  result := False;
  if FUpdating then exit;
  if (Index < 0) or (Index >= ContextList.Count) then exit;
  ContextList.Delete(Index);
  Table.DeleteRows(Index, 1, True);
  reText.Reformat;
  result := True;
end;

function TCustomViewer.DeleteLast: boolean;
begin
  result := DeleteContext(ContextList.Count-1);
end;

destructor TCustomViewer.Destroy;
begin
  ContextList.Free;
  inherited;
end;

procedure TCustomViewer.DoDraw;
var
  i: Integer;
  context: TCustomContext;
begin
  InitRe;
  InitTable;
  for I := 0 to ContextList.Count - 1 do
  begin
    context := ContextList.Items[i] as FContextClass;
    DoAddCell(i, context);
  end;
  reText.AddItem('table', Table);
  reText.Reformat;
end;

procedure TCustomViewer.InitRe;
begin
  reText.Clear;
end;

procedure TCustomViewer.InitTable;
begin
  Table := TRVTableItemInfo.Create(reText.RVData);
  Table.CellPadding := 0;
  Table.CellBorderWidth := 0;
  Table.CellHPadding := 0;
  Table.CellVPadding := 0;
  Table.CellVSpacing := 0;
  Table.CellHSpacing := 0;
end;

end.
