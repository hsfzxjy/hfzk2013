unit ViewerFactory;

interface

uses UserView, Forms, Classes, Sysutils, XMLObj, CustomViewFrame;

type
  TViewerType = (vtUser);

function CreateViewer(AOwner: TComponent;Category: TViewerType;Node: TNode): TCustomViewerFrame;

implementation

uses Controls;

const
  ViewerTypeStrs: array [vtUser..vtUser] of string = ('TUserViewer');

function CreateViewer(AOwner: TComponent;Category: TViewerType;Node: TNode): TCustomViewerFrame;
begin
  result := TCustomViewerFrameClass(GetClass(ViewerTypeStrs[Category])).Create(AOwner, Node);
  result.Parent := AOwner as TWinControl;
  result.Align := alClient;   
end;

end.
