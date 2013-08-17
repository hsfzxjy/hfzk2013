unit CustomViewFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, XMLObj, DynamicSkinForm;

type
  TCustomViewerFrameClass = class of TCustomViewerFrame;

  TCustomViewerFrame = class(TFrame)
    SkinFrame: TspSkinFrame;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent;Node: TNode);overload;virtual;
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TCustomViewerFrame }

constructor TCustomViewerFrame.Create(AOwner: TComponent; Node: TNode);
begin
  inherited Create(AOwner);
end;

end.
