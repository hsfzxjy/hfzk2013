unit FrmLargeImage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SkinData, ExtCtrls, DynamicSkinForm;

type
  TLargeImageForm = class(TForm)
    Image: TImage;
    SkinData: TspSkinData;
    CompressedSkinList: TspCompressedSkinList;
    spDynamicSkinForm1: TspDynamicSkinForm;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LargeImageForm: TLargeImageForm;

implementation

{$R *.dfm}

end.
