unit wxEditEx;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxContainer, cxEdit, cxTextEdit, cxMemo, cxRichEdit, cxGroupBox, ExtCtrls,
  LinkLabel;

type
  TwxEditEx = class(TControl)
  private
    lblUserName: TLinkLabel;
    reText: TcxRichEdit;
    pnlObj, pnlControl, pnlText, pnlRetweet, pnlUserName: TPanel;
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Standard', [TwxEditEx]);
end;

{ TwxEditEx }

constructor TwxEditEx.Create(AOwner: TComponent);
begin
  inherited;
  Height := 300;
  Width := 300;
  pnlUserName := TPanel.Create(self);
  pnlUserName.Align := alTop;
  pnlUserName.Color := clBlack;
  pnlUserName.Height := 27;
  lblUserName := TLinkLabel.Create(pnlUserName);
  lblUserName.Align := alLeft;
  lblUserName.Caption := 'label1';
  pnlText := TPanel.Create(self);
  pnlText.Align := alClient;
  pnlControl := TPanel.Create(self);
  pnlControl.Align := alBottom;
  reText := TcxRichEdit.Create(pnlText);
  reText.Align := alTop;
  reText.Height := 100;
  pnlObj := TPanel.Create(pnlText);
  pnlObj.Align := alTop;
  pnlObj.Height := 10;
  pnlRetweet := TPanel.Create(pnlText);
  pnlRetweet.Align := alClient;
  repaint;
end;

destructor TwxEditEx.Destroy;
begin
  lblUserName.Free;
  pnlUserName.Free;
  reText.Free;
  pnlObj.Free;
  pnlRetweet.Free;
  pnlText.Free;
  pnlControl.Free;
  inherited;
end;

end.
