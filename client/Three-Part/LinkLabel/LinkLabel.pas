unit LinkLabel;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Graphics,
   Messages;

type
  TLinkLabel = class(TCustomLabel)
  private
    FURL: string;
    FCount: Integer;
    FMouseIn: boolean;
    FMouseInColor: TColor;
    FMouseOutColor: TColor;
    procedure SetMouseInColor(const Value: TColor);
    procedure SetMouseOutColor(const Value: TColor);
    procedure SetCount(const Value: Integer);
    { Private declarations }
  protected
    procedure Paint; override;
    procedure CMMouseEnter(var msg: TMessage);message CM_MOUSEENTER;
    procedure CMMouseLeave(var msg: TMessage);message CM_MOUSELEAVE;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Count: Integer read FCount write SetCount default 0;
    property URL: string read FURL write FURL;
    property MouseInColor: TColor read FMouseInColor write SetMouseInColor;
    property MouseOutColor: TColor read FMouseOutColor write SetMouseOutColor;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Constraints;
    property EllipsisPosition;
    property Enabled;
    property ShowHint;
    property Hint;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property Font;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Additional', [TLinkLabel]);
end;

{ TLinkLabel }

procedure TLinkLabel.CMMouseEnter(var msg: TMessage);
begin
  inherited;
  FMouseIn := true;
  repaint;
end;

procedure TLinkLabel.CMMouseLeave(var msg: TMessage);
begin
  inherited;
  FMouseIn := false;
  repaint;
end;

constructor TLinkLabel.Create(AOwner: TComponent);
begin
  inherited;
  Cursor := crHandPoint;
  FMouseIn := False;
  ParentFont := False;
  FMouseInColor := $F4C86C;
  FMouseOutColor := $C42038;
  repaint;
end;

procedure TLinkLabel.Paint;
begin
  inherited;
  if FMouseIn then
    Font.Color := FMouseInColor
  else
    Font.Color := FMouseOutColor;
end;

procedure TLinkLabel.SetCount(const Value: Integer);
const
  FmtStr = '(%d)';
var
  str, s, cap: string;
  i: Integer;
begin
  cap := Caption;
  if Value = 0 then
    str := ''
  else
    str := Format(FmtStr, [Value]);
  if FCount <> 0 then
  begin
    s := Format(FmtStr, [FCount]);
    i := Pos(s, Caption);
    Delete(Cap, i, Length(s));
  end;
  Caption := Cap + str;
  FCount := Value;
  repaint;
end;

procedure TLinkLabel.SetMouseInColor(const Value: TColor);
begin
  FMouseInColor := Value;
  repaint;
end;

procedure TLinkLabel.SetMouseOutColor(const Value: TColor);
begin
  FMouseOutColor := Value;
  repaint;
end;

end.
