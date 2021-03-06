
{*******************************************************}
{                                                       }
{       RichView                                        }
{       TRVDocParameters - class containing page layout }
{       properties                                      }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}


unit RVDocParams;

interface
{$I RV_Defs.inc}
uses
  Classes, Printers, SysUtils, Forms,
  RVFuncs, RVStyle, RVScroll, RVFMisc, RVTypes;
{$IFNDEF RVDONOTUSEDOCPARAMS}
{$IFDEF RICHVIEWDEF9}
{$A2}
{$ELSE}
{$IFDEF RICHVIEWDEF6}
{$A-}
{$ENDIF}
{$ENDIF}
type
  { -------------------------------------------------------------------------- }
  { TRVDocParameters - class for TCustomRichView.DocParameters property.
    Contains page layout properties. They are not used by TRichView itself,
    but can be saved in RVF and RTF.
  }
  TRVDocParameters = class(TPersistent)
  private
    FPageWidth, FPageHeight,
    FLeftMargin, FRightMargin, FTopMargin, FBottomMargin,
    FHeaderY, FFooterY: Extended;
    FUnits: TRVUnits;
    FOrientation: TPrinterOrientation;
    FZoomPercent: Integer;
    FZoomMode: TRVZoomMode;
    FMirrorMargins: Boolean;
    function StorePageHeight: Boolean;
    function StorePageWidth: Boolean;
    function StoreBottomMargin: Boolean;
    function StoreLeftMargin: Boolean;
    function StoreRightMargin: Boolean;
    function StoreTopMargin: Boolean;
    function StoreHeaderY: Boolean;
    function StoreFooterY: Boolean;
  public
    constructor Create;
    procedure Reset;
    procedure ResetLayout;
    procedure ConvertToUnits(AUnits: TRVUnits);
    procedure Assign(Source: TPersistent); override;
    function AreAllValuesDefault: Boolean;
    function GetRVFLineCount: Integer;
    procedure SaveToRVF(Stream: TStream);
    procedure ReadProperyFromString(const s: TRVAnsiString);
    {$IFNDEF RVDONOTUSERTF}
    procedure SaveToRTF(Stream: TStream);
    {$ENDIF}
    function ToTwips(Value: Extended): Integer;
    function FromTwips(Value: Integer): Extended;
    function UnitsPerInch(Units: TRVUnits): Extended;
  published
    property PageWidth:  Extended read FPageWidth  write FPageWidth  stored StorePageWidth;
    property PageHeight: Extended read FPageHeight write FPageHeight stored StorePageHeight;
    property Units: TRVUnits read FUnits write FUnits default rvuInches;
    property Orientation: TPrinterOrientation read FOrientation write FOrientation default poPortrait;
    property LeftMargin: Extended read FLeftMargin write FLeftMargin stored StoreLeftMargin;
    property RightMargin: Extended read FRightMargin write FRightMargin stored StoreRightMargin;
    property TopMargin: Extended read FTopMargin write FTopMargin stored StoreTopMargin;
    property BottomMargin: Extended read FBottomMargin write FBottomMargin stored StoreBottomMargin;
    property HeaderY: Extended read FHeaderY write FHeaderY stored StoreHeaderY;
    property FooterY: Extended read FFooterY write FFooterY stored StoreFooterY;
    property ZoomPercent: Integer read FZoomPercent write FZoomPercent default 100;
    property ZoomMode: TRVZoomMode read FZoomMode write FZoomMode default rvzmCustom;
    property MirrorMargins: Boolean read FMirrorMargins write FMirrorMargins default False;
  end;
{$ENDIF}


implementation
{$IFNDEF RVDONOTUSEDOCPARAMS}

const
  A4Width  = 8.2677165354; // A4 width in inches
  A4Height = 11.692913386; // A4 height in inches

// These types, constants and functions are using for saving and loading in RVF
type
  TRVDocParamId = (rvdpidNone,
    rvdpidUnits, rvdpidOrientation,
    rvdpidZoomPercent, rvdpidZoomMode,
    rvdpidPageWidth, rvdpidPageHeight,
    rvdpidLeftMargin, rvdpidRightMargin, rvdpidTopMargin, rvdpidBottomMargin,
    rvdpidHeaderY, rvdpidFooterY, rvdpidMirrorMargins);

const
  RVDocParamStrIdArr: array [TRVDocParamId] of TRVAnsiString =
    ('', 'Units', 'Orientation',
     'ZoomPercent', 'ZoomMode',
     'PageWidth', 'PageHeight',
     'LeftMargin', 'RightMargin', 'TopMargin', 'BottomMargin',
     'HeaderY', 'FooterY', 'MirrorMargins');

function RVGetDocParamId(const Key: TRVAnsiString): TRVDocParamId;
var i: TRVDocParamId;
begin
  for i := Low(TRVDocParamId) to High(TRVDocParamId) do
    if Key=RVDocParamStrIdArr[i] then begin
      Result := i;
      exit;
    end;
  Result := rvdpidNone;
end;
{==============================================================================}
constructor TRVDocParameters.Create;
begin
  inherited Create;
  Reset;
end;
{------------------------------------------------------------------------------}
{ Set all properties to default values }
procedure TRVDocParameters.Reset;
begin
  FUnits         := rvuInches;
  FOrientation   := poPortrait;
  FPageWidth     := A4Width;
  FPageHeight    := A4Height;
  FLeftMargin    := 1.25;
  FRightMargin   := 1.25;
  FTopMargin     := 1.0;
  FBottomMargin  := 1.0;
  FHeaderY       := 0.5;
  FFooterY       := 0.5;
  FZoomPercent   := 100;
  FZoomMode      := rvzmCustom;
  FMirrorMargins := False;
end;
{------------------------------------------------------------------------------}
{ Sets all sizes to defaults, measured in Units }
procedure TRVDocParameters.ResetLayout;
begin
  FOrientation   := poPortrait;
  FPageWidth     := A4Width*UnitsPerInch(Units);
  FPageHeight    := A4Height*UnitsPerInch(Units);
  FLeftMargin    := 1.25*UnitsPerInch(Units);
  FRightMargin   := 1.25*UnitsPerInch(Units);
  FTopMargin     := 1.0*UnitsPerInch(Units);
  FBottomMargin  := 1.0*UnitsPerInch(Units);
  FHeaderY       := 0.5*UnitsPerInch(Units);
  FFooterY       := 0.5*UnitsPerInch(Units);
  FMirrorMargins := False;
end;
{------------------------------------------------------------------------------}
{ Converts all sized to AUnits }
procedure TRVDocParameters.ConvertToUnits(AUnits: TRVUnits);
  {...........................................................................}
  function GetNewValue(Value: Extended): Extended;
  begin
    Result := Value*UnitsPerInch(AUnits)/UnitsPerInch(Units);
  end;
  {...........................................................................}
begin
  if Units=AUnits then
    exit;
  FPageWidth    := GetNewValue(FPageWidth);
  FPageHeight   := GetNewValue(FPageHeight);
  FLeftMargin   := GetNewValue(FLeftMargin);
  FRightMargin  := GetNewValue(FRightMargin);
  FTopMargin    := GetNewValue(FTopMargin);
  FBottomMargin := GetNewValue(FBottomMargin);
  FHeaderY      := GetNewValue(FHeaderY);
  FFooterY      := GetNewValue(FFooterY);
  Units         := AUnits;
end;
{------------------------------------------------------------------------------}
{ Specified whether PageHeight property is stored in DFM}
function TRVDocParameters.StorePageHeight: Boolean;
begin
  Result := Abs(FPageHeight-A4Height)>RVEps;
end;
{------------------------------------------------------------------------------}
{ Specified whether PageWidth property is stored in DFM }
function TRVDocParameters.StorePageWidth: Boolean;
begin
  Result := Abs(FPageWidth-A4Width)>RVEps;
end;
{------------------------------------------------------------------------------}
{ The same for margins properties }
function TRVDocParameters.StoreLeftMargin: Boolean;
begin
  Result := Abs(FLeftMargin-1.25)>RVEps;
end;

function TRVDocParameters.StoreRightMargin: Boolean;
begin
  Result := Abs(FRightMargin-1.25)>RVEps;
end;

function TRVDocParameters.StoreTopMargin: Boolean;
begin
  Result := Abs(FTopMargin-1.0)>RVEps;
end;

function TRVDocParameters.StoreBottomMargin: Boolean;
begin
  Result := Abs(FBottomMargin-1.0)>RVEps;
end;

function TRVDocParameters.StoreFooterY: Boolean;
begin
  Result := Abs(FFooterY-0.5)>RVEps;
end;

function TRVDocParameters.StoreHeaderY: Boolean;
begin
  Result := Abs(FHeaderY-0.5)>RVEps;
end;
{------------------------------------------------------------------------------}
{ Are all properties have default values? }
function TRVDocParameters.AreAllValuesDefault: Boolean;
begin
  Result := (FUnits=rvuInches) and (FOrientation=poPortrait) and
    (ZoomPercent=100) and (ZoomMode=rvzmCustom) and not MirrorMargins and
    not (StorePageHeight or StorePageWidth or StoreLeftMargin or StoreRightMargin
    or StoreTopMargin or StoreBottomMargin or StoreHeaderY or StoreFooterY);
end;
{------------------------------------------------------------------------------}
{ Assigns all properties of Source to this object, if Source is
  TRVDocParameters }
procedure TRVDocParameters.Assign(Source: TPersistent);
begin
  if Source is TRVDocParameters then begin
    PageWidth     := TRVDocParameters(Source).PageWidth;
    PageHeight    := TRVDocParameters(Source).PageHeight;
    Units         := TRVDocParameters(Source).Units;
    Orientation   := TRVDocParameters(Source).Orientation;
    LeftMargin    := TRVDocParameters(Source).LeftMargin;
    RightMargin   := TRVDocParameters(Source).RightMargin;
    TopMargin     := TRVDocParameters(Source).TopMargin;
    BottomMargin  := TRVDocParameters(Source).BottomMargin;
    HeaderY       := TRVDocParameters(Source).HeaderY;
    FooterY       := TRVDocParameters(Source).FooterY;
    ZoomPercent   := TRVDocParameters(Source).ZoomPercent;
    ZoomMode      := TRVDocParameters(Source).ZoomMode;
    MirrorMargins := TRVDocParameters(Source).MirrorMargins;
    end
  else
    inherited;
end;
{------------------------------------------------------------------------------}
{ Returns the number of properties having non-default values }
function TRVDocParameters.GetRVFLineCount: Integer;
begin
  Result := 0;
  if FUnits<>rvuInches then
    inc(Result);
  if FOrientation<>poPortrait then
    inc(Result);
  if StorePageHeight then
    inc(Result);
  if StorePageWidth then
    inc(Result);
  if StoreLeftMargin then
    inc(Result);
  if StoreRightMargin then
    inc(Result);
  if StoreTopMargin then
    inc(Result);
  if StoreBottomMargin then
    inc(Result);
  if StoreHeaderY then
    inc(Result);
  if StoreFooterY then
    inc(Result);
  if ZoomPercent<>100 then
    inc(Result);
  if ZoomMode<>rvzmCustom then
    inc(Result);
  if MirrorMargins then
    inc(Result);  
end;
{------------------------------------------------------------------------------}
{ Saves properties as lines Key=Value.
  Properties with default values are not saved. }
procedure TRVDocParameters.SaveToRVF(Stream: TStream);

  procedure SaveFloatProperty(id: TRVDocParamId; Value: Extended);
  var s: TRVAnsiString;
      p: Integer;
  begin
    s := RVDocParamStrIdArr[id]+'='+RVFloatToStr(Value);
    if DecimalSeparator<>'.' then begin
      p := RVPos(TRVAnsiString(TRVAnsiChar(DecimalSeparator)), s);
      if p<>0 then
        s[p] := '.';
    end;
    RVFWriteLine(Stream, s);
  end;

begin
  if FUnits<>rvuInches then
    RVFWriteLine(Stream, RVDocParamStrIdArr[rvdpidUnits]+'='+RVIntToStr(Ord(FUnits)));
  if FOrientation<>poPortrait then
    RVFWriteLine(Stream, RVDocParamStrIdArr[rvdpidOrientation]+'='+RVIntToStr(Ord(FOrientation)));
  if StorePageHeight then
    SaveFloatProperty(rvdpidPageHeight, PageHeight);
  if StorePageWidth then
    SaveFloatProperty(rvdpidPageWidth, PageWidth);
  if StoreLeftMargin then
    SaveFloatProperty(rvdpidLeftMargin, LeftMargin);
  if StoreRightMargin then
    SaveFloatProperty(rvdpidRightMargin, RightMargin);
  if StoreTopMargin then
    SaveFloatProperty(rvdpidTopMargin, TopMargin);
  if StoreBottomMargin then
    SaveFloatProperty(rvdpidBottomMargin, BottomMargin);
  if StoreHeaderY then
    SaveFloatProperty(rvdpidHeaderY, HeaderY);
  if StoreFooterY then
    SaveFloatProperty(rvdpidFooterY, FooterY);
  if ZoomPercent<>100 then
    RVFWriteLine(Stream, RVDocParamStrIdArr[rvdpidZoomPercent]+'='+RVIntToStr(ZoomPercent));
  if ZoomMode<>rvzmCustom then
    RVFWriteLine(Stream, RVDocParamStrIdArr[rvdpidZoomMode]+'='+RVIntToStr(ord(ZoomMode)));
  if FMirrorMargins then
    RVFWriteLine(Stream, RVDocParamStrIdArr[rvdpidMirrorMargins]+'=1');
end;
{------------------------------------------------------------------------------}
{ s has format Key=Value }
procedure TRVDocParameters.ReadProperyFromString(const s: TRVAnsiString);
var p: Integer;
    id: TRVDocParamId;
    Value: TRVAnsiString;

    function GetFloat: Extended;
    var p: Integer;
    begin
      if DecimalSeparator<>'.' then begin
        p := RVPos('.', Value);
        if p<>0 then
          Value[p] := TRVAnsiChar(DecimalSeparator);
      end;
      Result := RVStrToFloat(Value);
    end;

begin
  p := RVPos('=', s);
  if p=0 then
    exit;
  id := RVGetDocParamId(Copy(s, 1, p-1));
  Value := Copy(s, p+1, Length(s));
  case id of
    rvdpidUnits:
      Units := TRVUnits(RVStrToInt(Value));
    rvdpidOrientation:
      Orientation := TPrinterOrientation(RVStrToInt(Value));
    rvdpidZoomPercent:
      ZoomPercent := RVStrToInt(Value);
    rvdpidZoomMode:
      ZoomMode := TRVZoomMode(RVStrToInt(Value));
    rvdpidPageWidth:
      PageWidth := GetFloat;
    rvdpidPageHeight:
      PageHeight := GetFloat;
    rvdpidLeftMargin:
      LeftMargin := GetFloat;
    rvdpidRightMargin:
      RightMargin := GetFloat;
    rvdpidTopMargin:
      TopMargin := GetFloat;
    rvdpidBottomMargin:
      BottomMargin := GetFloat;
    rvdpidHeaderY:
      HeaderY := GetFloat;
    rvdpidFooterY:
      FooterY := GetFloat;
    rvdpidMirrorMargins:
      MirrorMargins := Value<>'0';
  end;

end;
{------------------------------------------------------------------------------}
const
  cmPerInch = 2.54;
  mmPerInch = 25.4;
  PicasPerInch = 6;
  PointsPerInch = 72;
{------------------------------------------------------------------------------}
{ Returns Units per 1 inch, where Units is the parameter }
function TRVDocParameters.UnitsPerInch(Units: TRVUnits): Extended;
begin
  case Units of
    rvuCentimeters:
      Result := cmPerInch;
    rvuMillimeters:
      Result := mmPerInch;
    rvuPicas:
      Result := PicasPerInch;
    rvuPixels:
      if RichViewPixelsPerInch=0 then
        Result := Screen.PixelsPerInch
      else
        Result := RichViewPixelsPerInch;
    rvuPoints:
      Result := PointsPerInch;
  else
    Result := 1;
  end;
end;
{------------------------------------------------------------------------------}
{ Converts value from Units to twips }
function TRVDocParameters.ToTwips(Value: Extended): Integer;
begin
  Result := Round(Value*1440/UnitsPerInch(Units));
end;
{------------------------------------------------------------------------------}
{ Converts value from twips to Units }
function TRVDocParameters.FromTwips(Value: Integer): Extended;
begin
  Result := Value*UnitsPerInch(Units)/1440;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSERTF}
{ Saving to RTF }
procedure TRVDocParameters.SaveToRTF(Stream: TStream);
begin
  if FOrientation=poLandscape then
    RVFWrite(Stream, '\landscape');
  RVFWrite(Stream, '\paperw'+RVIntToStr(ToTwips(PageWidth)));
  RVFWrite(Stream, '\paperh'+RVIntToStr(ToTwips(PageHeight)));
  RVFWrite(Stream, '\margl'+RVIntToStr(ToTwips(LeftMargin)));
  RVFWrite(Stream, '\margr'+RVIntToStr(ToTwips(RightMargin)));
  RVFWrite(Stream, '\margt'+RVIntToStr(ToTwips(TopMargin)));
  RVFWrite(Stream, '\margb'+RVIntToStr(ToTwips(BottomMargin)));
  if StoreHeaderY then
    RVFWrite(Stream, '\headery'+RVIntToStr(ToTwips(HeaderY)));
  if StoreFooterY then
    RVFWrite(Stream, '\footery'+RVIntToStr(ToTwips(FooterY)));
  RVFWrite(Stream, '\viewscale'+RVIntToStr(ZoomPercent));
  case ZoomMode of
    rvzmFullPage:
      RVFWrite(Stream, '\viewzk1');
    rvzmPageWidth:
      RVFWrite(Stream, '\viewzk2');
    rvzmCustom:
      RVFWrite(Stream, '\viewzk0');
  end;
  if MirrorMargins then
      RVFWrite(Stream, '\margmirror');
  RVFWriteLine(Stream, '');
end;
{$ENDIF}

{$ENDIF}





end.
