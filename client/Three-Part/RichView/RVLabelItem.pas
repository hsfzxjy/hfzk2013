{*******************************************************}
{                                                       }
{       RichView                                        }
{       Label Item - item class for RichView.           }
{       Non-text item that looks like a text            }
{       (but cannot be wrapped and edited)              }
{       Does not support Unicode.                       }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit RVLabelItem;

{$I RV_Defs.inc}

interface
uses
     {$IFDEF RICHVIEWDEF2009}AnsiStrings,{$ENDIF}
     SysUtils, Classes, Windows, Graphics, RVFuncs, Controls,
     RVScroll, RVStyle, RVItem, RVFMisc, DLines,
     RVClasses, RVTypes, RVUni;

const
  rvsLabel = -200;

type
  TRVLabelItemInfo = class(TRVRectItemInfo)
    private
      Width, Height, Descend: Integer;
      FMinWidth: Integer;
      FAlignment: TAlignment;
      FCanUseCustomPPI: Boolean;
      FParentRVData: TPersistent;
      procedure SetMinWidth(const Value: Integer);
      procedure SetAlignment(const Value: TAlignment);
      function GetRVStyle: TRVStyle;
    protected
      FUpdated: Boolean;
      procedure DoPaint(r: TRect; Canvas: TCanvas; State: TRVItemDrawStates;
        Style: TRVStyle; dli: TRVDrawLineInfo; ColorMode: TRVColorMode;
        const Text: String); virtual;
      function GetHeight: Integer; override;
      function GetWidth: Integer;  override;
      function GetAssociatedTextStyleNo: Integer; override;
      procedure SetAssociatedTextStyleNo(Value: Integer); override;
      procedure SetParentRVData(const Value: TPersistent); virtual;
      procedure SavePropertiesToRVF(Stream: TStream; RVData: TPersistent);
      procedure LoadPropertiesFromRVF(const s: TRVRawByteString; Index: Integer;
        RVData: TPersistent; UTF8Strings: Boolean; var TextStyleNameUsed: Boolean);
      function GetTextForPrintMeasuring(RVData: TPersistent): String; dynamic;
      function GetTextForPrinting(RVData: TPersistent;
        DrawItem: TRVDrawLineInfo): String; virtual;
    public
      Text: String;
      TextStyleNo: Integer;
      ProtectTextStyleNo: Boolean;
      Cursor: TCursor;
      constructor Create(RVData: TPersistent); override;
      constructor CreateEx(RVData: TPersistent; TextStyleNo: Integer;
        const Text: String);
      procedure MovingToUndoList(ItemNo: Integer; RVData,
        AContainerUndoItem: TObject); override;
      procedure MovingFromUndoList(ItemNo: Integer; RVData: TObject); override;
      function MouseMove(Shift: TShiftState; X, Y, ItemNo: Integer;
        RVData: TObject): Boolean; override;
      function GetMinWidth(sad: PRVScreenAndDevice; Canvas: TCanvas;
        RVData: TPersistent): Integer; override;
      function GetBoolValue(Prop: TRVItemBoolProperty): Boolean; override;
      function GetBoolValueEx(Prop: TRVItemBoolPropertyEx;
        RVStyle: TRVStyle): Boolean; override;
      procedure Paint(x,y: Integer; Canvas: TCanvas; State: TRVItemDrawStates;
        Style: TRVStyle; dli: TRVDrawLineInfo); override;
      procedure Print(Canvas: TCanvas; x,y,x2: Integer;
        Preview, Correction: Boolean; const sad: TRVScreenAndDevice;
        RichView: TRVScroller; dli: TRVDrawLineInfo; Part: Integer;
        ColorMode: TRVColorMode; RVData: TPersistent); override;
      procedure AfterLoading(FileFormat: TRVLoadFormat); override;
      procedure SaveRVF(Stream: TStream; RVData: TPersistent;
        ItemNo, ParaNo: Integer; const Name: TRVRawByteString; Part: TRVMultiDrawItemPart;
        ForceSameAsPrev: Boolean); override;
      function ReadRVFLine(const s: TRVRawByteString; RVData: TPersistent;
        ReadType, LineNo, LineCount: Integer; var Name: TRVRawByteString;
        var ReadMode: TRVFReadMode; var ReadState: TRVFReadState;
        UTF8Strings: Boolean; var AssStyleNameUsed: Boolean): Boolean; override;
      procedure Assign(Source: TCustomRVItemInfo); override;
      procedure MarkStylesInUse(Data: TRVDeleteUnusedStylesData); override;
      procedure UpdateStyles(Data: TRVDeleteUnusedStylesData); override;
      procedure ApplyStyleConversion(RVData: TPersistent;
        ItemNo, UserData: Integer); override;
      procedure UpdateMe;
      procedure OnDocWidthChange(DocWidth: Integer; dli: TRVDrawLineInfo;
        Printing: Boolean; Canvas: TCanvas; RVData: TPersistent;
        sad: PRVScreenAndDevice; var HShift, Desc: Integer;
        NoCaching, Reformatting: Boolean); override;
      function GetFinalPrintingWidth(Canvas: TCanvas;
        dli: TRVDrawLineInfo; RVData: TPersistent): Integer;
      procedure Execute(RVData:TPersistent);override;
      {$IFNDEF RVDONOTUSERTF}
      procedure SaveRTF(Stream: TStream; const Path: String;
        RVData: TPersistent; ItemNo: Integer;
        TwipsPerPixel: Double; Level: Integer; ColorList: TRVColorList;
        StyleToFont, ListOverrideOffsetsList1,
        ListOverrideOffsetsList2: TRVIntegerList; FontTable: TRVList); override;
      {$ENDIF}
      {$IFNDEF RVDONOTUSEHTML}
      procedure SaveToHTML(Stream: TStream; RVData: TPersistent;
        ItemNo: Integer; const Text: TRVRawByteString; const Path,
        imgSavePrefix: String; var imgSaveNo: Integer;
        CurrentFileColor: TColor; SaveOptions: TRVSaveOptions;
        UseCSS: Boolean; Bullets: TRVList); override;
      {$ENDIF}
      function AsText(LineWidth: Integer;
        RVData: TPersistent; const Text: TRVRawByteString; const Path: String;
        TextOnly,Unicode: Boolean): TRVRawByteString; override;
      procedure Inserted(RVData: TObject; ItemNo: Integer); override;
      procedure Changed;
      property MinWidth: Integer read FMinWidth write SetMinWidth;
      property Alignment: TAlignment read FAlignment write SetAlignment;
      property RVStyle: TRVStyle read GetRVStyle;
      property ParentRVData: TPersistent read FParentRVData write SetParentRVData;
  end;

implementation
uses CRVData, CRVFData, RichView, RVUndo;
{==============================================================================}
{ TRVLabelItemInfo }
constructor TRVLabelItemInfo.CreateEx(RVData: TPersistent;
  TextStyleNo: Integer; const Text: String);
begin
   inherited Create(RVData);
   StyleNo := rvsLabel;
   VAlign := rvvaBaseLine;
   Self.TextStyleNo := TextStyleNo;
   Self.Text    := Text;
   ParentRVData := RVData;
   FCanUseCustomPPI := rvflCanUseCustomPPI in TCustomRVData(RVData).Flags;
   Cursor := crDefault;
end;
{------------------------------------------------------------------------------}
constructor TRVLabelItemInfo.Create(RVData: TPersistent);
begin
  inherited Create(RVData);
  StyleNo := rvsLabel;
  ParentRVData := RVData;
  FCanUseCustomPPI := rvflCanUseCustomPPI in TCustomRVData(RVData).Flags;
  Cursor := crDefault;
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.AfterLoading(FileFormat: TRVLoadFormat);
begin
  inherited;
  FUpdated := False;
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.UpdateMe;
var DC: HDC;
    Canvas: TCanvas;
    TextMetric: TTextMetric;
begin
   if (RVStyle=nil) or FUpdated then
     exit;
   DC := GetDC(0);
   Canvas := TCanvas.Create;
   try
     Canvas.Handle := DC;
     RVStyle.ApplyStyle(Canvas, TextStyleNo, rvbdUnspecified, FCanUseCustomPPI,
       nil, False);
     FillChar(TextMetric, sizeof(TextMetric), 0);
     GetTextMetrics(Canvas.Handle, TextMetric);
     Descend := TextMetric.tmDescent;
     Height  := TextMetric.tmHeight;
     Width := Canvas.TextWidth(Text);
     if Width<MinWidth then
       Width := MinWidth;
   finally
     Canvas.Handle := 0;
     Canvas.Free;
     ReleaseDC(0,DC);
   end;
   FUpdated := True;
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.Assign(Source: TCustomRVItemInfo);
begin
  if Source is TRVLabelItemInfo then begin
    TextStyleNo := TRVLabelItemInfo(Source).TextStyleNo;
    Text    := TRVLabelItemInfo(Source).Text;
    ProtectTextStyleNo := TRVLabelItemInfo(Source).ProtectTextStyleNo;
    MinWidth := TRVLabelItemInfo(Source).MinWidth;
    Alignment := TRVLabelItemInfo(Source).Alignment;
    Cursor := TRVLabelItemInfo(Source).Cursor;
  end;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.DoPaint(r: TRect; Canvas: TCanvas;
  State: TRVItemDrawStates; Style: TRVStyle; dli: TRVDrawLineInfo;
  ColorMode: TRVColorMode; const Text: String);
var TextDrawState: TRVTextDrawStates;
    DTOption: Integer;
begin
  TextDrawState := [];
  if rvidsSelected in State then
    include(TextDrawState, rvtsSelected);
  if rvidsControlFocused in State then
    include(TextDrawState, rvtsControlFocused);
  if rvidsHover in State then
    include(TextDrawState, rvtsHover);
  RVStyle.ApplyStyle(Canvas, TextStyleNo, rvbdUnspecified,
    rvidsCanUseCustomPPI in State, nil, False);
  RVStyle.ApplyStyleColor(Canvas,TextStyleNo,TextDrawState, False, ColorMode);
  if not (rvidsSelected in State) and
     ((Style.FieldHighlightType=rvfhAlways) or
      ((Style.FieldHighlightType=rvfhCurrent) and
      ([rvidsCurrent,rvidsControlFocused]*State=[rvidsCurrent,rvidsControlFocused]))) then begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Style.FieldHighlightColor;
  end;
  case Alignment of
    taRightJustify:
      DTOption := DT_RIGHT;
    taCenter:
      DTOption := DT_CENTER;
    else
      DTOption := DT_LEFT;
  end;
  if Canvas.Brush.Style<>bsClear then
    Canvas.FillRect(r);
  DrawText(Canvas.Handle, PChar(Text), Length(Text), r,
    DT_SINGLELINE or DT_NOCLIP or DTOption);
  Canvas.Brush.Style := bsClear;
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.Paint(x, y: Integer; Canvas: TCanvas;
  State: TRVItemDrawStates; Style: TRVStyle; dli: TRVDrawLineInfo);
begin
  UpdateMe;
  DoPaint(Bounds(x, y, Width, Height), Canvas, State, Style, dli, rvcmColor, Text);
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.Print(Canvas: TCanvas; x, y, x2: Integer;
  Preview, Correction: Boolean; const sad: TRVScreenAndDevice;
  RichView: TRVScroller; dli: TRVDrawLineInfo; Part: Integer;
  ColorMode: TRVColorMode; RVData: TPersistent);
var r: TRect;
   DrawStates: TRVItemDrawStates;
begin
  UpdateMe;
  r := Rect(x, y, Width, Height);
  r.Right  := RV_XToDevice(r.Right,  sad);
  r.Bottom := RV_YToDevice(r.Bottom, sad);
  inc(r.Right,  x);
  inc(r.Bottom, y);
  DrawStates := [];
  if rvflCanUseCustomPPI in TCustomRVData(RVData).Flags then
    Include(DrawStates, rvidsCanUseCustomPPI);
  DoPaint(r, Canvas, DrawStates, TCustomRichView(RichView).Style, dli, ColorMode,
    GetTextForPrinting(RVData, dli));
end;
{------------------------------------------------------------------------------}
function TRVLabelItemInfo.GetBoolValueEx(Prop: TRVItemBoolPropertyEx;
  RVStyle: TRVStyle): Boolean;
begin
  case Prop of
    rvbpDisplayActiveState:
      Result := RVStyle.FieldHighlightType=rvfhCurrent;
    rvbpActualPrintSize:
      Result := True;
    rvbpJump, rvbpAllowsFocus,rvbpXORFocus:
      Result := RVStyle.TextStyles[TextStyleNo].Jump;
    rvbpHotColdJump:
      Result := RVStyle.TextStyles[TextStyleNo].Jump and
                RVStyle.StyleHoverSensitive(StyleNo);
   rvbpPrintToBMP:
     Result := False;
   else
     Result := inherited GetBoolValueEx(Prop, RVStyle);
  end;
end;
{------------------------------------------------------------------------------}
function TRVLabelItemInfo.GetBoolValue(Prop: TRVItemBoolProperty): Boolean;
begin
  case Prop of
    {$IFDEF RVUNICODESTR}
    rvbpCanSaveUnicode:
      Result := True;
    {$ENDIF}
    rvbpAlwaysInText:
      Result := True;
    rvbpDrawingChangesFont:
      Result := True;
    rvbpSwitchToAssStyleNo:
      Result := not ProtectTextStyleNo;
    else
      Result := inherited GetBoolValue(Prop);
  end;
end;
{------------------------------------------------------------------------------}
function TRVLabelItemInfo.GetHeight: Integer;
begin
  UpdateMe;
  Result := Height;
end;
{------------------------------------------------------------------------------}
function TRVLabelItemInfo.GetWidth: Integer;
begin
  UpdateMe;
  Result := Width;
end;
{------------------------------------------------------------------------------}
function TRVLabelItemInfo.GetMinWidth(sad: PRVScreenAndDevice; Canvas: TCanvas;
  RVData: TPersistent): Integer;
begin
  UpdateMe;
  Result := Width;
  if MinWidth>Result then
    Result := MinWidth;
  if Sad<>nil then
    Result := MulDiv(Result, sad.ppixDevice, sad.ppixScreen);
end;
{------------------------------------------------------------------------------}
function TRVLabelItemInfo.GetTextForPrintMeasuring(RVData: TPersistent): String;
begin
  Result := Text;
end;
{------------------------------------------------------------------------------}
function TRVLabelItemInfo.GetTextForPrinting(RVData: TPersistent;
  DrawItem: TRVDrawLineInfo): String;
begin
  Result := Text;
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.SavePropertiesToRVF(Stream: TStream; RVData: TPersistent);
begin
   RVFWriteLine(Stream, RVFSaveText(TCustomRVData(RVData).GetRVStyle,
    rvfoUseStyleNames in TCustomRVData(RVData).RVFOptions, TextStyleNo));
   RVFWriteLine(Stream, RVIntToStr(MinWidth));
   RVFWriteLine(Stream, RVIntToStr(ord(Alignment)));
   if ProtectTextStyleNo then
     RVFWriteLine(Stream, 'protect')
   else
     RVFWriteLine(Stream, 'no-protect');
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.SaveRVF(Stream: TStream; RVData: TPersistent;
  ItemNo, ParaNo: Integer; const Name: TRVRawByteString; Part: TRVMultiDrawItemPart;
  ForceSameAsPrev: Boolean);
begin
   // if you want to modify saving/loading, modify
   // 1) second parameter in header - number of additional lines
   // 2) lines after header
   // Do not change other parameters in header
   RVFWriteLine(Stream,
     {$IFDEF RVUNICODESTR}AnsiStrings.{$ENDIF}Format('%d %d %s %d %d %s %s',
       [StyleNo, 6+GetRVFExtraPropertyCount {Line count after header},
        RVFItemSavePara(ParaNo, TCustomRVData(RVData), ForceSameAsPrev),
        Byte(RVFGetItemOptions(ItemOptions, ForceSameAsPrev)) and RVItemOptionsMask,
        0 {text mode saving},
        RVFSaveTag(rvoTagsArePChars in TCustomRVData(RVData).Options,Tag),
        SaveRVFHeaderTail(RVData)]));
   // lines after header
   {0} RVFWriteLine(Stream, StringToRVFString(Text));
   {1,2,3,4} SavePropertiesToRVF(Stream, RVData);
   {5} RVFWriteLine(Stream, Name);
   SaveRVFExtraProperties(Stream);
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.LoadPropertiesFromRVF(const s: TRVRawByteString; Index: Integer;
  RVData: TPersistent; UTF8Strings: Boolean; var TextStyleNameUsed: Boolean);
var P: PRVAnsiChar;
begin
  case Index of
    0:
      begin
        P := PRVAnsiChar(s);
        RVFReadTextStyle(TCustomRVData(RVData).GetRVStyle, P, TextStyleNo,
          UTF8Strings, TextStyleNameUsed);
      end;
    1:
      MinWidth := RVStrToInt(s);
    2:
      Alignment := TAlignment(RVStrToInt(s));
    3:
      ProtectTextStyleNo := s='protect';
  end;
end;
{------------------------------------------------------------------------------}
function TRVLabelItemInfo.ReadRVFLine(const s: TRVRawByteString; RVData: TPersistent;
  ReadType, LineNo, LineCount: Integer; var Name: TRVRawByteString;
  var ReadMode: TRVFReadMode; var ReadState: TRVFReadState;
  UTF8Strings: Boolean; var AssStyleNameUsed: Boolean): Boolean;
begin
  case LineNo of
    0:
      Text := RVFStringToString(s, UTF8Strings);
    1..4:
      LoadPropertiesFromRVF(s, LineNo-1, RVData, UTF8Strings, AssStyleNameUsed);
    5:
      begin
        Name := s;
        ParentRVData := RVData;
      end;
    else
      SetExtraPropertyFromRVFStr(s, UTF8Strings);
  end;
  Result := True;
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.MarkStylesInUse(Data: TRVDeleteUnusedStylesData);
begin
  inherited MarkStylesInUse(Data);
  Data.UsedTextStyles[TextStyleNo] := 1;
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.UpdateStyles(Data: TRVDeleteUnusedStylesData);
begin
  inherited UpdateStyles(Data);
  dec(TextStyleNo,Data.UsedTextStyles[TextStyleNo]-1);
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.ApplyStyleConversion(RVData: TPersistent;
  ItemNo, UserData: Integer);
begin
  if ProtectTextStyleNo then
    exit;
  TCustomRVFormattedData(RVData).DoCurrentTextStyleConversion(TextStyleNo, ParaNo,
    ItemNo, UserData, False);
  FUpdated := False;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSERTF}
procedure TRVLabelItemInfo.SaveRTF(Stream: TStream; const Path: String;
  RVData: TPersistent; ItemNo: Integer; TwipsPerPixel: Double;
  Level: Integer; ColorList: TRVColorList; StyleToFont,
  ListOverrideOffsetsList1, ListOverrideOffsetsList2: TRVIntegerList;
  FontTable: TRVList);
begin

  RVFWrite(Stream,
    {$IFDEF RVUNICODESTR}
    RVMakeRTFStrW(Text, TCustomRVData(RVData).GetStyleCodePage(TextStyleNo),
      rvrtfDuplicateUnicode in TCustomRVData(RVData).RTFOptions, False, False)
    {$ELSE}
    RVMakeRTFStr(Text, False, True)
    {$ENDIF}
    );
end;
{$ENDIF}
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEHTML}
procedure TRVLabelItemInfo.SaveToHTML(Stream: TStream; RVData: TPersistent;
  ItemNo: Integer; const Text: TRVRawByteString; const Path, imgSavePrefix: String;
  var imgSaveNo: Integer; CurrentFileColor: TColor;
  SaveOptions: TRVSaveOptions; UseCSS: Boolean; Bullets: TRVList);
var s: TRVRawByteString;
begin
  {$IFDEF RVUNICODESTR}
  s := RVU_GetRawUnicode(Self.Text);
  if rvsoUTF8 in SaveOptions then
    s := RVU_UnicodeToUTF8(s, False)
  else
    s := RVU_GetHTMLEncodedUnicode(s, False);
  {$ELSE}
  s := RV_MakeHTMLStr(Self.Text, False);
  if rvsoUTF8 in SaveOptions then
    s := RVU_AnsiToUTF8(TCustomRVData(RVData).GetStyleCodePage(TextStyleNo), s);
  {$ENDIF}
  RVFWrite(Stream, s);
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TRVLabelItemInfo.AsText(LineWidth: Integer; RVData: TPersistent;
  const Text: TRVRawByteString; const Path: String;
  TextOnly, Unicode: Boolean): TRVRawByteString;
begin
  {$IFDEF RVUNICODESTR}
  if Unicode then
    Result := RVU_GetRawUnicode(Self.Text)
  else
    Result := TRVAnsiString(Self.Text);
  {$ELSE}
  Result := Self.Text;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.Inserted(RVData: TObject; ItemNo: Integer);
begin
  if RVData<>nil then begin
    ParentRVData := TCustomRVData(RVData);
    FCanUseCustomPPI := rvflCanUseCustomPPI in TCustomRVData(RVData).Flags;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.Execute(RVData: TPersistent);
begin
  if RVData is TCustomRVFormattedData then begin
    if GetBoolValueEx(rvbpJump, TCustomRVData(RVData).GetRVStyle) then
      TCustomRVFormattedData(RVData).DoJump(JumpID+
          TCustomRVFormattedData(RVData).FirstJumpNo)
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.SetMinWidth(const Value: Integer);
begin
  if FMinWidth<>Value then begin
    FMinWidth := Value;
    FUpdated := False;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.OnDocWidthChange(DocWidth: Integer;
  dli: TRVDrawLineInfo; Printing: Boolean; Canvas: TCanvas;
  RVData: TPersistent; sad: PRVScreenAndDevice; var HShift, Desc: Integer;
  NoCaching, Reformatting: Boolean);
var TextMetric: TTextMetric;
    sz: TSize;
    s: String;
begin
  inherited;
  if (sad=nil) or
     ((sad.ppixScreen=sad.ppixDevice) and (sad.ppiyScreen=sad.ppiyDevice)) then begin
    UpdateMe;
    dli.Width := Width;
    dli.Height := Height;
    Desc := Descend;
    end
  else begin
    RVStyle.ApplyStyle(Canvas, TextStyleNo, rvbdUnspecified,
      rvflCanUseCustomPPI in TCustomRVData(RVData).Flags, nil, False);
    FillChar(TextMetric, sizeof(TextMetric), 0);
    GetTextMetrics(Canvas.Handle, TextMetric);
    Desc := TextMetric.tmDescent;
    s := GetTextForPrintMeasuring(RVData);
    GetTextExtentPoint32(Canvas.Handle, PChar(s), Length(s), sz);
    dli.Width := sz.cx;
    dli.Height := sz.cy;
  end;
end;
{------------------------------------------------------------------------------}
function TRVLabelItemInfo.GetFinalPrintingWidth(Canvas: TCanvas;
  dli: TRVDrawLineInfo; RVData: TPersistent): Integer;
begin
  RVStyle.ApplyStyle(Canvas, TextStyleNo, rvbdUnspecified,
    rvflCanUseCustomPPI in TCustomRVData(RVData).Flags, nil, False);
  Result := Canvas.TextWidth(GetTextForPrinting(RVData, dli));
end;
{------------------------------------------------------------------------------}
function TRVLabelItemInfo.MouseMove(Shift: TShiftState; X, Y,
  ItemNo: Integer; RVData: TObject): Boolean;
begin
  Result := inherited MouseMove(Shift, X, Y, ItemNo, RVData);
  if Cursor<>crDefault then begin
    TCustomRVFormattedData(RVData).SetCursor(Cursor);
    Result := True;
  end;
end;
{------------------------------------------------------------------------------}
function TRVLabelItemInfo.GetAssociatedTextStyleNo: Integer;
begin
  Result := TextStyleNo;
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.SetAssociatedTextStyleNo(Value: Integer);
begin
  TextStyleNo := Value;
end;
{------------------------------------------------------------------------------}
function TRVLabelItemInfo.GetRVStyle: TRVStyle;
begin
  Result := TCustomRVData(FParentRVData).GetRVStyle;
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.SetParentRVData(const Value: TPersistent);
begin
  FParentRVData := Value;
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.MovingFromUndoList(ItemNo: Integer;
  RVData: TObject);
begin
  inherited;
  ParentRVData := TCustomRVData(RVData);
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.MovingToUndoList(ItemNo: Integer; RVData,
  AContainerUndoItem: TObject);
begin
  inherited;
  ParentRVData := TRVUndoInfo(AContainerUndoItem).GetUndoListOwnerRVData;
end;
{------------------------------------------------------------------------------}
procedure TRVLabelItemInfo.Changed;
begin
  FUpdated := False;
end;
{------------------------------------------------------------------------------}

initialization

  RegisterRichViewItemClass(rvsLabel, TRVLabelItemInfo);

end.
