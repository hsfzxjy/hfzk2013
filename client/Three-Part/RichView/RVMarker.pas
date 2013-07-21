
{*******************************************************}
{                                                       }
{       RichView                                        }
{       TRVMarkerItemInfo: RichView item type           }
{       representing paragraph markers.                 }
{       TRVMarkerList: list of markers in the document  }
{       and all its subdocuments.                       }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit RVMarker;

interface
{$I RV_Defs.inc}
uses {$IFDEF RICHVIEWDEF2009}AnsiStrings,{$ENDIF}
     SysUtils, Windows, Classes, Controls, Graphics, Forms,
     RVFuncs, RVItem, RVStyle, DLines, RVFMisc, RVScroll, RVUni, RVClasses,
     RVStr, RVTypes;

{$IFNDEF RVDONOTUSELISTS}

type

  TRVMarkerList = class;

  TRVMarkerItemInfo = class (TRVRectItemInfo)
    private
      FWidth, FHeight, FDescent, FOverhang: Integer;
      FCachedIndexInList: Integer;
      procedure DoPaint(x,y: Integer; Canvas: TCanvas; State: TRVItemDrawStates;
        Style: TRVStyle; dli: TRVDrawLineInfo; ColorMode: TRVColorMode);
    protected
      function SaveRVFHeaderTail(RVData: TPersistent): TRVRawByteString; override;
      procedure CalcSize(Canvas: TCanvas; RVData: TPersistent;
        var Width, Height, Desc, Overhang: Integer;
        sad: PRVScreenAndDevice; ForMinWidth: Boolean;
        var HShift, SpaceBefore: Integer);
      procedure CalcDisplayString(RVStyle: TRVStyle; List: TRVMarkerList;
        Index: Integer);
      function GetHeight: Integer; override;
      function GetWidth: Integer; override;
      function GetLevelInfoEx(RVStyle: TRVStyle; LevelNo: Integer): TRVListLevel;
      function GetRVFExtraPropertyCount: Integer; override;
      procedure SaveRVFExtraProperties(Stream: TStream); override;
    public
      ListNo, Level: Integer;
      Counter: Integer;
      Reset: Boolean;
      StartFrom: Integer;
      DisplayString: String;
      NoHTMLImageSize: Boolean;
      constructor CreateEx(RVData: TPersistent;
        AListNo, ALevel, AStartFrom: Integer; AReset: Boolean);
      constructor Create(RVData: TPersistent); override;
      procedure Assign(Source: TCustomRVItemInfo); override;
      function GetLevelInfo(RVStyle: TRVStyle): TRVListLevel;
      function GetMinWidth(sad: PRVScreenAndDevice; Canvas: TCanvas;
        RVData: TPersistent): Integer; override;
      function GetBoolValue(Prop: TRVItemBoolProperty): Boolean; override;
      function GetBoolValueEx(Prop: TRVItemBoolPropertyEx;
        RVStyle: TRVStyle): Boolean; override;
      procedure OnDocWidthChange(DocWidth: Integer; dli: TRVDrawLineInfo; Printing: Boolean;
        Canvas: TCanvas; RVData: TPersistent; sad: PRVScreenAndDevice;
        var HShift, Desc: Integer; NoCaching, Reformatting: Boolean); override;
      procedure Paint(x,y: Integer; Canvas: TCanvas; State: TRVItemDrawStates;
        Style: TRVStyle; dli: TRVDrawLineInfo); override;
      procedure Print(Canvas: TCanvas; x,y,x2: Integer; Preview, Correction: Boolean;
        const sad: TRVScreenAndDevice; RichView: TRVScroller; dli: TRVDrawLineInfo;
        Part: Integer; ColorMode: TRVColorMode; RVData: TPersistent); override;
      function PrintToBitmap(Bkgnd: TBitmap; Preview: Boolean; RichView: TRVScroller;
          dli: TRVDrawLineInfo; Part: Integer; ColorMode: TRVColorMode):Boolean; override;
      function ReadRVFHeaderTail(var P: PRVAnsiChar; RVData: TPersistent;
        UTF8Strings: Boolean; var AssStyleNameUsed: Boolean): Boolean; override;
      procedure SaveRVF(Stream: TStream; RVData: TPersistent;
        ItemNo, ParaNo: Integer; const Name: TRVRawByteString; Part: TRVMultiDrawItemPart;
        ForceSameAsPrev: Boolean); override;
      procedure MovingToUndoList(ItemNo: Integer; RVData, AContainerUndoItem: TObject); override;
      procedure MovingFromUndoList(ItemNo: Integer; RVData: TObject); override;
      function GetImageWidth(RVStyle: TRVStyle): Integer; override;
      function GetImageHeight(RVStyle: TRVStyle): Integer; override;
      function GetLeftOverhang: Integer; override;
      {$IFNDEF RVDONOTUSEHTML}
      procedure HTMLOpenOrCloseTags(Stream: TStream;
        OldLevelNo, NewLevelNo: Integer; RVStyle: TRVStyle; UseCSS: Boolean);
      procedure SaveHTMLSpecial(Stream: TStream; Prev: TRVMarkerItemInfo;
        RVStyle: TRVStyle; UseCSS: Boolean);
      procedure SaveToHTML(Stream: TStream; RVData: TPersistent;
        ItemNo: Integer; const Text: TRVRawByteString; const Path,
        imgSavePrefix: String; var imgSaveNo: Integer;
        CurrentFileColor: TColor; SaveOptions: TRVSaveOptions;
        UseCSS: Boolean; Bullets: TRVList); override;
      function GetLICSS(RVData: TPersistent; ItemNo: Integer; const Path,
        imgSavePrefix: String; var imgSaveNo: Integer; CurrentFileColor: TColor;
        SaveOptions: TRVSaveOptions; Bullets: TRVList): String;
      {$ENDIF}
      {$IFNDEF RVDONOTUSERTF}
      procedure FillRTFTables(ColorList: TRVColorList;
        ListOverrideCountList: TRVIntegerList; RVData: TPersistent); override;
      procedure SaveRTF(Stream: TStream; const Path: String;
        RVData: TPersistent; ItemNo: Integer;
        TwipsPerPixel: Double; Level: Integer;
        ColorList: TRVColorList; StyleToFont, ListOverrideOffsetsList1,
        ListOverrideOffsetsList2: TRVIntegerList; FontTable: TRVList); override;
      {$ENDIF}
      procedure MarkStylesInUse(Data: TRVDeleteUnusedStylesData); override;
      procedure UpdateStyles(Data: TRVDeleteUnusedStylesData); override;
      function AsText(LineWidth: Integer; RVData: TPersistent;
        const Text: TRVRawByteString; const Path: String;
        TextOnly,Unicode: Boolean): TRVRawByteString; override;
      function GetIndexInList(List: TList): Integer;
      function SetExtraIntProperty(Prop: TRVExtraItemProperty;
        Value: Integer): Boolean; override;
      function GetExtraIntProperty(Prop: TRVExtraItemProperty;
        var Value: Integer): Boolean; override;
  end;

  TRVMarkerList = class (TList)
  public
    PrevMarkerList: TRVMarkerList;
    function InsertAfter(InsertMe, AfterMe: TRVMarkerItemInfo): Integer;
    procedure RecalcCounters(StartFrom: Integer; RVStyle: TRVStyle);
    function FindParentMarker(Index: Integer; Marker: TRVMarkerItemInfo;
      var ParentList: TRVMarkerList; var ParentIndex: Integer): Boolean;
    procedure RecalcDisplayStrings(RVStyle: TRVStyle);
    procedure SaveToStream(Stream: TStream; Count: Integer; IncludeSize: Boolean);
    procedure LoadFromStream(Stream: TStream; RVData: TPersistent;
      IncludeSize: Boolean);
    procedure SaveTextToStream(Stream: TStream; Count: Integer);
    procedure LoadText(const s: TRVAnsiString; RVData: TPersistent);
    procedure LoadBinary(const s: TRVRawByteString; RVData: TPersistent);
  end;

  function RVGetLevelInfo(RVStyle: TRVStyle; ListNo, Level: Integer): TRVListLevel;

{$ENDIF}

implementation
{$IFNDEF RVDONOTUSELISTS}
uses CRVData, CRVFData, RichView;

{============================= TRVMarkerItemInfo ==============================}
{ Constructor with additional parameters }
constructor TRVMarkerItemInfo.CreateEx(RVData: TPersistent; AListNo,
  ALevel, AStartFrom: Integer; AReset: Boolean);
begin
  inherited Create(RVData);
  StyleNo   := rvsListMarker;
  ListNo    := AListNo;
  Level     := ALevel;
  StartFrom := AStartFrom;
  Reset     := AReset;
  SameAsPrev := False;
  Counter   := 1;
  FCachedIndexInList := -1;
end;
{------------------------------------------------------------------------------}
{ Virtual constructor }
constructor TRVMarkerItemInfo.Create(RVData: TPersistent);
begin
  inherited Create(RVData);
  SameAsPrev := False;
  Counter   := 1;
  FCachedIndexInList := -1;  
end;
{------------------------------------------------------------------------------}
{ Assigns properties of Source to this list marker item }
procedure TRVMarkerItemInfo.Assign(Source: TCustomRVItemInfo);
begin
  if Source is TRVMarkerItemInfo then begin
    ListNo    := TRVMarkerItemInfo(Source).ListNo;
    Level     := TRVMarkerItemInfo(Source).Level;
    StartFrom := TRVMarkerItemInfo(Source).StartFrom;
    Reset     := TRVMarkerItemInfo(Source).Reset;
    NoHTMLImageSize := TRVMarkerItemInfo(Source).NoHTMLImageSize;
  end;
  inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
{ This method is called before moving this item to undo list (on deletion, for
  example) }
procedure TRVMarkerItemInfo.MovingToUndoList(ItemNo: Integer; RVData,
  AContainerUndoItem: TObject);
begin
  inherited;
  TCustomRVData(RVData).DeleteMarkerFromList(TCustomRVData(RVData).GetItem(ItemNo), False);
end;
{------------------------------------------------------------------------------}
{ This method is called before moving this item from undo list back to the
  document (on deletion undo, for example) }
procedure TRVMarkerItemInfo.MovingFromUndoList(ItemNo: Integer; RVData: TObject);
begin
  inherited;
  TCustomRVData(RVData).AddMarkerInList(ItemNo);
end;
{------------------------------------------------------------------------------}
{ Reads the rest of RVF header line }
function TRVMarkerItemInfo.ReadRVFHeaderTail(var P: PRVAnsiChar;
  RVData: TPersistent; UTF8Strings: Boolean; var AssStyleNameUsed: Boolean): Boolean;
    {$IFDEF RICHVIEWCBDEF3}
    function ListNameToIndex(const aStyleName: String): Integer;
    var i: Integer;
    begin
      with TCustomRVData(RVData).GetRVStyle.ListStyles do
        for i := 0 to Count-1 do
          if Items[i].StyleName = aStyleName then begin
            Result := i;
            exit;
          end;
      Result := 0;
    end;
    {$ENDIF}
var v: Integer;
begin
  Result :=  RVFReadInteger(P,ListNo);
  {$IFDEF RICHVIEWCBDEF3}
  AssStyleNameUsed := not Result;
  if not Result then begin
    ListNo := ListNameToIndex(RVFStringToString(RVFReadText(P), UTF8Strings));
    Result := True;
  end;
  {$ELSE}
  AssStyleNameUsed := False;
  {$ENDIF}

  Result :=  Result and RVFReadInteger(P,Level) and
    RVFReadInteger(P,StartFrom) and
    RVFReadInteger(P,v);
  Reset := v<>0;
end;
{------------------------------------------------------------------------------}
{ Returns the rest of RVF header line }
function TRVMarkerItemInfo.SaveRVFHeaderTail(RVData: TPersistent): TRVRawByteString;
  {......................................}
  function GetListNoStr: String;
  var RVStyle: TRVStyle;
  begin
    {$IFDEF RICHVIEWCBDEF3}
    RVStyle := TCustomRVData(RVData).GetRVStyle;
    if (ListNo>=0) and (ListNo<RVStyle.ListStyles.Count) and
      (rvfoUseStyleNames in TCustomRVData(RVData).RVFOptions) then
        Result := AnsiQuotedStr(RVStyle.ListStyles[ListNo].StyleName, '"')
      else
    {$ENDIF}
        Result := IntToStr(ListNo);
  end;
  {......................................}
begin
  Result := {$IFDEF RVUNICODESTR}AnsiStrings.{$ENDIF}Format('%s %d %d %d',
    [StringToRVFString(GetListNoStr), Level, StartFrom, ord(Reset)]);
end;
{------------------------------------------------------------------------------}
{ Saves this item to RVF Stream }
procedure TRVMarkerItemInfo.SaveRVF(Stream: TStream; RVData: TPersistent;
  ItemNo, ParaNo: Integer; const Name: TRVRawByteString; Part: TRVMultiDrawItemPart;
  ForceSameAsPrev: Boolean);
begin
  RVFWriteLine(Stream, {$IFDEF RVUNICODESTR}AnsiStrings.{$ENDIF}Format('%d %d %s %d %d %s %s',
    [StyleNo, GetRVFExtraPropertyCount,
     RVFItemSavePara(ParaNo, TCustomRVData(RVData), False),
     Byte(ItemOptions) and RVItemOptionsMask,
     0, RVFSaveTag(rvoTagsArePChars in TCustomRVData(RVData).Options,Tag),
     SaveRVFHeaderTail(RVData)]));
  SaveRVFExtraProperties(Stream);
end;
{------------------------------------------------------------------------------}
{ Returns list level, where list style index is defined in the properties of this
  item (ListNo), RVStyle and LevelNo are in the parameters.
  If LevelNo is too large, the last level is returned. }
function TRVMarkerItemInfo.GetLevelInfoEx(RVStyle: TRVStyle; LevelNo: Integer): TRVListLevel;
begin
  if LevelNo>=RVStyle.ListStyles[ListNo].Levels.Count then
    LevelNo :=RVStyle.ListStyles[ListNo].Levels.Count-1;
  Result := RVStyle.ListStyles[ListNo].Levels[LevelNo];
end;
{------------------------------------------------------------------------------}
{ Returns the list level by the specified parameters.
  If ListNo<0, returns nil.
  If ListNo is too large, returns the last style.
  If ListLevel is too large, returns the last level. }
function RVGetLevelInfo(RVStyle: TRVStyle; ListNo, Level: Integer): TRVListLevel;
var LevelNo: Integer;
begin
  Result := nil;
  if ListNo<0 then
    exit;
  LevelNo := Level;
  if LevelNo>=RVStyle.ListStyles[ListNo].Levels.Count then
    LevelNo :=RVStyle.ListStyles[ListNo].Levels.Count-1;
  if LevelNo<RVStyle.ListStyles[ListNo].Levels.Count then
    Result := RVStyle.ListStyles[ListNo].Levels[LevelNo];
end;
{------------------------------------------------------------------------------}
{ Returns the list level by the specified parameters.
  If ListNo<0, returns nil.
  The following checks are performed only if RichViewDoNotCheckRVFStyleRefs
  (global variable) is True:
    - If ListNo is too large, returns the last style.
    - If ListLevel is too large, returns the last level. }
function TRVMarkerItemInfo.GetLevelInfo(RVStyle: TRVStyle): TRVListLevel;
var LevelNo: Integer;
begin
  Result := nil;
  if ListNo<0 then
    exit;
  if RichViewDoNotCheckRVFStyleRefs then
    exit;    
  LevelNo := Level;
  if LevelNo<0 then
    exit;
  if LevelNo>=RVStyle.ListStyles[ListNo].Levels.Count then
    LevelNo :=RVStyle.ListStyles[ListNo].Levels.Count-1;
  if LevelNo<RVStyle.ListStyles[ListNo].Levels.Count then
    Result := RVStyle.ListStyles[ListNo].Levels[LevelNo];
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.CalcSize(Canvas: TCanvas; RVData: TPersistent;
  var Width, Height, Desc, Overhang: Integer; sad: PRVScreenAndDevice;
  ForMinWidth: Boolean; var HShift, SpaceBefore: Integer);
var sz: TSize;
    LevelInfo: TRVListLevel;
    TextMetric: TTextMetric;
    RVStyle: TRVStyle;
    {.........................................................}
    procedure CountLR(var LeftWidth, RightWidth: Integer);
    begin
      case LevelInfo.MarkerAlignment of
        rvmaLeft:
          begin
            LeftWidth := 0;
            RightWidth := Width;
          end;
        rvmaRight:
          begin
            LeftWidth := Width;
            RightWidth := 0;
          end;
        rvmaCenter:
          begin
            RightWidth := Width div 2;
            LeftWidth := Width - RightWidth;
          end;
      end;
    end;
    {.........................................................}
    procedure CountWidth(UseSad: Boolean);
    var  LeftWidth, RightWidth, w: Integer;
    begin
      CountLR(LeftWidth, RightWidth);
      if UseSaD and (sad<>nil) then
        w := MulDiv(LevelInfo.FirstIndent+LevelInfo.LeftIndent-LevelInfo.MarkerIndent, sad.ppixDevice, sad.ppixScreen)
      else
        w := LevelInfo.FirstIndent+LevelInfo.LeftIndent-LevelInfo.MarkerIndent;
      if ForMinWidth then begin
        Width := RightWidth;
        if Width<w then
          Width := w;
        end
      else begin
        if RightWidth<w then
          RightWidth := w;
        if TCustomRVData(RVData).GetParaBiDiMode(ParaNo)=rvbdRightToLeft then begin
          HShift :=  LeftWidth;
          SpaceBefore := LeftWidth+RightWidth-Width;
          end
        else begin
          HShift := - LeftWidth;
          SpaceBefore := 0;
        end;
        Width := LeftWidth+RightWidth;
        Overhang := HShift;
      end;
    end;
    {.........................................................}
begin
  if (ListNo<0) or (Level<0) then begin
    Width := 0;
    Height := 0;
    HShift := 0;
    Desc := 0;
    Overhang := 0;
    SpaceBefore := 0;
    exit;
  end;
  RVStyle := TCustomRVData(RVData).GetRVStyle;
  LevelInfo := GetLevelInfo(RVStyle);
  Desc := 0;
  case LevelInfo.ListType of
    rvlstPicture:
      begin
        if LevelInfo.HasPicture then begin
          Width := LevelInfo.Picture.Graphic.Width;
          Height := LevelInfo.Picture.Graphic.Height;
          if sad<>nil then begin
            Width := MulDiv(Width, sad.ppixDevice, sad.ppixScreen);
            Height := MulDiv(Height, sad.ppiyDevice, sad.ppiyScreen);
          end;
          end
        else begin
          Width := 0;
          Height := 0;
        end;
        CountWidth(True);
      end;
    rvlstImageList, rvlstImageListCounter:
      begin
        if LevelInfo.ImageList<>nil then begin
          Width := TImageList(LevelInfo.ImageList).Width;
          Height := TImageList(LevelInfo.ImageList).Height;
          if sad<>nil then begin
            Width := MulDiv(Width, sad.ppixDevice, sad.ppixScreen);
            Height := MulDiv(Height, sad.ppiyDevice, sad.ppiyScreen);
          end;
          end
        else begin
          Width := 0;
          Height := 0;
        end;
        CountWidth(True);
      end;
    {$IFNDEF RVDONOTUSEUNICODE}
    {$IFDEF RICHVIEWCBDEF3}
    rvlstUnicodeBullet:
      begin
        Canvas.Font := LevelInfo.Font;
        if (RVStyle.TextStyles.PixelsPerInch<>0) and (LevelInfo.Font.Size>0) then
          Canvas.Font.Height := - MulDiv(LevelInfo.Font.Size, RVStyle.TextStyles.PixelsPerInch, 72);
        {$IFNDEF RVDONOTUSECHARSPACING}
        SetTextCharacterExtra(Canvas.Handle, 0);
        {$ENDIF}
        SetTextAlign(Canvas.Handle, TA_LEFT);
        GetTextExtentPoint32W(Canvas.Handle, Pointer(LevelInfo.FormatStringW),
           Length(LevelInfo.FormatStringW), sz);
        GetTextMetrics(Canvas.Handle, TextMetric);
        Desc := TextMetric.tmDescent;
        Width := sz.cx;
        Height := sz.cy;
        CountWidth(True);
      end;
    {$ENDIF}
    {$ENDIF}
    else
      begin
        Canvas.Font := LevelInfo.Font;
        if (RVStyle.TextStyles.PixelsPerInch<>0) and (LevelInfo.Font.Size>0) then
          Canvas.Font.Height := - MulDiv(LevelInfo.Font.Size, RVStyle.TextStyles.PixelsPerInch, 72);
        {$IFNDEF RVDONOTUSECHARSPACING}
        SetTextCharacterExtra(Canvas.Handle, 0);
        {$ENDIF}
        SetTextAlign(Canvas.Handle, TA_LEFT);
        {$IFDEF RVUNICODESTR}GetTextExtentPoint32W{$ELSE}GetTextExtentPoint32A{$ENDIF}
          (Canvas.Handle, PChar(DisplayString),
           Length(DisplayString), sz);
        GetTextMetrics(Canvas.Handle, TextMetric);
        Desc := TextMetric.tmDescent;
        Width := sz.cx;
        Height := sz.cy;
        CountWidth(True);
      end;
  end;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetBoolValue(Prop: TRVItemBoolProperty): Boolean;
begin
  case Prop of
    rvbpDrawingChangesFont, rvbpCanSaveUnicode {, rvbpAlwaysInText}:
      Result := True;
    else
      Result := inherited GetBoolValue(Prop);
  end;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetBoolValueEx(Prop: TRVItemBoolPropertyEx; RVStyle: TRVStyle): Boolean;
var LevelInfo: TRVListLevel;
begin
  case Prop of
    rvbpActualPrintSize:
      begin
        (*
        LevelInfo := GetLevelInfo(RVStyle);
        if LevelInfo<>nil then
          Result := LevelInfo.ListType in [rvlstDecimal, rvlstLowerAlpha, rvlstUpperAlpha,
            rvlstBullet,
            rvlstLowerRoman, rvlstUpperRoman {$IFNDEF RVDONOTUSEUNICODE}, rvlstUnicodeBullet{$ENDIF}]
        else
        *)
          Result := True;
      end;
    rvbpPrintToBMP:
      begin
        LevelInfo := GetLevelInfo(RVStyle);
        if LevelInfo<>nil then
          Result := (LevelInfo.ListType in [rvlstImageList, rvlstImageListCounter]) or
                  ((LevelInfo.ListType=rvlstPicture) and LevelInfo.HasPicture and
                    not (LevelInfo.Picture.Graphic is TMetafile))
        else
          Result := False;
      end;
    else
      Result := inherited GetBoolValueEx(Prop,RVStyle);
  end;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetHeight: Integer;
begin
  Result := FHeight;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetWidth: Integer;
begin
  Result := FWidth;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetLeftOverhang: Integer;
begin
  Result := FOverhang;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetMinWidth(sad: PRVScreenAndDevice;
  Canvas: TCanvas; RVData: TPersistent): Integer;
var h,d,s,o,sb: Integer;
begin
  CalcSize(Canvas, RVData, Result, h, d, o, sad, True, s, sb);
  if not GetBoolValueEx(rvbpActualPrintSize, TCustomRVData(RVData).GetRVStyle) and
     (sad<>nil) then
    Result := MulDiv(Result, sad.ppixDevice, sad.ppixScreen);
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.OnDocWidthChange(DocWidth: Integer;
  dli: TRVDrawLineInfo; Printing: Boolean; Canvas: TCanvas;
  RVData: TPersistent; sad: PRVScreenAndDevice;
  var HShift, Desc: Integer; NoCaching, Reformatting: Boolean);
var Oh: Integer;
begin
  CalcSize(Canvas, RVData, dli.Width, dli.Height, Desc, Oh, sad, False, HShift, dli.SpaceBefore);
  if not Printing then begin
    FWidth := dli.Width;
    FHeight := dli.Height;
    FDescent  := Desc;
    FOverhang := Oh;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.CalcDisplayString(RVStyle: TRVStyle;
   List: TRVMarkerList; Index: Integer);
var LevelInfo: TRVListLevel;
  {.......................................................}
  function Number2Str(Value: Integer; ListType: TRVListType): String;
  begin
    case ListType of
      rvlstDecimal, rvlstImageListCounter:
        Result := IntToStr(Value);
      rvlstLowerAlpha:
        Result := LowerCase(RV_IntToAlpha(Value));
      rvlstUpperAlpha:
        Result := RV_IntToAlpha(Value);
      rvlstUpperRoman:
        Result := RV_IntToRoman(Value);
      rvlstLowerRoman:
        Result := LowerCase(RV_IntToRoman(Value));
      else
        Result := '';
    end;
  end;

  {.......................................................}
  function MultiLevelList : String;
  var CountersVal: array [0..255] of TVarRec;
      CountersStr: array [0..255] of String;
      ParentIndex, CurIndex, i: Integer;
      ALevelInfo: TRVListLevel;
      ParentLevelNo, CurLevelNo : Integer;
      ParentList, CurList: TRVMarkerList;
      Marker: TRVMarkerItemInfo;
      LegalStyle : Boolean;
      ListType : TRVListType;
  begin
    LegalStyle := rvloLegalStyleNumbering in LevelInfo.Options;

    CurLevelNo := Level;
    Marker  := Self;
    ALevelInfo := LevelInfo;
    CurIndex := Index;
    CurList  := List;

    while True do begin
      ListType := ALevelInfo.ListType;
      if (CurLevelNo<Level) and (ListType in [rvlstLowerRoman, rvlstUpperRoman, rvlstLowerAlpha, rvlstUpperAlpha]) and
         LegalStyle then
        ListType := rvlstDecimal;
      CountersStr[CurLevelNo]             := Number2Str(Marker.Counter, ListType);
      if CountersStr[CurLevelNo]<>'' then
        CountersVal[CurLevelNo].VAnsiString := PChar(CountersStr[CurLevelNo])
      else
        CountersVal[CurLevelNo].VAnsiString := nil;
      CountersVal[CurLevelNo].VType       := vtAnsiString;

      if CurLevelNo=0 then
        break;

      if CurList.FindParentMarker(CurIndex, nil, ParentList, ParentIndex) then begin
        Marker     := TRVMarkerItemInfo(ParentList.Items[ParentIndex]);
        ALevelInfo := Marker.GetLevelInfo(RVStyle);
        ParentLevelNo := Marker.Level;
        end
      else begin
        Marker     := nil;
        ALevelInfo := nil;
        ParentLevelNo := -1;
      end;
      for i := CurLevelNo-1 downto ParentLevelNo+1 do begin
        with GetLevelInfoEx(RVStyle,i) do
          CountersStr[i]  := Number2Str(StartFrom, ListType);
        if CountersStr[i]<>'' then
          CountersVal[i].VAnsiString := PChar(CountersStr[i])
        else
          CountersVal[i].VAnsiString := nil;
        CountersVal[i].VType       := vtAnsiString;
      end;
      if ParentLevelNo<0 then
        break;
      CurLevelNo := ParentLevelNo;
      CurIndex   := ParentIndex;
      CurList    := ParentList;
    end;
    Result := Format(LevelInfo.FormatString, CountersVal);
  end;
  {.......................................................}
begin
  LevelInfo := GetLevelInfo(RVStyle);
  if LevelInfo=nil then begin
    DisplayString := '';
    exit;
  end;
  case LevelInfo.ListType of
    rvlstBullet:
      try
        DisplayString := Format(LevelInfo.FormatString, ['','','','','','','','','','','','','','','','','','','','']);
      except
        DisplayString := LevelInfo.FormatString;
      end;
    {$IFNDEF RVDONOTUSEUNICODE}
    rvlstUnicodeBullet:
      DisplayString := ''; // RVU_UnicodeToAnsi(RVStyle.DefCodePage, PChar(Pointer(LevelInfo.FormatStringW)))
    {$ENDIF}
    rvlstPicture, rvlstImageList, rvlstImageListCounter:
      DisplayString := '';
    else
      DisplayString := MultiLevelList;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.Paint(x, y: Integer; Canvas: TCanvas;
  State: TRVItemDrawStates; Style: TRVStyle; dli: TRVDrawLineInfo);
begin
  DoPaint(x, y, Canvas, State, Style, dli, rvcmColor);
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.DoPaint(x, y: Integer; Canvas: TCanvas;
  State: TRVItemDrawStates; Style: TRVStyle; dli: TRVDrawLineInfo;
  ColorMode: TRVColorMode);
var LevelInfo: TRVListLevel;
    Index: Integer;
begin
  if (ListNo<0) or (Level<0) then
    exit;
  Canvas.Pen.Color := clRed;
  if dli<>nil then
    inc(x, dli.SpaceBefore);
  LevelInfo := GetLevelInfo(TRVStyle(Style));
  case LevelInfo.ListType of
    rvlstPicture:
      begin
        if LevelInfo.HasPicture then
          Canvas.Draw(X,Y, LevelInfo.Picture.Graphic);
      end;
    rvlstImageList, rvlstImageListCounter:
      begin
        Index := LevelInfo.ImageIndex;
        if LevelInfo.ListType = rvlstImageListCounter then
          inc(Index, Counter-1);
        if (LevelInfo.ImageList<>nil) and (Index>=0) and (Index<LevelInfo.ImageList.Count) then
          LevelInfo.ImageList.Draw(Canvas, X,Y, Index);
      end;
    {$IFNDEF RVDONOTUSEUNICODE}
    {$IFDEF RICHVIEWCBDEF3}
    rvlstUnicodeBullet:
      begin
        Canvas.Font := LevelInfo.Font;
        Canvas.Font.Color := RV_GetColor(Canvas.Font.Color, ColorMode);
        if (Style.TextStyles.PixelsPerInch<>0) and (LevelInfo.Font.Size>0) then
          Canvas.Font.Height := - MulDiv(LevelInfo.Font.Size, Style.TextStyles.PixelsPerInch, 72);
        {$IFNDEF RVDONOTUSECHARSPACING}
        SetTextCharacterExtra(Canvas.Handle, 0);
        {$ENDIF}
        SetTextAlign(Canvas.Handle, TA_LEFT);
        Canvas.Brush.Style := bsClear;
        TextOutW(Canvas.Handle, X,Y, Pointer(LevelInfo.FormatStringW), Length(LevelInfo.FormatStringW));
      end;
    {$ENDIF}
    {$ENDIF}
    else
      begin
        Canvas.Font := LevelInfo.Font;
        Canvas.Font.Color := RV_GetColor(Canvas.Font.Color, ColorMode);        
        if (Style.TextStyles.PixelsPerInch<>0) and (LevelInfo.Font.Size>0) then
          Canvas.Font.Height := - MulDiv(LevelInfo.Font.Size, Style.TextStyles.PixelsPerInch, 72);
        {$IFNDEF RVDONOTUSECHARSPACING}
        SetTextCharacterExtra(Canvas.Handle, 0);
        {$ENDIF}
        SetTextAlign(Canvas.Handle, TA_LEFT);
        Canvas.Brush.Style := bsClear;
        Canvas.TextOut(X,Y, DisplayString);
      end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.Print(Canvas: TCanvas; x, y, x2: Integer;
  Preview, Correction: Boolean; const sad: TRVScreenAndDevice;
  RichView: TRVScroller; dli: TRVDrawLineInfo; Part: Integer;
  ColorMode: TRVColorMode; RVData: TPersistent);
begin
  DoPaint(x, y, Canvas, [], TCustomRichView(RichView).Style, dli, ColorMode);
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.PrintToBitmap(Bkgnd: TBitmap; Preview: Boolean; RichView: TRVScroller;
  dli: TRVDrawLineInfo; Part: Integer; ColorMode: TRVColorMode):Boolean;
begin
  DoPaint(0, 0, Bkgnd.Canvas, [], TCustomRichView(RichView).Style, dli, ColorMode);
  Result := True;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetImageHeight(RVStyle: TRVStyle): Integer;
var LevelInfo: TRVListLevel;
begin
  Result := 0;
  LevelInfo := GetLevelInfo(RVStyle);
  case LevelInfo.ListType of
    rvlstImageList, rvlstImageListCounter:
      if LevelInfo.ImageList<>nil then
        Result := TImageList(LevelInfo.ImageList).Height;
    rvlstPicture:
      if LevelInfo.HasPicture then
        Result := LevelInfo.Picture.Graphic.Height;
  end;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetImageWidth(RVStyle: TRVStyle): Integer;
var LevelInfo: TRVListLevel;
begin
  Result := 0;
  LevelInfo := GetLevelInfo(RVStyle);
  case LevelInfo.ListType of

    rvlstImageList, rvlstImageListCounter:
      if LevelInfo.ImageList<>nil then
        Result := TImageList(LevelInfo.ImageList).Width;
        // Result := FWidth;
    rvlstPicture:
      if LevelInfo.HasPicture then
        Result := LevelInfo.Picture.Graphic.Width;
        //Result := FWidth;
  end;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSERTF}
procedure TRVMarkerItemInfo.FillRTFTables(ColorList: TRVColorList;
  ListOverrideCountList: TRVIntegerList; RVData: TPersistent);
var LevelInfo: TRVListLevel;
begin
  if not Reset or (ListNo<0) or (Level<0) then
    exit;
  LevelInfo := GetLevelInfo(TCustomRVData(RVData).GetRVStyle);
  if LevelInfo=nil then
    exit;
  if LevelInfo.HasNumbering and (LevelInfo.ListType<>rvlstImageListCounter) then
    ListOverrideCountList[ListNo] := ListOverrideCountList[ListNo]+1;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.SaveRTF(Stream: TStream; const Path: String;
  RVData: TPersistent; ItemNo: Integer; TwipsPerPixel: Double;
  Level: Integer; ColorList: TRVColorList;
  StyleToFont, ListOverrideOffsetsList1, ListOverrideOffsetsList2: TRVIntegerList;
  FontTable: TRVList);
var LevelInfo: TRVListLevel;
   ListOverrideNo: Integer;
begin
  LevelInfo := GetLevelInfo(TCustomRVData(RVData).GetRVStyle);
  if LevelInfo=nil then
    exit;
  case LevelInfo.ListType of
    rvlstPicture:
      RVSaveImageToRTF(Stream,TwipsPerPixel, LevelInfo.Picture.Graphic,
        0, 0, TCustomRVData(RVData).RTFOptions, nil);
    rvlstImageList:
      RVSaveImageListImageToRTF(Stream, TwipsPerPixel, LevelInfo.ImageList,
        LevelInfo.ImageIndex, TCustomRVData(RVData).RTFOptions);
    rvlstImageListCounter:
      RVSaveImageListImageToRTF(Stream, TwipsPerPixel, LevelInfo.ImageList,
        LevelInfo.ImageIndex+Counter-1, TCustomRVData(RVData).RTFOptions);
    else
      begin
        RVFWrite(Stream, '{\listtext\pard\plain');
        RVSaveFontToRTF(Stream, LevelInfo.Font, ColorList, TRVRTFFontTable(FontTable),
          TCustomRVData(RVData).GetRVStyle);
        RVFWrite(Stream, ' ');
        {$IFNDEF RVDONOTUSEUNICODE}
        {$IFDEF RICHVIEWCBDEF3}
        if LevelInfo.ListType=rvlstUnicodeBullet then
          RVFWrite(Stream, RVMakeRTFStrW(LevelInfo.FormatStringW,
            TCustomRVData(RVData).GetRVStyle.DefCodePage,
            rvrtfDuplicateUnicode in TCustomRVData(RVData).RTFOptions, False, False))
        else
        {$ENDIF}
        {$ENDIF}
        begin
          {$IFDEF RVUNICODESTR}
          RVFWrite(Stream, RVMakeRTFStrW(DisplayString,
            TCustomRVData(RVData).GetRVStyle.DefCodePage,
            rvrtfDuplicateUnicode in TCustomRVData(RVData).RTFOptions, False, False))
          {$ELSE}
          RVFWrite(Stream, RVMakeRTFStr(DisplayString, False, False))
          {$ENDIF}
        end;
        if Reset and LevelInfo.HasNumbering then begin
          ListOverrideOffsetsList1[ListNo] := ListOverrideOffsetsList1[ListNo]+1;
          ListOverrideNo := ListOverrideOffsetsList1[ListNo];
          end
        else
          ListOverrideNo := ListOverrideOffsetsList2[ListNo];
        RVFWrite(Stream, {$IFDEF RVUNICODESTR}AnsiStrings.{$ENDIF}Format('\tab}\ls%d\ilvl%d ', [ListOverrideNo, Self.Level]));
      end;
  end;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEHTML}
procedure TRVMarkerItemInfo.HTMLOpenOrCloseTags(Stream: TStream;
  OldLevelNo, NewLevelNo: Integer; RVStyle: TRVStyle; UseCSS: Boolean);
var i: Integer;
    LevelInfo: TRVListLevel;
begin
  for i := OldLevelNo downto NewLevelNo+1 do begin
    LevelInfo := GetLevelInfoEx(RVStyle,i);
    LevelInfo.HTMLCloseTag(Stream, UseCSS);
  end;
  for i := OldLevelNo+1 to NewLevelNo do begin
    LevelInfo := GetLevelInfoEx(RVStyle,i);
    LevelInfo.HTMLOpenTag(Stream, UseCSS);
  end;
  if OldLevelNo<>NewLevelNo then
    RVFWriteLine(Stream,'');
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.SaveHTMLSpecial(Stream: TStream;
  Prev: TRVMarkerItemInfo; RVStyle: TRVStyle; UseCSS: Boolean);
begin
  if Prev<>nil then
    if Prev.ListNo<>ListNo then begin
      Prev.HTMLOpenOrCloseTags(Stream, Prev.Level, -1, RVStyle, UseCSS);
      HTMLOpenOrCloseTags(Stream, -1, Level, RVStyle, UseCSS)
      end
    else
      Prev.HTMLOpenOrCloseTags(Stream, Prev.Level, Level, RVStyle, UseCSS)
  else
    HTMLOpenOrCloseTags(Stream, -1, Level, RVStyle, UseCSS);
end;
{------------------------------------------------------------------------------}
function GetWingdingsCode(ch: Char): Word;
begin
  case ord(ch) of
    $28: Result := 9742;
    $45: Result := 9756;
    $46: Result := 9758;
    $4A: Result := 9786;
    $52: Result := 9788;
    $5C: Result := 2384;
    $6B: Result := 38;
    $6C: Result := 9679;
    $6D: Result := 9675;
    $6E: Result := 9632;
    $6F..$72,$78..$7A,$7F,$A8,$FD..$FF: Result := 9633;
    $73..$74,$77: Result := 9830;
    $75..$76: Result := 9670;
    $80..$8A: Result := $30+ord(ch)-$80;
    $8B..$95: Result := $30+ord(ch)-$8B;
    $9E: Result := 183;
    $9F: Result := 8226;
    $A0,$A7: Result := 9642;
    $A1..$A3,$A6: Result := 9675;
    $A4..$A5,$B7..$C2: Result := 9678;
    $A9..$B6: Result := 9733;
    $F9..$FA: Result := 9643;
    $D5,$D7,$DB,$DF,$E7,$EF: Result := 9668;
    $D6,$D8,$DC,$E0,$E8,$F0: Result := 9658;
    $D9,$DD,$E1,$E9,$F1: Result := 9650;
    $DA,$DE,$E2,$EA,$F2: Result := 9660;
    else
      Result := 8226;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.SaveToHTML(Stream: TStream; RVData: TPersistent;
  ItemNo: Integer; const Text: TRVRawByteString; const Path,
  imgSavePrefix: String; var imgSaveNo: Integer;
  CurrentFileColor: TColor; SaveOptions: TRVSaveOptions;
  UseCSS: Boolean; Bullets: TRVList);
var LevelInfo: TRVListLevel;
    DoDefault : Boolean;
    Location: String;
    s: TRVRawByteString;
    ImageIndex, W,H, i: Integer;
    EncodeText: Boolean;
begin
  if not (rvsoMarkersAsText in SaveOptions) then
    exit;
  LevelInfo := GetLevelInfo(TCustomRVData(RVData).GetRVStyle);
  if LevelInfo=nil then
    exit;
  case LevelInfo.ListType of
    rvlstBullet, rvlstDecimal, rvlstLowerAlpha, rvlstUpperAlpha, rvlstLowerRoman,
    rvlstUpperRoman {$IFNDEF RVDONOTUSEUNICODE}, rvlstUnicodeBullet{$ENDIF}:
      begin
        EncodeText :=
        {$IFNDEF RVDONOTUSEUNICODE}
          (LevelInfo.ListType<>rvlstUnicodeBullet) and
        {$ENDIF}
          ((AnsiCompareText(LevelInfo.Font.Name,RVFONT_SYMBOL)=0) or
          (AnsiCompareText(LevelInfo.Font.Name,RVFONT_WINGDINGS)=0));
        if not UseCSS then begin
          s := StringToHTMLString2(
            RV_HTMLOpenFontTag2(LevelInfo.Font,
              TCustomRVData(RVData).GetRVStyle.TextStyles[0], True, SaveOptions),
            SaveOptions, CP_ACP);
          RVFWrite(Stream, s)
          end
        else begin
          s := StringToHTMLString2(RV_GetHTMLFontCSS(LevelInfo.Font, True),
            SaveOptions, CP_ACP);
          RVFWrite(Stream, {$IFDEF RVUNICODESTR}AnsiStrings.{$ENDIF}Format('<span style="%s', [s]));
          if LevelInfo.MarkerIndent>=LevelInfo.LeftIndent then
            RVFWrite(Stream, {$IFDEF RVUNICODESTR}AnsiStrings.{$ENDIF}Format(' width: %dpx;', // workaround for IE buggy rendering: not saving width for hanging indents
              [LevelInfo.LeftIndent+LevelInfo.FirstIndent-LevelInfo.MarkerIndent]));
          RVFWrite(Stream, '">');
        end;
        {$IFNDEF RVDONOTUSEUNICODE}
        if LevelInfo.ListType=rvlstUnicodeBullet then begin
          {$IFDEF RICHVIEWCBDEF3}
          RVFWrite(Stream, RVU_GetHTMLEncodedUnicode(RVU_GetRawUnicode(LevelInfo.FormatStringW), False))
          {$ENDIF}
          end
        else
        {$ENDIF}
        begin
          if EncodeText then begin
            if AnsiCompareText(LevelInfo.Font.Name,RVFONT_WINGDINGS)=0 then
              for i := 1 to Length(DisplayString) do
                RVFWrite(Stream, {$IFDEF RVUNICODESTR}AnsiStrings.{$ENDIF}Format('&#%d;', [GetWingdingsCode(DisplayString[i])]))
            else if AnsiCompareText(LevelInfo.Font.Name,RVFONT_SYMBOL)=0 then
              RVFWrite(Stream, RV_MakeHTMLSymbolStr(DisplayString));
            end
          else if rvsoUTF8 in SaveOptions then
            RVFWrite(Stream, StringToHTMLString2(DisplayString, SaveOptions,
              {$IFDEF RICHVIEWCBDEF3}
              RVU_Charset2CodePage(LevelInfo.Font.Charset)
              {$ELSE}
              CP_ACP
              {$ENDIF}));
        end;
        if not UseCSS then
          RVFWriteLine(Stream,RV_HTMLCloseFontTag2(LevelInfo.Font,
            TCustomRVData(RVData).GetRVStyle.TextStyles[0], True))
        else
          RVFWriteLine(Stream,'</span>');
      end;
    rvlstImageList,rvlstImageListCounter:
      begin
        TCustomRVData(RVData).HTMLSaveImage(TCustomRVData(RVData), ItemNo, Path, CurrentFileColor, Location, DoDefault);
        if DoDefault then begin
          ImageIndex := LevelInfo.ImageIndex;
          if LevelInfo.ListType=rvlstImageListCounter then
            inc(ImageIndex, Counter-1);
          if (ImageIndex>=0) and (ImageIndex<LevelInfo.ImageList.Count) and
             (LevelInfo.ImageList<>nil) then begin
            RVSaveImageSharedImageInHTML(LevelInfo.ImageList, ImageIndex, nil, Location, RVData, Path,
              imgSavePrefix, imgSaveNo, CurrentFileColor, SaveOptions, Bullets);
          end;
        end;
        if UseCSS and (LevelInfo.MarkerIndent>=LevelInfo.LeftIndent) then
          RVFWrite(Stream, {$IFDEF RVUNICODESTR}AnsiStrings.{$ENDIF}Format('<span style="width:%dpx">',
            [LevelInfo.LeftIndent+LevelInfo.FirstIndent-LevelInfo.MarkerIndent]));
        if Location<>'' then begin
          if LevelInfo.ImageList=nil then begin
            W := 0;
            H := 0;
            Exclude(SaveOptions, rvsoImageSizes);
            end
          else begin
            W := TImageList(LevelInfo.ImageList).Width;
            H := TImageList(LevelInfo.ImageList).Height;
          end;
          RVFWriteLine(Stream,
            {$IFDEF RVUNICODESTR}AnsiStrings.{$ENDIF}Format('<img%ssrc="%s"%s>',[RV_GetExtraIMGStr(SaveOptions, W, H, NoHTMLImageSize),
              Location, RV_HTMLGetEndingSlash(SaveOptions)]));
        end;
        if UseCSS and (LevelInfo.MarkerIndent>=LevelInfo.LeftIndent) then
          RVFWrite(Stream, {$IFDEF RVUNICODESTR}AnsiStrings.{$ENDIF}Format('</span>',
            [LevelInfo.LeftIndent+LevelInfo.FirstIndent-LevelInfo.MarkerIndent]));
      end;
    rvlstPicture:
      begin
        TCustomRVData(RVData).HTMLSaveImage(TCustomRVData(RVData), ItemNo, Path, CurrentFileColor, Location, DoDefault);
        if DoDefault and (LevelInfo.Picture.Graphic<>nil) then
          RVSaveImageSharedImageInHTML(nil, -1, LevelInfo.Picture.Graphic, Location, RVData, Path,
            imgSavePrefix, imgSaveNo, CurrentFileColor, SaveOptions, Bullets);
        if Location<>'' then begin
          if LevelInfo.Picture.Graphic=nil then begin
            W := 0;
            H := 0;
            Exclude(SaveOptions, rvsoImageSizes);
            end
          else begin
            W := LevelInfo.Picture.Graphic.Width;
            H := LevelInfo.Picture.Graphic.Height;
          end;
          RVFWriteLine(Stream,
            {$IFDEF RVUNICODESTR}AnsiStrings.{$ENDIF}Format('<img%ssrc="%s"%s>',
              [RV_GetExtraIMGStr(SaveOptions, W, H, NoHTMLImageSize),Location,
               RV_HTMLGetEndingSlash(SaveOptions)]));
        end;
      end;
  end
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetLICSS(RVData: TPersistent; ItemNo: Integer; const Path,
        imgSavePrefix: String; var imgSaveNo: Integer; CurrentFileColor: TColor;
        SaveOptions: TRVSaveOptions; Bullets: TRVList): String;
var LevelInfo: TRVListLevel;
    DoDefault : Boolean;
    Location: String;
    ImageIndex, DefIndent: Integer;
begin
  Result := '';
  LevelInfo := GetLevelInfo(TCustomRVData(RVData).GetRVStyle);
  case LevelInfo.ListType of
    rvlstImageList,rvlstImageListCounter:
      begin
        TCustomRVData(RVData).HTMLSaveImage(TCustomRVData(RVData), ItemNo, Path, CurrentFileColor, Location, DoDefault);
        if DoDefault then begin
          ImageIndex := LevelInfo.ImageIndex;
          if LevelInfo.ListType=rvlstImageListCounter then
            inc(ImageIndex, Counter-1);
          if (ImageIndex>=0) and (ImageIndex<LevelInfo.ImageList.Count) and
             (LevelInfo.ImageList<>nil) then begin
            RVSaveImageSharedImageInHTML(LevelInfo.ImageList, ImageIndex, nil, Location, RVData, Path,
              imgSavePrefix, imgSaveNo, CurrentFileColor, SaveOptions, Bullets);
          end;
        end;
        if Location<>'' then
          Result := Format('list-style-image: url(''%s'')',[Location]);
      end;
    rvlstPicture:
      begin
        TCustomRVData(RVData).HTMLSaveImage(TCustomRVData(RVData), ItemNo, Path, CurrentFileColor, Location, DoDefault);
        if DoDefault and (LevelInfo.Picture.Graphic<>nil) then
            RVSaveImageSharedImageInHTML(nil, -1, LevelInfo.Picture.Graphic, Location, RVData, Path,
              imgSavePrefix, imgSaveNo, CurrentFileColor, SaveOptions, Bullets);
        if Location<>'' then
          Result := Format('list-style-image: url(''%s'')',[Location]);
      end;
  end;
  if TCustomRVData(RVData).GetRVStyle.ParaStyles[ParaNo].LeftIndent<>0 then
    RV_AddStrEx(Result, 'margin-left: 0', '; ');
  if LevelInfo.MarkerIndent>=LevelInfo.LeftIndent then
    DefIndent := LevelInfo.MarkerIndent-LevelInfo.LeftIndent
  else
    DefIndent := LevelInfo.FirstIndent;
  if TCustomRVData(RVData).GetRVStyle.ParaStyles[ParaNo].FirstIndent<>DefIndent then
    RV_AddStrEx(Result, Format('text-indent: %dpx', [DefIndent]), '; ');
  if Result<>'' then
    Result := Format(' style="%s"', [Result]);
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.MarkStylesInUse(Data: TRVDeleteUnusedStylesData);
begin
  inherited;
  if ListNo>=0 then
    Data.UsedListStyles[ListNo] := 1;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.UpdateStyles(Data: TRVDeleteUnusedStylesData);
begin
  inherited;
  if ListNo>=0 then
    dec(ListNo, Data.UsedListStyles[ListNo]-1);
end;
{------------------------------------------------------------------------------}
function ProcessSymbolString(const s: String): String;
var i: Integer;
  ch: Char;
begin
  Result := s;
  for i := 1 to Length(s) do begin
    ch := Result[i];
    if ((ch>=#$A7) and (ch<=#$AA)) or (ch=#$B7) or (ch=#$D7) or (ch=#$E0) then
      Result[i] := '*';
  end;
  RV_ReplaceStr(Result,#$DB,'<=>');
  RV_ReplaceStr(Result,#$DC,'<=');
  RV_ReplaceStr(Result,#$DE,'=>');
  RV_ReplaceStr(Result,#$AB,'<->');
  RV_ReplaceStr(Result,#$AC,'<-');
  RV_ReplaceStr(Result,#$AE,'->');
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.AsText(LineWidth: Integer; RVData: TPersistent;
  const Text: TRVRawByteString; const Path: String;
  TextOnly, Unicode: Boolean): TRVRawByteString;
var LevelInfo: TRVListLevel;
  {$IFNDEF RVDONOTUSEUNICODE}
    i: Integer;
  {$ENDIF}
  s: String;
begin
  Result := '';
  LevelInfo := GetLevelInfo(TCustomRVData(RVData).GetRVStyle);
  case LevelInfo.ListType of
    rvlstBullet,rvlstDecimal,rvlstLowerAlpha,rvlstUpperAlpha,
    rvlstLowerRoman,rvlstUpperRoman:
      begin
        if (AnsiCompareText(LevelInfo.Font.Name,RVFONT_SYMBOL)=0) then
          s := ProcessSymbolString(DisplayString)
        else
          s := DisplayString;
        s := s+' ';
        {$IFDEF RVUNICODESTR}
        Result := RVU_GetRawUnicode(s);
        if not Unicode then begin
          Result := RVU_UnicodeToAnsi(CP_ACP, Result);
          for i := 1 to Length(Result) do begin
            if i>Length(s) then
              break;
            if (Result[i]='?') and (s[i]<>'?') then
              Result[i] := '*';
          end;
        end;
        {$ELSE}
        if Unicode then
          Result := RVU_AnsiToUnicode(
            {$IFDEF RICHVIEWCBDEF3}
            RVU_Charset2CodePage(LevelInfo.Font.Charset)
            {$ELSE}
            CP_ACP
            {$ENDIF}, s);
        {$ENDIF}
      end;
    rvlstPicture,rvlstImageList,rvlstImageListCounter:
      begin
        Result := '* ';
        if Unicode then
          Result := RVU_AnsiToUnicode(CP_ACP, Result)
      end;
    {$IFNDEF RVDONOTUSEUNICODE}
    {$IFDEF RICHVIEWCBDEF3}
    rvlstUnicodeBullet:
      begin
        Result := RVU_GetRawUnicode(LevelInfo.FormatStringW+' ');
        if not Unicode then begin
          Result := RVU_UnicodeToAnsi(CP_ACP, Result);
          for i := 1 to Length(Result) do begin
            if i>Length(LevelInfo.FormatStringW) then
              break;
            if (Result[i]='?') and (LevelInfo.FormatStringW[i]<>'?') then
              Result[i] := '*';
          end;
        end;
      end;
    {$ENDIF}
    {$ENDIF}
  end;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetIndexInList(List: TList): Integer;
begin
  if List=nil then begin
    Result := -1;
    exit;
  end;
  if (FCachedIndexInList<0) or (FCachedIndexInList>=List.Count) or
     (List.Items[FCachedIndexInList]<>Self) then
    FCachedIndexInList := List.IndexOf(Self);
  Result := FCachedIndexInList;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetRVFExtraPropertyCount: Integer;
begin
  Result := inherited GetRVFExtraPropertyCount;
  if NoHTMLImageSize then
    inc(Result);
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.SaveRVFExtraProperties(Stream: TStream);
begin
  inherited SaveRVFExtraProperties(Stream);
  if NoHTMLImageSize then
    WriteRVFExtraIntPropertyStr(Stream, rvepNoHTMLImageSize, 1);
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetExtraIntProperty(Prop: TRVExtraItemProperty;
  var Value: Integer): Boolean;
begin
  case Prop of
    rvepNoHTMLImageSize:
      begin
        Value := ord(NoHTMLImageSize);
        Result := True;
      end;
    else
      Result := inherited GetExtraIntProperty(Prop, Value);
  end;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.SetExtraIntProperty(Prop: TRVExtraItemProperty;
  Value: Integer): Boolean;
begin
  case Prop of
    rvepNoHTMLImageSize:
      begin
        NoHTMLImageSize := Value<>0;
        Result := True;
      end;
    else
      Result := inherited SetExtraIntProperty(Prop, Value);
  end;
end;
{=============================== TRVMarkerList ================================}
function TRVMarkerList.FindParentMarker(Index: Integer;
  Marker: TRVMarkerItemInfo; var ParentList: TRVMarkerList;
  var ParentIndex: Integer): Boolean;
var Marker2: TRVMarkerItemInfo;
    i: Integer;
begin
  if Marker=nil then
    Marker := TRVMarkerItemInfo(Items[Index]);
  for i := Index-1 downto 0 do begin
    Marker2 := TRVMarkerItemInfo(Items[i]);
    if (Marker2.ListNo = Marker.ListNo) and
       (Marker2.Level<Marker.Level) then begin
      ParentIndex := i;
      ParentList  := Self;
      Result := True;
      exit;
    end;
  end;
  if PrevMarkerList<>nil then
    Result := PrevMarkerList.FindParentMarker(PrevMarkerList.Count, Marker,
      ParentList, ParentIndex)
  else begin
    Result := False;
    ParentIndex := -1;
    ParentList  := nil;
  end;
end;
{------------------------------------------------------------------------------}
function TRVMarkerList.InsertAfter(InsertMe, AfterMe: TRVMarkerItemInfo): Integer;
begin
  if AfterMe = nil then
    Result := 0
  else
    Result := AfterMe.GetIndexInList(Self)+1;
  Insert(Result, InsertMe);
  InsertMe.FCachedIndexInList := Result;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerList.RecalcCounters(StartFrom: Integer; RVStyle: TRVStyle);
var i,j, ListNo: Integer;
    LevelReset: Boolean;
    Marker, Markerj: TRVMarkerItemInfo;
    LevelInfo: TRVListLevel;
begin
  if StartFrom>=Count then
    exit;
  ListNo := TRVMarkerItemInfo(Items[StartFrom]).ListNo;
  if ListNo<0 then
    exit;
  for i := StartFrom to Count-1 do begin
    Marker := TRVMarkerItemInfo(Items[i]);
    if Marker.ListNo=ListNo then begin
      if Marker.Reset then
        Marker.Counter := Marker.StartFrom
      else begin
        LevelInfo := TRVMarkerItemInfo(Items[i]).GetLevelInfo(RVStyle);
        if LevelInfo<>nil then begin
          Marker.Counter := LevelInfo.StartFrom;
          LevelReset := rvloLevelReset in LevelInfo.Options;
          for j := i-1 downto 0 do begin
            Markerj := TRVMarkerItemInfo(Items[j]);
            if Markerj.ListNo=ListNo then
              if Markerj.Level=Marker.Level then begin
                Marker.Counter := Markerj.Counter+1;
                break;
                end
              else if LevelReset and (Markerj.Level<Marker.Level) then begin
                Marker.Counter := LevelInfo.StartFrom;
                break;
                end
              else if (Markerj.Level>Marker.Level) then
                Marker.Counter := LevelInfo.StartFrom+1;
          end;
          end
        else
          Marker.Counter := 0;
      end;
      Marker.CalcDisplayString(RVStyle, Self, i);
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerList.RecalcDisplayStrings(RVStyle: TRVStyle);
var i: Integer;
begin
  for i := 0 to Count-1 do
    TRVMarkerItemInfo(Items[i]).CalcDisplayString(RVStyle, Self, i);
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerList.LoadFromStream(Stream: TStream; RVData: TPersistent;
 IncludeSize: Boolean);
var i,c: Integer;
    ListNo, Level: Integer;
    Counter: Integer;
    Reset: Boolean;
    StartFrom: Integer;
    Marker: TRVMarkerItemInfo;
begin
  Clear;
  if IncludeSize then begin
    Stream.ReadBuffer(c, sizeof(c));
  end;
  Stream.ReadBuffer(c, sizeof(c));
  Stream.ReadBuffer(c, sizeof(c));
  Capacity := c;
  for i := 0 to c-1 do begin
    Stream.ReadBuffer(ListNo, sizeof(ListNo));
    Stream.ReadBuffer(Level, sizeof(Level));
    Stream.ReadBuffer(Reset, sizeof(Reset));
    Stream.ReadBuffer(StartFrom, sizeof(StartFrom));
    Stream.ReadBuffer(Counter, sizeof(Counter));
    Marker := TRVMarkerItemInfo.CreateEx(RVData, ListNo, Level, StartFrom, Reset);
    Marker.Counter := Counter;
    Add(Marker);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerList.SaveToStream(Stream: TStream; Count: Integer;
 IncludeSize: Boolean);
var i: Integer;
begin
  if IncludeSize then begin
    i := sizeof(Integer)*(1+1+Count*4)+sizeof(Boolean)*Count;
    Stream.WriteBuffer(i, sizeof(i));
  end;
  i := 0;
  Stream.WriteBuffer(i, sizeof(i));
  Stream.WriteBuffer(Count, sizeof(Count));
  for i := 0 to Count-1 do
    with TRVMarkerItemInfo(Items[i]) do begin
      Stream.WriteBuffer(ListNo, sizeof(ListNo));
      Stream.WriteBuffer(Level, sizeof(Level));
      Stream.WriteBuffer(Reset, sizeof(Reset));
      Stream.WriteBuffer(StartFrom, sizeof(StartFrom));
      Stream.WriteBuffer(Counter, sizeof(Counter));
    end;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerList.LoadBinary(const s: TRVRawByteString; RVData: TPersistent);
var TmpStream: TMemoryStream;
begin
   TmpStream := TMemoryStream.Create;
   try
     TmpStream.WriteBuffer(PRVAnsiChar(s)^, Length(s));
     TmpStream.Position := 0;
     LoadFromStream(TmpStream, RVData, False);
   finally
     TmpStream.Free;
   end;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerList.LoadText(const s: TRVAnsiString; RVData: TPersistent);
var TmpStream: TRVMemoryStream;
begin
   TmpStream := TRVMemoryStream.Create;
   try
     RVFTextString2Stream(s, TmpStream);
     TmpStream.Position := 0;
     LoadFromStream(TmpStream, RVData, False);
   finally
     TmpStream.Free;
   end;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerList.SaveTextToStream(Stream: TStream; Count: Integer);
var TmpStream: TRVMemoryStream;
    s: TRVAnsiString;
begin
   TmpStream := TRVMemoryStream.Create;
   try
     SaveToStream(TmpStream, Count, False);
     TmpStream.Position := 0;
     s := RVFStream2TextString(TmpStream);
     RVFWriteLine(Stream, s);
   finally
     TmpStream.Free;
   end;
end;
{$ENDIF}


end.
