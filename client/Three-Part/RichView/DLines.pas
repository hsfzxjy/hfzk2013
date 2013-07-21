
{*******************************************************}
{                                                       }
{       RichView                                        }
{       TRVDrawLineInfo: stores formatting for          }
{       one item in RichView document.                  }
{       TRVDrawLines: a list of TRVDrawLineInfo.        }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit DLines;

interface
{$I RV_Defs.inc}
uses Classes, RVStyle, RVTypes;
type
{-----------------------------------------------------------------------}
{
  TRVDrawLineInfo stores formatting for one item in RichView document.
  These classes are often referred in other places as "draw items".
  One item can corresponds to several draw items:
  - each line of wrapped text item corresponds to one draw item.
  This class is actually used for the most of items. But in some cases
  (printing item on several pages) inherited classes are used instead.
}
  TRVDrawLineInfo = class
    public
     // space before the item, pixels (used for justified text and paragraph lists)
     {$IFNDEF RVDONOTUSEJUSTIFY}
     SpaceBefore: Integer;
     {$ELSE}
     {$IFNDEF RVDONOTUSELISTS}
     SpaceBefore: Integer;
     {$ENDIF}
     {$ENDIF}
     {$IFDEF RVUSEBASELINE}
     BaseLine: Integer;
     {$ENDIF}
     Left, Top, Width, Height: Integer; // coordinates, relative to the top left of document
     ItemNo, Offs, Length: Integer; // links to items; length is used for text drawitems
     ExtraSpaceBelow: Integer;      // used for line spacing
     ExtraSpaceAbove: Integer;      // used for line spacing
     FromNewLine: ByteBool;         // true, if draw item starts a screen line
     constructor CreateEx(ALeft, ATop, AWidth, AHeight, AItemNo: Integer;
       AFromNewLine: ByteBool);
     procedure SetData(ALeft, ATop, AItemNo: Integer;
                       AFromNewLine: ByteBool); virtual;
     procedure SetSize(AWidth, AHeight: Integer);virtual;
     function InitSplit(const SaD: TRVScreenAndDevice): Boolean; dynamic;
     function CanSplitFirst(Y: Integer; const SaD: TRVScreenAndDevice;
       FirstOnPage, PageHasFootnotes, FootnotesChangeHeight: Boolean): Boolean; dynamic;
     function SplitAt(Y: Integer; const SaD: TRVScreenAndDevice;
       FirstOnPage: Boolean; var FootnoteRVDataList: TList;
       var MaxHeight: Integer; FootnotesChangeHeight: Boolean): Boolean; dynamic;
  end;
  TRVDrawLineInfoClass = class of TRVDrawLineInfo;
{-----------------------------------------------------------------------}
{
  TRVDrawLines - a list of TRVDrawLineInfo
}
  TRVDrawLines = class (TList)
    private
      FStartDeletedIndex, FDeletedCount: Integer;
      function Get(Index: Integer): TRVDrawLineInfo;
      procedure Put(Index: Integer; const Value: TRVDrawLineInfo);
      procedure DeleteRange(Index1, Index2: Integer);      
    public
      constructor Create;
      procedure MarkForDelete(Index1, Index2: Integer);
      procedure DeleteMarked;
      procedure Insert(Index: Integer; Item: Pointer);
      procedure Delete(Index: Integer);
      function GetString(Index: Integer; AItems: TList): TRVRawByteString;
      function GetSubString(Index: Integer; AItems: TList;
        AStartIndex, ALength: Integer): TRVRawByteString;
      function GetRightString(Index: Integer; AItems: TList;
        AStartIndex: Integer): TRVRawByteString;
      property Items[Index: Integer]: TRVDrawLineInfo read Get write Put; default;
  end;

implementation
uses RVUni, RVItem;
{=============================== TRVDrawLineInfo ==============================}
{ Constructor. Assigns the main properties                                     }
constructor TRVDrawLineInfo.CreateEx(ALeft, ATop, AWidth, AHeight,
  AItemNo: Integer; AFromNewLine: ByteBool);
begin
  inherited Create;
  SetData(ALeft, ATop, AItemNo, AFromNewLine);
  SetSize(AWidth, AHeight);
end;
{------------------------------------------------------------------------------}
{ Assigning the main properties                                                }
procedure TRVDrawLineInfo.SetData(ALeft, ATop, AItemNo: Integer; AFromNewLine: ByteBool);
begin
  Left   := ALeft;
  Top    := ATop;
  ItemNo := AItemNo;
  FromNewLine := AFromNewLine;
end;
{------------------------------------------------------------------------------}
{ Assigning width and height                                                   }
procedure TRVDrawLineInfo.SetSize(AWidth, AHeight: Integer);
begin
  Width  := AWidth;
  Height := AHeight;
end;
{------------------------------------------------------------------------------}
{ CanSplitFirst, InitSplit, SplitAt are used for printing items across pages.
  Actual implementations are in inherited classes.
  CanSplitFirst: can item be split at position Y (relative to the top of item) }
function TRVDrawLineInfo.CanSplitFirst(Y: Integer;
  const Sad: TRVScreenAndDevice; FirstOnPage, PageHasFootnotes,
  FootnotesChangeHeight: Boolean): Boolean;
begin
  Result := False;
end;
{------------------------------------------------------------------------------}
{ Initialize splitting. Return true if successful                              }
function TRVDrawLineInfo.InitSplit(const Sad: TRVScreenAndDevice): Boolean;
begin
  Result := False;
end;
{------------------------------------------------------------------------------}
{ Split at the specified position (Y is relative to the top of item)           }
function TRVDrawLineInfo.SplitAt(Y: Integer; const Sad: TRVScreenAndDevice;
  FirstOnPage: Boolean; var FootnoteRVDataList: TList;
  var MaxHeight: Integer; FootnotesChangeHeight: Boolean): Boolean;
begin
  Result := False;
end;
{================================ TRVDrawLines ================================}
{ Constructor                                                                  }
constructor TRVDrawLines.Create;
begin
  inherited Create;
  FStartDeletedIndex := -1;
  FDeletedCount      := 0;
end;
{------------------------------------------------------------------------------}
{ This method is used in editor for reformatting a part of document.
  It's more efficient to mark a range of items for deletion (freeing the
  corresponding objects) and reuse list items, than to perform actual deletion
  with subsequent insertion }
procedure TRVDrawLines.MarkForDelete(Index1, Index2: Integer);
var i: Integer;
begin
  FStartDeletedIndex := Index1;
  FDeletedCount      := Index2-Index1+1;
  for i := Index1 to Index2 do
    TObject(Items[i]).Free;
end;
{------------------------------------------------------------------------------}
{ Deletes items marked by MarkForDelete                                        }
procedure TRVDrawLines.DeleteMarked;
begin
  if FDeletedCount<>0 then begin
    DeleteRange(FStartDeletedIndex, FStartDeletedIndex+FDeletedCount-1);
    FDeletedCount := 0;
  end;
end;
{------------------------------------------------------------------------------}
{ Inserts a new item at the position Index. Tries to reuse items marked by
  MarkForDelete                                                                }
procedure TRVDrawLines.Insert(Index: Integer; Item: Pointer);
begin
  if FDeletedCount=0 then
    inherited Insert(Index, Item)
  else begin
    //Assert(Index=FStartDeletedIndex);
    inc(FStartDeletedIndex);
    dec(FDeletedCount);
    Items[Index] := Item;
  end;
end;
{------------------------------------------------------------------------------}
{ Deletes the item at the position Index. Takes marking for deletion into
  account                                                                      }
procedure TRVDrawLines.Delete(Index: Integer);
begin
  Items[Index].Free;
  if FDeletedCount=0 then
    inherited Delete(Index)
  else begin
    //Assert(Index=FStartDeletedIndex);
    dec(FStartDeletedIndex);
    inc(FDeletedCount);
  end;
end;
{------------------------------------------------------------------------------}
{ Internal method. Deletes a range of items from Index1 to Index2.
  Assumes that corresponding objects are alredy freed                          }
procedure TRVDrawLines.DeleteRange(Index1, Index2: Integer);
begin
  if Index2 < Count-1 then
    System.Move(List^[Index2 + 1], List^[Index1],
      (Count - Index2 -1) * SizeOf(Pointer));
  Count := Count - (Index2-Index1+1);
end;
{------------------------------------------------------------------------------}
{ Method for accessing property Items[Index]                                   }
function TRVDrawLines.Get(Index: Integer): TRVDrawLineInfo;
begin
  Result := TRVDrawLineInfo(inherited Get(Index));
end;
{------------------------------------------------------------------------------}
{ Method for assigning property Items[Index]                                   }
procedure TRVDrawLines.Put(Index: Integer; const Value: TRVDrawLineInfo);
begin
  inherited Put(Index, Value);
end;
{------------------------------------------------------------------------------}
{ Returns a string corresponding to the Index-th draw item.
  String is taken from the list of items (AItems).
  If one item corresponds to one draw item, this is a AItems[drawitem.ItemNo].
  If not (because of text wrapping), this is a substring of it (starting from
  the drawitem.Offs, having length drawitem.Length
  In case of Unicode, returns "raw unicode" string                             }
function TRVDrawLines.GetString(Index: Integer; AItems: TList): TRVRawByteString;
begin
 {$IFDEF RVDONOTUSEUNICODE}
  with Items[Index] do
    Result := Copy(TRVItemList(AItems).Items[ItemNo], Offs, Length);
  {$ELSE}
  with Items[Index] do
    Result := RVU_Copy(TRVItemList(AItems).Items[ItemNo], Offs, Length,
      TCustomRVItemInfo(TRVItemList(AItems).Objects[ItemNo]).ItemOptions);
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
{ The same, but returns substring of drawitem's text, starting at AStartIndex
  position (1-based).                                                          }
function TRVDrawLines.GetRightString(Index: Integer; AItems: TList;
  AStartIndex: Integer): TRVRawByteString;
begin
  {$IFDEF RVDONOTUSEUNICODE}
  with Items[Index] do
    Result := Copy(TRVItemList(AItems).Items[ItemNo], Offs+AStartIndex-1, Length-AStartIndex+1);
  {$ELSE}
  with Items[Index] do
    Result := RVU_Copy(TRVItemList(AItems).Items[ItemNo], Offs+AStartIndex-1, Length-AStartIndex+1,
      TCustomRVItemInfo(TRVItemList(AItems).Objects[ItemNo]).ItemOptions);
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
{ The same, but returns substring of drawitem's text, starting at AStartIndex
  position (1-based) and having length ALength                                 }
function TRVDrawLines.GetSubString(Index: Integer; AItems: TList;
  AStartIndex, ALength: Integer): TRVRawByteString;
begin
  with Items[Index] do begin
    if AStartIndex+ALength>Length+1 then
      ALength := Length-AStartIndex+1;
    {$IFDEF RVDONOTUSEUNICODE}
    Result := Copy(TRVItemList(AItems).Items[ItemNo], Offs+AStartIndex-1, ALength);
    {$ELSE}
    Result := RVU_Copy(TRVItemList(AItems).Items[ItemNo], Offs+AStartIndex-1, ALength,
      TCustomRVItemInfo(TRVItemList(AItems).Objects[ItemNo]).ItemOptions);
    {$ENDIF}
  end;
end;

end.
