{*******************************************************}
{                                                       }
{       RichView                                        }
{       TRVJvGIFImageAnimator: displaying animation for }
{       TJvGIFImage from Project JEDI's JVCL.           }
{       http://jvcl.sourceforge.net                     }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

{$I RV_Defs.inc}

unit RVJvGifAnimate;

interface

{$IFNDEF RVDONOTUSEANIMATION}

uses Windows, Classes, Graphics, DLines,
     CRVFData, RVAnimate, RVItem, JvGif;

type
  { ---------------------------------------------------------------------------
    TRVJvGifImageAnimator: displaying animations for gif images
    (using TJvGIFImage from Project JEDI's JVCL)
    Variables:
      bmp: the current frame of animation
      bmpsrc: background under the image (if it is not a plain color, otherwise
        it's nil)
      FX, FY - the stored values of image position. Used to check if the image
        is moved.
      FParaNo - the stored value of paragraph image. Used to check if the
        paragraph is changed (may be it's background was changed?)
      FLastDrawnFrameIndex - index of the frame drawn in bmp.
  }
  TRVJvGifImageAnimator = class (TRVAnimator)
    private
      bmp, bmpsrc: TBitmap;
      FBackColor: TColor;
      FX,FY, FParaNo, FLastDrawnFrameIndex: Integer;
      procedure CalcInterval;
    protected
      function GetFrameCount: Integer; override;
      procedure ResetBackground; override;
    public
      destructor Destroy; override;
      procedure Reset; override;
      procedure ChangeFrame; override;
      procedure Draw(X, Y: Integer; Canvas: TCanvas; Animation: Boolean); override;
      function GetExportImageSize: TSize; override;
      procedure DrawForExport(Canvas: TCanvas); override;      
  end;

{$ENDIF}

implementation

{$IFNDEF RVDONOTUSEANIMATION}
{================================= TRVJvGifImageAnimator ========================}
{ Destructor }
destructor TRVJvGifImageAnimator.Destroy;
begin
  bmp.Free;
  bmpsrc.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
{ Determines how long to display the current frame. }
procedure TRVJvGifImageAnimator.CalcInterval;
var gif: TJvGIFImage;
begin
  gif := TJvGIFImage(TRVGraphicItemInfo(item).Image);
  Interval := 100;
  if gif.Frames[FrameIndex].AnimateInterval > 0 then begin
    Interval := gif.Frames[FrameIndex].AnimateInterval;
  end;
end;
{------------------------------------------------------------------------------}
{ Change frame to the next one. Updates Interval. }
procedure TRVJvGifImageAnimator.ChangeFrame;
begin
  CalcNextFrameIndex;
  CalcInterval;
end;
{------------------------------------------------------------------------------}
{ Clears the stored background info }
procedure TRVJvGifImageAnimator.ResetBackground;
begin
  bmpsrc.Free;
  bmpsrc := nil;
  bmp.Free;
  bmp := nil;
end;
{------------------------------------------------------------------------------}
{ Draws the current frame }
procedure TRVJvGifImageAnimator.Draw(X, Y: Integer; Canvas: TCanvas; Animation: Boolean);
var gif: TJvGIFImage;
    i: Integer;
    UseSrcBitmap: Boolean;
    r: TRect;

    function ScaleRect(const DestRect: TRect; FrameIndex: Integer): TRect;
    var
      HeightMul  ,
      HeightDiv  : integer;
      WidthMul  ,
      WidthDiv  : integer;
    begin
      with gif.Frames[FrameIndex] do begin
        HeightDiv := gif.Height;
        HeightMul := DestRect.Bottom-DestRect.Top;
        WidthDiv := gif.Width;
        WidthMul := DestRect.Right-DestRect.Left;

        Result.Left := DestRect.Left + muldiv(Origin.X, WidthMul, WidthDiv);
        Result.Top := DestRect.Top + muldiv(Origin.Y, HeightMul, HeightDiv);
        Result.Right := DestRect.Left + muldiv(Origin.X+Width, WidthMul, WidthDiv);
        Result.Bottom := DestRect.Top + muldiv(Origin.Y+Height, HeightMul, HeightDiv);
      end;
    end;

    procedure MakeBitmap(FrameIndex: Integer);
    var r: TRect;
    begin
      if FrameIndex>0 then
        case gif.Frames[FrameIndex-1].DisposalMethod of
          dmRestoreBackground:
            begin
              r := Rect(0,0,bmp.Width,bmp.Height);
              if bmpsrc<>nil then
                bmp.Canvas.CopyRect(r, bmpsrc.Canvas, r)
              else begin
                bmp.Canvas.Brush.Color := FBackColor;
                bmp.Canvas.FillRect(r);
              end;
            end;
        end
      else begin
        if bmpsrc<>nil then
          bmp.Assign(bmpsrc)
        else begin
          bmp.Canvas.Brush.Color := FBackColor;
          bmp.Canvas.FillRect(Rect(0,0,bmp.Width,bmp.Height));
        end;
      end;
      r := ScaleRect(Rect(0, 0, item.GetImageWidth(nil), item.GetImageHeight(nil)), FrameIndex);
      gif.Frames[FrameIndex].Draw(bmp.Canvas, r, gif.Frames[FrameIndex].TransparentColor <> clNone);
    end;

begin
  gif := TJvGIFImage(TRVGraphicItemInfo(item).Image);
  if (bmp=nil) or
    (item.ParaNo<>FParaNo) or
    (X<>FX) or
    (Y<>FY) or
    (bmp.Width<>item.GetImageWidth(nil)) or
    (bmp.Height<>item.GetImageHeight(nil)) then begin
    bmp.Free;
    bmp := TBitmap.Create;
    bmp.Width := item.GetImageWidth(nil);
    bmp.Height := item.GetImageHeight(nil);
    FParaNo := item.ParaNo;
    FX := X;
    FY := Y;
    if gif.Transparent then begin
      r := Rect(0,0,0,0);
      RVData.GetItemBackground(RVData.DrawItems[item.DrawItemNo].ItemNo, r, True,
        FBackColor, bmpsrc, UseSrcBitmap);
      if not UseSrcBitmap then begin
        bmp.Canvas.Brush.Color := RVData.GetColor;
        bmp.Canvas.FillRect(Rect(0,0,bmp.Width,bmp.Height));
      end;
      end
    else begin
      FBackColor := clWhite;
      UseSrcBitmap := False;
    end;
    if not UseSrcBitmap then begin
      bmpsrc.Free;
      bmpsrc := nil;
    end;
    for i := 0 to FrameIndex-1 do
      MakeBitmap(i);
    end
  else if (FrameIndex=FLastDrawnFrameIndex) then begin
    Canvas.Draw(X,Y,bmp);
    exit;
    end
  else if (FrameIndex>0) and (FLastDrawnFrameIndex<>FrameIndex-1) then begin
    if FLastDrawnFrameIndex<FrameIndex then begin
      for i := FLastDrawnFrameIndex+1 to FrameIndex-1 do
        MakeBitmap(i);
      end
    else
      for i := 0 to FrameIndex-1 do
        MakeBitmap(i);
  end;
  MakeBitmap(FrameIndex);
  FLastDrawnFrameIndex := FrameIndex;
  Canvas.Draw(X,Y,bmp);
end;
{------------------------------------------------------------------------------}
{ Draws the first frame for RTF export }
procedure TRVJvGifImageAnimator.DrawForExport(Canvas: TCanvas);
var gif: TJvGIFImage;
    r: TRect;
begin
  gif := TJvGIFImage(TRVGraphicItemInfo(item).Image);
  Canvas.Brush.Color := clWhite;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(Rect(0, 0, gif.Width, gif.Height));
  with gif.Frames[0] do
    r := Bounds(Origin.X, Origin.Y, Width, Height);
  gif.Frames[0].Draw(Canvas, r, gif.Frames[FrameIndex].TransparentColor <> clNone);
end;
{------------------------------------------------------------------------------}
{ Image size for RTF saving }
function TRVJvGifImageAnimator.GetExportImageSize: TSize;
begin
  Result.cy := TJvGIFImage(TRVGraphicItemInfo(item).Image).Height;
  Result.cx := TJvGIFImage(TRVGraphicItemInfo(item).Image).Width;
end;
{------------------------------------------------------------------------------}
{ Returns a number of frames in gif }
function GetGifFrameCount(gif: TJvGIFImage): Integer;
begin
  Result := gif.Count;
end;
{------------------------------------------------------------------------------}
{ Returns a number of frames }
function TRVJvGifImageAnimator.GetFrameCount: Integer;
begin
  Result := TJvGIFImage(TRVGraphicItemInfo(item).Image).Count;
end;
{------------------------------------------------------------------------------}
{ Rewinds to the first frame. Updates Interval. }
procedure TRVJvGifImageAnimator.Reset;
begin
  bmp.Free;
  bmp := nil;
  bmpsrc.Free;
  bmpsrc := nil;
  FrameIndex := 0;
  FLastDrawnFrameIndex := -1;
  CalcInterval;
end;
{==============================================================================}
var DefMakeAnimator: TRVMakeAnimatorProc;
{ This procedure creates an animator (anim) for the item, if it's necessary.
  This procedure can create only TRVJvGifImageAnimator.
  If it cannot be applied, it calls the stored value of RV_MakeAnimator. }
procedure RV_MakeAnimatorGif(item: TCustomRVItemInfo; RVData: TCustomRVFormattedData;
  var anim: TRVAnimator);
begin
  if (item is TRVGraphicItemInfo) and
     (TRVGraphicItemInfo(item).Image is TJvGifImage) and
     (GetGifFrameCount(TJvGIFImage(TRVGraphicItemInfo(item).Image))>1) then begin
    if (anim<>nil) and not (anim is TRVJvGifImageAnimator) then begin
      anim.Free;
      anim := nil;
    end;
    if anim=nil then begin
      anim := TRVJvGifImageAnimator.Create(RVData, Item);
      RVData.InsertAnimator(TObject(anim));
      end
    else if anim<>nil then begin
      anim.Update(RVData, Item);
      anim.Reset;
    end;
    exit;
  end;
  DefMakeAnimator(item, RVData, anim)
end;

initialization
  DefMakeAnimator := RV_MakeAnimator;
  RV_MakeAnimator := RV_MakeAnimatorGif;

{$ENDIF}

end.
