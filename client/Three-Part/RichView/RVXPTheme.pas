
{*******************************************************}
{                                                       }
{       RichView                                        }
{       Windows XP Themes API                           }
{       (only functions required for RichView)          }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit RVXPTheme;

interface
{$I RV_Defs.inc}

uses SysUtils, Windows;

type
  HTheme = Cardinal;

  RV_IsThemeActiveProc = function : boolean; stdcall;
  RV_IsAppThemedProc =  function: boolean; stdcall;
  RV_OpenThemeDataProc = function (hwnd : HWND; pszClassList : PWideChar) : HTheme; stdcall;
  RV_CloseThemeDataProc= function (Theme : HTheme) : HRESULT; stdcall;
  RV_DrawThemeParentBackgroundProc = function (hwnd : HWND; hdc : HDC; Rect : PRect) : HRESULT; stdcall;
  RV_DrawThemeEdgeProc = function(Theme : HTheme; hdc : HDC;
    iPartId, iStateId: Integer; const pDestRect: TRect; uEdge, uFlags: UINT;
    pContentRect: PRect): HRESULT; stdcall;
  RV_DrawThemeBackgroundProc = function (Theme : HTheme; hdc : HDC;
    iPartId : integer; iStateId : integer; const Rect : TRect; {OPTIONAL} pClipRect : PRect) : HRESULT; stdcall;
  RV_DrawThemeTextProc = function (Theme : HTheme; hdc : HDC; iPartId, iStateId : integer;
    pszText : PWideChar; iCharCount : integer; dwTextFlags : DWORD;
    dwTextFlags2 : DWORD; var Rect : TRect) : HRESULT; stdcall;
  RV_IsThemeBackgroundPartiallyTransparentProc =
    function(hTheme: HTheme; iPartId, iStateId: Integer): Boolean; stdcall;

var
  RV_IsThemeActive: RV_IsThemeActiveProc;
  RV_IsAppThemed: RV_IsAppThemedProc;
  RV_OpenThemeData: RV_OpenThemeDataProc;
  RV_CloseThemeData: RV_CloseThemeDataProc;
  RV_DrawThemeParentBackground: RV_DrawThemeParentBackgroundProc;
  RV_DrawThemeEdge: RV_DrawThemeEdgeProc;
  RV_DrawThemeBackground: RV_DrawThemeBackgroundProc;
  RV_DrawThemeText: RV_DrawThemeTextProc;
  RV_IsThemeBackgroundPartiallyTransparent: RV_IsThemeBackgroundPartiallyTransparentProc;

const
{$IFNDEF RICHVIEWDEF2006}
  WM_THEMECHANGED          = $031A;
{$ENDIF}

  EP_EDITTEXT              = 1;

  ETS_NORMAL               = 1;
  ETS_HOT                  = 2;
  ETS_SELECTED             = 3;
  ETS_DISABLED             = 4;
  ETS_FOCUSED              = 5;
  ETS_READONLY             = 6;
  ETS_ASSIST               = 7;

  BP_RADIOBUTTON          = 2;

  RBS_UNCHECKEDNORMAL     = 1;
  RBS_UNCHECKEDHOT        = 2;
  RBS_UNCHECKEDPRESSED    = 3;
  RBS_UNCHECKEDDISABLED   = 4;
  RBS_CHECKEDNORMAL       = 5;
  RBS_CHECKEDHOT          = 6;
  RBS_CHECKEDPRESSED      = 7;
  RBS_CHECKEDDISABLED     = 8;

  BP_CHECKBOX             = 3;

  CBS_UNCHECKEDNORMAL     = 1;
  CBS_UNCHECKEDHOT        = 2;
  CBS_UNCHECKEDPRESSED    = 3;
  CBS_UNCHECKEDDISABLED   = 4;
  CBS_CHECKEDNORMAL       = 5;
  CBS_CHECKEDHOT          = 6;
  CBS_CHECKEDPRESSED      = 7;
  CBS_CHECKEDDISABLED     = 8;
  CBS_MIXEDNORMAL         = 9;
  CBS_MIXEDHOT            = 10;
  CBS_MIXEDPRESSED        = 11;
  CBS_MIXEDDISABLED       = 12;

  BP_GROUPBOX             = 4;

  GBS_NORMAL              = 1;
  GBS_DISABLED            = 2;

  CP_DROPDOWNBUTTON        = 1;

  CBXS_NORMAL              = 1;
  CBXS_HOT                 = 2;
  CBXS_PRESSED             = 3;
  CBXS_DISABLED            = 4;

implementation

var hThemeLib: HINST;

procedure Init;
begin
  hThemeLib := 0;
  if (Win32Platform  = VER_PLATFORM_WIN32_NT)
     {$IFDEF RICHVIEWCBDEF3}
      and
     (((Win32MajorVersion = 5) and (Win32MinorVersion >= 1)) or
      (Win32MajorVersion > 5))
     {$ENDIF}
      then
  begin
    RV_IsThemeActive := nil;
    RV_IsAppThemed := nil;
    RV_OpenThemeData := nil;
    RV_CloseThemeData := nil;
    RV_DrawThemeParentBackground := nil;
    RV_DrawThemeText := nil;
    RV_DrawThemeEdge := nil;
    RV_DrawThemeBackground := nil;
    RV_IsThemeBackgroundPartiallyTransparent := nil;
    hThemeLib := LoadLibrary('uxtheme.dll');
    if hThemeLib <> 0 then
    begin
      RV_IsThemeActive := GetProcAddress(hThemeLib, 'IsThemeActive');
      RV_IsAppThemed   := GetProcAddress(hThemeLib, 'IsAppThemed');
      RV_OpenThemeData := GetProcAddress(hThemeLib, 'OpenThemeData');
      RV_CloseThemeData := GetProcAddress(hThemeLib, 'CloseThemeData');
      RV_DrawThemeParentBackground := GetProcAddress(hThemeLib, 'DrawThemeParentBackground');
      RV_DrawThemeText := GetProcAddress(hThemeLib, 'DrawThemeText');
      RV_DrawThemeEdge := GetProcAddress(hThemeLib, 'DrawThemeEdge');
      RV_DrawThemeBackground := GetProcAddress(hThemeLib, 'DrawThemeBackground');
      RV_IsThemeBackgroundPartiallyTransparent := GetProcAddress(hThemeLib, 'IsThemeBackgroundPartiallyTransparent');
    end;
  end;
end;

initialization
  Init;
finalization
  if hThemeLib <> 0 then
    FreeLibrary(hThemeLib);


end.
