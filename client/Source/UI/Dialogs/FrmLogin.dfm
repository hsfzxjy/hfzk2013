object LoginForm: TLoginForm
  Left = 0
  Top = 0
  Caption = #30331#24405
  ClientHeight = 378
  ClientWidth = 516
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object WBLogin: TEmbeddedWB
    Left = 0
    Top = 25
    Width = 516
    Height = 353
    Align = alClient
    TabOrder = 0
    OnDownloadComplete = WBLoginDownloadComplete
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    About = ' EmbeddedWB http://bsalsa.com/'
    PrintOptions.Margins.Left = 19.050000000000000000
    PrintOptions.Margins.Right = 19.050000000000000000
    PrintOptions.Margins.Top = 19.050000000000000000
    PrintOptions.Margins.Bottom = 19.050000000000000000
    PrintOptions.Header = 'w&b'#39029#30721#65292'&p/&P(&W)'
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Footer = '&u&b&d'
    PrintOptions.Orientation = poPortrait
    ExplicitLeft = 120
    ExplicitTop = 136
    ExplicitWidth = 300
    ExplicitHeight = 150
    ControlData = {
      4C00000055350000112700000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object StatusPanel: TPanel
    Left = 0
    Top = 0
    Width = 516
    Height = 25
    Align = alTop
    Alignment = taLeftJustify
    BevelInner = bvRaised
    BevelOuter = bvNone
    Caption = #31967#31957#65281#26381#21153#22120#36830#25509#19981#19978#8230#35831#24744#37325#26032#30331#24405#21543#65281
    Color = clWindow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 1
  end
  object IECache: TIECache
    FilterOptions = [NORMAL_ENTRY, STICKY_ENTRY, COOKIE_ENTRY, URLHISTORY_ENTRY, TRACK_OFFLINE_ENTRY, TRACK_ONLINE_ENTRY]
    SearchPattern = spAll
    Left = 272
    Top = 200
  end
end
