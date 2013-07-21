
{*******************************************************}
{                                                       }
{       RichView                                        }
{       Registering all non data-aware components of    }
{       RichView Package.                               }
{       This unit must not be used by applications      }
{       themselves.                                     }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit RVReg;

interface
uses Classes, RVStyle, RichView, RVEdit,  PtblRV, RVPP, CtrlImg, RVMisc, RVTable, RVReport,
     RVOfficeCnv;

procedure Register;

implementation
uses RVStr;

procedure Register;
begin
  RegisterComponents(RVPalettePage,
    [TRVStyle,TRichView,TRichViewEdit,TRVPrint,TRVPrintPreview, TRVReportHelper,
     TRVOfficeConverter]);
end;

end.
