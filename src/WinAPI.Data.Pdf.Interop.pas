{*******************************************************}
{                                                       }
{  Files: windows.data.pdf.interop.h                    }
{    Copyright (C) Microsoft Corporation.               }
{    All Rights Reserved.                               }
{                                                       }
{  Translator: Lyna                                     }
{    Copyright(c) 2016 Lyna  All rights reserved.       }
{                                                       }
{*******************************************************}
unit Winapi.Data.Pdf.Interop;

{$ALIGN ON}
{$MINENUMSIZE 4}

interface

uses
  Winapi.Windows, Winapi.DXGI, Winapi.D2D1, Winapi.D2D1_1;

type
  PPdfRenderParams = ^TPdfRenderParams;
  PDF_RENDER_PARAMS = record
    SourceRect: D2D_RECT_F;
    DestinationWidth: UINT32;
    DestinationHeight: UINT32;
    BackgroundColor: D2D_COLOR_F;
    IgnoreHighContrast: BOOLEAN;
  end;
  TPdfRenderParams = PDF_RENDER_PARAMS;

  IPdfRendererNative = interface(IUnknown)
    ['{7d9dcd91-d277-4947-8527-07a0daeda94a}']
    procedure RenderPageToSurface(pdfPage: IUnknown; pSurface: IDXGISurface;
      offset: TPoint; pRenderParams: PPdfRenderParams = nil); safecall;
    procedure RenderPageToDeviceContext(pdfPage: IUnknown;
      pD2DDeviceContext: ID2D1DeviceContext;
      pRenderParams: PPdfRenderParams = nil); safecall;
  end;

  IID_IPdfRendererNative = IPdfRendererNative;

const
  sc_PdfRenderParamsDefaultSrcRect: D2D_RECT_F = (left: 0; top: 0; right: 0; bottom: 0);
  sc_PdfRenderParamsDefaultBkColor: D2D_COLOR_F = (r: 1; g: 1; b: 1; a: 1);

function PdfCreateRenderer(
  pDevice: IDXGIDevice;
  out ppRenderer: IPdfRendererNative): HRESULT; stdcall;

function PdfRenderParams(
  const srcRect: D2D_RECT_F {= sc_PdfRenderParamsDefaultSrcRect};
  destinationWidth: UINT32 {= 0};
  destinationHeight: UINT32 {= 0};
  const bkColor: D2D_COLOR_F {= sc_PdfRenderParamsDefaultBkColor};
  ignoreHighContrast: Boolean = True): PDF_RENDER_PARAMS; inline;

implementation

function PdfCreateRenderer; external 'Windows.Data.Pdf.dll';

function PdfRenderParams(
  const srcRect: D2D_RECT_F {= sc_PdfRenderParamsDefaultSrcRect};
  destinationWidth: UINT32 {= 0};
  destinationHeight: UINT32 {= 0};
  const bkColor: D2D_COLOR_F {= sc_PdfRenderParamsDefaultBkColor};
  ignoreHighContrast: Boolean = True): PDF_RENDER_PARAMS;
begin
  Result.SourceRect := srcRect;
  Result.DestinationWidth := destinationWidth;
  Result.DestinationHeight := destinationHeight;
  Result.BackgroundColor := bkColor;
  Result.IgnoreHighContrast := ignoreHighContrast;
end;

end.