{*******************************************************}
{                                                       }
{  Files: windows.data.pdf.h                            }
{    Copyright (C) Microsoft Corporation.               }
{    All Rights Reserved.                               }
{                                                       }
{  Translator: Lyna                                     }
{    Copyright(c) 2016 Lyna  All rights reserved.       }
{                                                       }
{*******************************************************}
unit Winapi.Data.Pdf;

{$ALIGN ON}
{$MINENUMSIZE 4}
{$SCOPEDENUMS ON}

interface

uses
  System.Win.WinRT, Winapi.Windows, Winapi.Winrt, Winapi.Foundation, Winapi.Foundation.Types,
  Winapi.Storage, Winapi.Storage.Streams;

type
  // Windows.Foundation.Size
  TWindowsFoundationSize = record
    Width: Single;
    Height: Single;
  end;
  
  // from Windows.Foundation.Rect
  TWindowsFoundationRect = record
    X: Single;
    Y: Single;
    Width: Single;
    Height: Single;
  end;

  // Windows.UI.Color
  TWindowsUIColor = record
    A: BYTE;
    R: BYTE;
    G: BYTE;
    B: BYTE;
  end;

const
  RuntimeClass_Windows_Data_Pdf_PdfPageRenderOptions = 'Windows.Data.Pdf.PdfPageRenderOptions';
  RuntimeClass_Windows_Data_Pdf_PdfPageDimensions = 'Windows.Data.Pdf.PdfPageDimensions';
  RuntimeClass_Windows_Data_Pdf_PdfPage = 'Windows.Data.Pdf.PdfPage';
  RuntimeClass_Windows_Data_Pdf_PdfDocument = 'Windows.Data.Pdf.PdfDocument';

type
  IAsyncOperation_1_Windows__CData__CPdf__CPdfDocument = interface;

  IAsyncOperation<T: IInspectable> = interface;

  IAsyncOperationCompletedHandler<T: IInspectable> = interface(IUnknown)
    ['{5233899b-ba7e-504f-bb83-ceebac62decf}']
    procedure Invoke(asyncInfo: IAsyncOperation<T>; status: AsyncStatus); safecall;
  end;

  IAsyncOperation<T: IInspectable> = interface(IInspectable)
    ['{6607bc41-294b-5975-9c3f-4b49836d0916}']
    procedure put_Completed(handler: IAsyncOperationCompletedHandler<T>); safecall;
    function get_Completed: IAsyncOperationCompletedHandler<T>; safecall;
    function GetResults: T; safecall;
    property Completed: IAsyncOperationCompletedHandler<T> read get_Completed write put_Completed;
  end;

  // Windows.Data.Pdf.PdfPageRotation
  PdfPageRotation = (
    Normal	= 0,
    Rotate90	= 1,
    Rotate180	= 2,
    Rotate270	= 3
  );
  TPdfPageRotation = PdfPageRotation;

  // Windows.Data.Pdf.IPdfPageDimensions
  [WinRTClassNameAttribute(RuntimeClass_Windows_Data_Pdf_PdfPageDimensions)]
  IPdfPageDimensions = interface(IInspectable)
    ['{22170471-313E-44E8-835D-63A3E7624A10}']
    function get_MediaBox: TWindowsFoundationRect; safecall;
    function get_CropBox: TWindowsFoundationRect; safecall;
    function get_BleedBox: TWindowsFoundationRect; safecall;
    function get_TrimBox: TWindowsFoundationRect; safecall;
    function get_ArtBox: TWindowsFoundationRect; safecall;
    property MediaBox: TWindowsFoundationRect read get_MediaBox;
    property CropBox: TWindowsFoundationRect read get_CropBox;
    property BleedBox: TWindowsFoundationRect read get_BleedBox;
    property TrimBox: TWindowsFoundationRect read get_TrimBox;
    property ArtBox: TWindowsFoundationRect read get_ArtBox;
  end;

  // Windows.Data.Pdf.IPdfPageRenderOptions
  [WinRTClassNameAttribute(RuntimeClass_Windows_Data_Pdf_PdfPageRenderOptions)]
  IPdfPageRenderOptions = interface(IInspectable)
    ['{3C98056F-B7CF-4C29-9A04-52D90267F425}']
    function get_SourceRect: TWindowsFoundationRect; safecall;
    procedure put_SourceRect(value: TWindowsFoundationRect); safecall;
    function get_DestinationWidth: UINT32; safecall;
    procedure put_DestinationWidth(value: UINT32); safecall;
    function get_DestinationHeight: UINT32; safecall;
    procedure put_DestinationHeight(value: UINT32); safecall;
    function get_BackgroundColor: TWindowsUIColor; safecall;
    procedure put_BackgroundColor(value: TWindowsUIColor); safecall;
    function get_IsIgnoringHighContrast: Boolean; safecall;
    procedure put_IsIgnoringHighContrast(value: Boolean); safecall;
    function get_BitmapEncoderId: TGUID; safecall;
    procedure put_BitmapEncoderId(value: TGUID); safecall;
    property SourceRect: TWindowsFoundationRect read get_SourceRect write put_SourceRect;
    property DestinationWidth: UINT32 read get_DestinationWidth write put_DestinationWidth;
    property DestinationHeight: UINT32 read get_DestinationHeight write put_DestinationHeight;
    property BackgroundColor: TWindowsUIColor read get_BackgroundColor write put_BackgroundColor;
    property IsIgnoringHighContrast: Boolean read get_IsIgnoringHighContrast write put_IsIgnoringHighContrast;
    property BitmapEncoderId: TGUID read get_BitmapEncoderId write put_BitmapEncoderId;
  end;

  // Windows.Data.Pdf.IPdfPage
  [WinRTClassNameAttribute(RuntimeClass_Windows_Data_Pdf_PdfPage)]
  IPdfPage = interface(IInspectable)
    ['{9DB4B0C8-5320-4CFC-AD76-493FDAD0E594}']
    function RenderToStreamAsync(
      outputStream: IRandomAccessStream): IAsyncAction; safecall;
    function RenderWithOptionsToStreamAsync(
      outputStream: IRandomAccessStream;
      options: IPdfPageRenderOptions): IAsyncAction; safecall;
    function PreparePageAsync: IAsyncAction; safecall;
    function get_Index: UINT32; safecall;
    function get_Size: TWindowsFoundationSize; safecall;
    function get_Dimensions: IPdfPageDimensions; safecall;
    function get_Rotation: TPdfPageRotation; safecall;
    function get_PreferredZoom: Single; safecall;
    property Index: UINT32 read get_Index;
    property Size: TWindowsFoundationSize read get_Size;
    property Dimensions: IPdfPageDimensions read get_Dimensions;
    property Rotation: TPdfPageRotation read get_Rotation;
    property PreferredZoom: Single read get_PreferredZoom;
  end;

  // Windows.Data.Pdf.IPdfDocument
  [WinRTClassNameAttribute(RuntimeClass_Windows_Data_Pdf_PdfDocument)]
  IPdfDocument = interface(IInspectable)
    ['{AC7EBEDD-80FA-4089-846E-81B77FF5A86C}']
    function GetPage(pageIndex: UINT32): IPdfPage; safecall;
    function get_PageCount: UINT32; safecall;
    function get_IsPasswordProtected: Boolean; safecall;
    property PageCount: UINT32 read get_PageCount;
    property IsPasswordProtected: Boolean read get_IsPasswordProtected;
  end;

  // Windows.Data.Pdf.IPdfDocumentStatics
  [WinRTClassNameAttribute(RuntimeClass_Windows_Data_Pdf_PdfDocument)]
  IPdfDocumentStatics = interface(IInspectable)
    ['{433A0B5F-C007-4788-90F2-08143D922599}']
    function LoadFromFileAsync(
      file_: IStorageFile): IAsyncOperation<IPdfDocument>; safecall;
    function LoadFromFileWithPasswordAsync(
      file_: IStorageFile;
      password: HSTRING): IAsyncOperation<IPdfDocument>; safecall;
    function LoadFromStreamAsync(
      inputStream: IRandomAccessStream): IAsyncOperation<IPdfDocument>; safecall;
    function LoadFromStreamWithPasswordAsync(
      inputStream: IRandomAccessStream;
      password: HSTRING): IAsyncOperation<IPdfDocument>; safecall;
  end;

  IAsyncOperationCompletedHandler_1_Windows__CData__CPdf__CPdfDocument = interface(IAsyncOperationCompletedHandler<IPdfDocument>)
    ['{8d4950b3-629d-5d7d-84cc-04c0dcf7942b}']
  end;

  IAsyncOperation_1_Windows__CData__CPdf__CPdfDocument = interface(IAsyncOperation<IPdfDocument>)
    ['{d6b166ec-099a-5ee2-ad2e-f4c88614aabb}']
  end;

implementation

end.