unit PdfDoc;

interface

uses
  Winapi.ActiveX, Winapi.WinRT, WinAPI.CommonTypes, WinAPI.Foundation,
  WinAPI.Foundation.Types, WinAPI.Storage.Streams, WinAPI.Data.Pdf,
  Winapi.Wincodec, System.SysUtils, System.Classes, System.Win.WinRT;

type
  TPdfDocument = class;

  TExportFileType = (eftBmp, eftJpeg, eftPng, eftTiff, eftGif, eftJpegXR);

  TPdfPage = record
  private
  type
    TCallback = class(TInterfacedObject, AsyncActionCompletedHandler)
    private
      FDoc: TPdfDocument;
      FCompleteProc: TProc;
    public
      constructor Create(ADocument: TPdfDocument; ACompleteProc: TProc); overload;
      constructor Create(ADocument: TPdfDocument); overload;
      procedure Invoke(asyncInfo: IAsyncAction; status: AsyncStatus); safecall;
    end;
  var
    FDoc: TPdfDocument;
    FPage: IPdfPage;
    function GetHeight: Single; inline;
    function GetIndex: UInt32; inline;
    function GetWidth: Single; inline;
  public
    procedure ExportAsImage(Adapter: IStream; FileType: TExportFileType = eftPng;
      DestWidth: UInt32 = 0; DestHeight: UInt32 = 0; CompleteProc: TProc = nil); overload;
    procedure ExportAsImage(Stream: TStream; FileType: TExportFileType = eftPng;
      DestWidth: UInt32 = 0; DestHeight: UInt32 = 0; CompleteProc: TProc = nil); overload;
    procedure ExportAsImage(const FileName: string; FileType: TExportFileType = eftPng;
      DestWidth: UInt32 = 0; DestHeight: UInt32 = 0; CompleteProc: TProc = nil); overload;
    property Handle: IPdfPage read FPage;
    property Height: Single read GetHeight;
    property Index: UInt32 read GetIndex;
    property Width: Single read GetWidth;
  end;

  TPdfDocument = class(TComponent)
  private
  type
    TCallback = class(TInterfacedObject, IAsyncOperationCompletedHandler_1_Windows__CData__CPdf__CPdfDocument)
    private
      FDoc: TPdfDocument;
      FCompleteProc: TProc;
    public
      constructor Create(ADocument: TPdfDocument; ACompleteProc: TProc); overload;
      constructor Create(ADocument: TPdfDocument); overload;
      procedure Invoke(asyncInfo: IAsyncOperation<IPdfDocument>; status: AsyncStatus); safecall;
    end;
  var
    FDoc: IPdfDocument;
    FLoaded: Boolean;
    FOnExportComplete: TNotifyEvent;
    FOnLoadComplete: TNotifyEvent;
    function GetPageCount: Integer;
    function GetPages(Index: Integer): TPdfPage;
  public
    constructor Create; reintroduce; overload;
    procedure LoadFromFile(const FileName: string; CompleteProc: TProc = nil);

    property DocumentLoaded: Boolean read FLoaded;
    property Handle: IPdfDocument read FDoc;
    property Pages[Index: Integer]: TPdfPage read GetPages; default;
    property PageCount: Integer read GetPageCount;
  published
    property OnExportComplete: TNotifyEvent read FOnExportComplete write FOnExportComplete;
    property OnLoadComplete: TNotifyEvent read FOnLoadComplete write FOnLoadComplete;
  end;

  EPdfException = class(Exception);

procedure Register;

implementation

resourcestring
  SNotLoadedError = 'PDFがロードされていません';
  SNowLoadingError = 'PDFは現在ロード中です';
  SLoadError = 'PDFがロードできません';
  SExportError = 'PDFのエクスポート中にエラーが発生しました';

procedure NotLoadedError;
begin
  raise EPdfException.CreateRes(@SNotLoadedError);
end;

procedure NowLoadingError;
begin
  raise EPdfException.CreateRes(@SNowLoadingError);
end;

{$WARN SYMBOL_PLATFORM OFF}
// from shcore.h
function CreateRandomAccessStreamOnFile(
  filePath: PWideChar; accessMode: FileAccessMode; const riid: TGUID; out ppv): HRESULT; stdcall;
  external 'SHCore.dll' delayed;
type
{$MINENUMSIZE 4}
  BSOS_OPTIONS = (
    BSOS_DEFAULT = 0,             // when creating a byte seeker over a stream, base randomaccessstream behavior on the STGM mode from IStream::Stat.
    BSOS_PREFERDESTINATIONSTREAM  // in addition, utilize IDestinationStreamFactory::GetDestinationStream.
  );
  TBSOSOptions = BSOS_OPTIONS;
{$MINENUMSIZE 1}
function CreateRandomAccessStreamOverStream(stream: IStream; options: TBSOSOptions;
  const riid: TGUID; out ppv): HRESULT; stdcall; external 'SHCore.dll' delayed;
{$WARN SYMBOL_PLATFORM ON}

{ TPdfPage }

procedure TPdfPage.ExportAsImage(Adapter: IStream; FileType: TExportFileType = eftPng;
  DestWidth: UInt32 = 0; DestHeight: UInt32 = 0; CompleteProc: TProc = nil);
var
  stream: IRandomAccessStream;
  options: IPdfPageRenderOptions;
  async: IAsyncAction;
begin
  if Failed(CreateRandomAccessStreamOverStream(Adapter, BSOS_DEFAULT, IRandomAccessStream, stream)) then
    raise EPdfException.CreateRes(@SExportError);

  options := TWinRTGenericImportI<IPdfPageRenderOptions>.Create;
  options.DestinationWidth := DestWidth;
  options.DestinationHeight := DestHeight;
  case FileType of
    eftBmp: options.BitmapEncoderId := CLSID_WICBmpEncoder;
    eftJpeg: options.BitmapEncoderId := CLSID_WICJpegEncoder;
    eftPng: options.BitmapEncoderId := CLSID_WICPngEncoder;
    eftTiff: options.BitmapEncoderId := CLSID_WICTiffEncoder;
    eftGif: options.BitmapEncoderId := CLSID_WICGifEncoder;
    eftJpegXR: options.BitmapEncoderId := CLSID_WICWmpEncoder;
  end;

  async := FPage.RenderWithOptionsToStreamAsync(stream, options);
  if Assigned(CompleteProc) then
    async.Completed := TCallback.Create(FDoc, CompleteProc)
  else
    async.Completed := TCallback.Create(FDoc);
end;

procedure TPdfPage.ExportAsImage(Stream: TStream; FileType: TExportFileType = eftPng;
  DestWidth: UInt32 = 0; DestHeight: UInt32 = 0; CompleteProc: TProc = nil);
var
  adapter: IStream;
begin
  adapter := TStreamAdapter.Create(Stream, soReference);
  ExportAsImage(adapter, FileType, DestWidth, DestHeight, CompleteProc);
end;

procedure TPdfPage.ExportAsImage(const FileName: string;
  FileType: TExportFileType = eftPng; DestWidth: UInt32 = 0;
  DestHeight: UInt32 = 0; CompleteProc: TProc = nil);
var
  adapter: IStream;
begin
  adapter := TStreamAdapter.Create(TFileStream.Create(FileName, fmCreate), soOwned);
  ExportAsImage(adapter, FileType, DestWidth, DestHeight, CompleteProc);
end;

function TPdfPage.GetHeight: Single;
begin
  Result := FPage.Size.Width;
end;

function TPdfPage.GetIndex: UInt32;
begin
  Result := FPage.Index;
end;

function TPdfPage.GetWidth: Single;
begin
  Result := FPage.Size.Height;
  FPage.PreferredZoom
end;

{ TPdfPage.TCallback }

constructor TPdfPage.TCallback.Create(ADocument: TPdfDocument; ACompleteProc: TProc);
begin
  inherited Create;
  FDoc := ADocument;
  FCompleteProc := ACompleteProc;
end;

constructor TPdfPage.TCallback.Create(ADocument: TPdfDocument);
begin
  inherited Create;
  FDoc := ADocument;
end;

procedure TPdfPage.TCallback.Invoke(asyncInfo: IAsyncAction; status: AsyncStatus);
begin
  if status <> AsyncStatus.Completed then
    Exit;
  asyncInfo.GetResults;
  if Assigned(FCompleteProc) then
    FCompleteProc()
  else if Assigned(FDoc.OnExportComplete) then
    FDoc.OnExportComplete(FDoc);
end;

{ TPdfDocument }

constructor TPdfDocument.Create;
begin
  inherited Create(nil);
end;

procedure TPdfDocument.LoadFromFile(const FileName: string; CompleteProc: TProc = nil);
var
  async: IAsyncOperation<IPdfDocument>;
  stream: IRandomAccessStream;
  callback: IAsyncOperationCompletedHandler_1_Windows__CData__CPdf__CPdfDocument;
begin
  FLoaded := False;
  FDoc := nil;

  if Failed(CreateRandomAccessStreamOnFile(PChar(FileName), FileAccessMode.Read, IRandomAccessStream, stream)) then
    raise EPdfException.Create(SLoadError);

  async := TWinRTGenericImportS<IPdfDocumentStatics>.Statics.LoadFromStreamAsync(stream);
  if Assigned(CompleteProc) then
    callback := TCallback.Create(Self, CompleteProc)
  else
    callback := TCallback.Create(Self);
  async.Completed := callback;
end;

function TPdfDocument.GetPageCount: Integer;
begin
  if FDoc = nil then
    NotLoadedError;
  if not FLoaded then
    NowLoadingError;

  Result := FDoc.PageCount;
end;

function TPdfDocument.GetPages(Index: Integer): TPdfPage;
begin
  if FDoc = nil then
    NotLoadedError;
  if not FLoaded then
    NowLoadingError;

  Result.FDoc := Self;
  Result.FPage := FDoc.GetPage(Index);
end;

{ TPdfDocument.TCallback }

constructor TPdfDocument.TCallback.Create(ADocument: TPdfDocument; ACompleteProc: TProc);
begin
  inherited Create;
  FDoc := ADocument;
  FCompleteProc := ACompleteProc;
end;

constructor TPdfDocument.TCallback.Create(ADocument: TPdfDocument);
begin
  inherited Create;
  FDoc := ADocument;
end;

procedure TPdfDocument.TCallback.Invoke(asyncInfo: IAsyncOperation<IPdfDocument>; status: AsyncStatus);
begin
  if status <> AsyncStatus.Completed then
    Exit;
  FDoc.FDoc := asyncInfo.GetResults;
  FDoc.FLoaded := True;
  if Assigned(FCompleteProc) then
    FCompleteProc()
  else if Assigned(FDoc.OnLoadComplete) then
    FDoc.OnLoadComplete(FDoc);
end;

procedure Register;
begin
  RegisterComponents('Samples', [TPdfDocument]);
end;

end.
