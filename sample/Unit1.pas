unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus,
  Winapi.ActiveX, Winapi.WinRT, System.Win.WinRT, Winapi.D2D1, Winapi.D2D1_1,
  Winapi.D3DCommon, Winapi.D3D11, Winapi.DXGI, Winapi.DXGI1_2, Winapi.DxgiType,
  Winapi.DxgiFormat, WinAPI.CommonTypes, WinAPI.Foundation.Types,
  WinAPI.Storage.Streams, WinAPI.Data.Pdf.Interop, WinAPI.Data.Pdf;

type
  TForm1 = class(TForm)
    PopupMenu1: TPopupMenu;
    A1: TMenuItem;
    FileSaveDialog1: TFileSaveDialog;
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure A1Click(Sender: TObject);
  private
    // デバイス非依存リソース
    FD2dFactory: ID2D1Factory1;
    // デバイス依存リソース
    FDxgiSwapChain: IDXGISwapChain1;
    FD2dDeviceContext: ID2D1DeviceContext;
    FPdfRenderer: IPdfRendererNative;
    // その他
    FFileName: string;
    FDocument: IPdfDocument;

    function CreateDeviceIndependentResources: Boolean;
    function Direct3DCreateDevice(driverType: D3D_DRIVER_TYPE;
      out ppDevice: ID3D11Device): HRESULT; overload;
    function Direct3DCreateDevice(out ppDevice: ID3D11Device): HRESULT; overload;
    function InitializeRenderTarget: HRESULT;
    function CreateDeviceResources: Boolean;
    function LoadPdf(s: IRandomAccessStream): Boolean; overload;
    function LoadPdf: Boolean; overload;
    procedure ReleaseDeviceResources;
    procedure HandleDeviceLost;
    procedure OnRender;
    procedure OnRenderCore;
  public
    { Public 宣言 }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{$WARN SYMBOL_PLATFORM OFF}
// from shcore.h
function CreateRandomAccessStreamOnFile(
  filePath: PWideChar; accessMode: FileAccessMode; const riid: TGUID; out ppv): HRESULT; stdcall;
  external 'SHCore.dll' delayed;
{$WARN SYMBOL_PLATFORM ON}

procedure TForm1.A1Click(Sender: TObject);
var
  pdfPage: IPdfPage;
  s: IRandomAccessStream;
  hr: HRESULT;
begin
  if FDocument = nil then Exit;
  FileSaveDialog1.FileName := ExtractFileName(ChangeFileExt(FFileName, '.png'));
  if not FileSaveDialog1.Execute(Handle) then Exit;
  pdfPage := FDocument.GetPage(0);

  if not FileExists(FileSaveDialog1.FileName) then
    with TFileStream.Create(FileSaveDialog1.FileName, fmCreate) do Free;
  hr := CreateRandomAccessStreamOnFile(PChar(FileSaveDialog1.FileName),
    FileAccessMode.ReadWrite, IRandomAccessStream, s);
  if Failed(hr) then Exit;
  pdfPage.RenderToStreamAsync(s);
end;

function TForm1.CreateDeviceIndependentResources: Boolean;
var
  options: D2D1_FACTORY_OPTIONS;
  hr: HRESULT;
begin
  options := Default(D2D1_FACTORY_OPTIONS);
{$IFDEF DEBUG}
  options.debugLevel := D2D1_DEBUG_LEVEL_INFORMATION;
{$ENDIF}
  hr := D2D1CreateFactory(D2D1_FACTORY_TYPE_MULTI_THREADED, ID2D1Factory1,
    @options, ID2D1Factory(FD2dFactory));
  Result := Succeeded(hr);
end;

function TForm1.Direct3DCreateDevice(driverType: D3D_DRIVER_TYPE;
  out ppDevice: ID3D11Device): HRESULT;
const
  featureLevels: array[0..6] of D3D_FEATURE_LEVEL = (
    D3D_FEATURE_LEVEL_11_1,
    D3D_FEATURE_LEVEL_11_0,
    D3D_FEATURE_LEVEL_10_1,
    D3D_FEATURE_LEVEL_10_0,
    D3D_FEATURE_LEVEL_9_3,
    D3D_FEATURE_LEVEL_9_2,
    D3D_FEATURE_LEVEL_9_1
  );
begin
  Result := D3D11CreateDevice(nil, driverType, 0,
    D3D11_CREATE_DEVICE_BGRA_SUPPORT, @featureLevels, Length(featureLevels),
    D3D11_SDK_VERSION, ppDevice, D3D_FEATURE_LEVEL(nil^),
    ID3D11DeviceContext(nil^));
end;

function TForm1.Direct3DCreateDevice(out ppDevice: ID3D11Device): HRESULT;
begin
  Result := Direct3DCreateDevice(D3D_DRIVER_TYPE_HARDWARE, ppDevice);
  if Succeeded(Result) then Exit;
  Result := Direct3DCreateDevice(D3D_DRIVER_TYPE_WARP, ppDevice);
end;

function BitmapProperties1(
  bitmapOptions: D2D1_BITMAP_OPTIONS = D2D1_BITMAP_OPTIONS_NONE;
  dxgiFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
  alphaMode: D2D1_ALPHA_MODE = D2D1_ALPHA_MODE_UNKNOWN;
  dpiX: Single = 96.0; dpiY: SIngle = 96.0;
  colorContext: ID2D1ColorContext = nil): D2D1_BITMAP_PROPERTIES1; inline;
begin
  Result.pixelFormat.format := dxgiFormat;
  Result.pixelFormat.alphaMode := alphaMode;
  Result.dpiX := dpiX;
  Result.dpiY := dpiY;
  Result.bitmapOptions := bitmapOptions;
  Result.colorContext := colorContext;
end;

function TForm1.InitializeRenderTarget: HRESULT;
var
  d2d1RenderTarget: ID2D1Bitmap1;
  dxgiSurface: IDXGISurface2;
  prop: TD2D1BitmapProperties1;
begin
  Result := FDxgiSwapChain.GetBuffer(
    0, IDXGISurface2, dxgiSurface);
  if Failed(Result) then Exit;

  prop := BitmapProperties1(
    D2D1_BITMAP_OPTIONS_TARGET or D2D1_BITMAP_OPTIONS_CANNOT_DRAW,
    DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_IGNORE);
  Result := FD2dDeviceContext.CreateBitmapFromDxgiSurface(
    dxgiSurface,
    @prop,
    d2d1RenderTarget);
  if Failed(Result) then Exit;
  FD2dDeviceContext.SetTarget(ID2D1Image(d2d1RenderTarget)); // <- 本来ID2D1Bitmap1はID2D1Imageを継承しているはずが、
                                                             //    エンバカデロがSDKの更新を反映してないのでキャストが必要。とてもFxxk。
  Result := S_OK;
end;

function TForm1.CreateDeviceResources: Boolean;
var
  hr: HRESULT;
  device: ID3D11Device;
  dxgiDevice: IDXGIDevice2;
  d2d1Device: ID2D1Device;
  dxgiAdapter: IDXGIAdapter;
  dxgiFactory: IDXGIFactory2;
  swapChainDesc: DXGI_SWAP_CHAIN_DESC1;
begin
  Result := False;
  hr := Direct3DCreateDevice(device);
  if Failed(hr) then Exit;
  if not Supports(device, IDXGIDevice2, dxgiDevice) then Exit;
  hr := FD2dFactory.CreateDevice(dxgiDevice, d2d1Device);
  if Failed(hr) then Exit;

  hr := d2d1Device.CreateDeviceContext(D2D1_DEVICE_CONTEXT_OPTIONS_NONE,
    FD2dDeviceContext);
  if Failed(hr) then Exit;
  hr := dxgiDevice.GetAdapter(dxgiAdapter);
  if Failed(hr) then Exit;
  hr := dxgiAdapter.GetParent(IDXGIFactory2, dxgiFactory);
  if Failed(hr) then Exit;

  swapChainDesc := Default(DXGI_SWAP_CHAIN_DESC1);
  swapChainDesc.Width := 0;
  swapChainDesc.Height := 0;
  swapChainDesc.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
  swapChainDesc.Stereo := False;
  swapChainDesc.SampleDesc.Count := 1;
  swapChainDesc.SampleDesc.Quality := 0;
  swapChainDesc.BufferUsage := DXGI_USAGE_RENDER_TARGET_OUTPUT;
  swapChainDesc.BufferCount := 2;
  swapChainDesc.Scaling := DXGI_SCALING_NONE;
  swapChainDesc.SwapEffect := DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL;
  swapChainDesc.AlphaMode := DXGI_ALPHA_MODE_UNSPECIFIED;
  hr := dxgiFactory.CreateSwapChainForHwnd(device, Handle, @swapChainDesc,
    nil, nil, FDxgiSwapChain);
  if Failed(hr) then Exit;
  dxgiDevice.SetMaximumFrameLatency(1);
  hr := InitializeRenderTarget;
  if Failed(hr) then Exit;
  hr := PdfCreateRenderer(dxgiDevice, FPdfRenderer);
  if Failed(hr) then Exit;
  Result := True;
end;

type
  TCallback = class(TInterfacedObject, IAsyncOperationCompletedHandler_1_Windows__CData__CPdf__CPdfDocument)
  private
    FAsync: IAsyncOperation<IPdfDocument>;
  public
    constructor Create(AAsync: IAsyncOperation<IPdfDocument>);
    procedure Invoke(
      asyncInfo: IAsyncOperation<IPdfDocument>;
      status: AsyncStatus); safecall;
  end;

constructor TCallback.Create(AAsync: IAsyncOperation<IPdfDocument>);
begin
  inherited Create;
  FAsync := AAsync;
end;

procedure TCallback.Invoke(asyncInfo: IAsyncOperation<IPdfDocument>; status: AsyncStatus);
begin
  if status <> AsyncStatus.Completed then
    Exit;
  Form1.FDocument := FAsync.GetResults;
  Form1.Invalidate;
end;

function TForm1.LoadPdf(s: IRandomAccessStream): Boolean;
var
  async: IAsyncOperation<IPdfDocument>;
  callback: IAsyncOperationCompletedHandler_1_Windows__CData__CPdf__CPdfDocument;
begin
  async := TWinRTGenericImportS<IPdfDocumentStatics>.Statics.LoadFromStreamAsync(s);
  callback := TCallback.Create(async);
  async.Completed := callback;
  Result := True;
end;

function TForm1.LoadPdf: Boolean;
var
  hr: HRESULT;
  s: IRandomAccessStream;
begin
  if ParamCount = 0 then
    Exit(False);
  if not FileExists(ParamStr(1)) then
    Exit(False);
  FFileName := ParamStr(1);
  hr := CreateRandomAccessStreamOnFile(PChar(FFileName),
    FileAccessMode.Read, IRandomAccessStream, s);
  if Failed(hr) then Exit(False);
  Result := LoadPdf(s);
end;

procedure TForm1.ReleaseDeviceResources;
begin
  FDxgiSwapChain := nil;
  FD2dDeviceContext := nil;
  FPdfRenderer := nil;
end;

procedure TForm1.HandleDeviceLost;
begin
  OutputDebugString('デバイスロストが発生しました。');

  ReleaseDeviceResources;
  CreateDeviceResources;
  Invalidate;
end;

procedure TForm1.OnRender;
var
  hr: HRESULT;
begin
  FD2dDeviceContext.BeginDraw;
  OnRenderCore;
  hr := FD2dDeviceContext.EndDraw;
  if hr = D2DERR_RECREATE_TARGET then
  begin
    HandleDeviceLost;
  end
  else if Succeeded(hr) then
  begin
    hr := FDxgiSwapChain.Present(1, 0);
    if (hr = DXGI_ERROR_DEVICE_REMOVED) or (hr = DXGI_ERROR_DEVICE_RESET) then
    begin
      HandleDeviceLost;
    end;
  end;
end;

procedure TForm1.OnRenderCore;
var
  color: TD2D1ColorF;
  pdfPage: IPdfPage;
  params: TPdfRenderParams;
begin
  color.r := 153;
  color.g := 153;
  color.b := 153;
  color.a := 1;
  FD2dDeviceContext.Clear(color);
  if FDocument <> nil then
  begin
    pdfPage := FDocument.GetPage(0);
    params := PdfRenderParams(sc_PdfRenderParamsDefaultSrcRect, 0, 0, sc_PdfRenderParamsDefaultBkColor);
    FPdfRenderer.RenderPageToDeviceContext(pdfPage, FD2dDeviceContext, @params);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if not CreateDeviceIndependentResources then
    Halt;
  if not CreateDeviceResources then
    Halt;
  if not LoadPdf then
    Halt;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ReleaseDeviceResources;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  OnRender;
  Invalidate;
end;

procedure TForm1.FormResize(Sender: TObject);
var
  swapChainDesc: DXGI_SWAP_CHAIN_DESC1;
  hr: HRESULT;
begin
  swapChainDesc := Default(DXGI_SWAP_CHAIN_DESC1);
  hr := FDxgiSwapChain.GetDesc1(swapChainDesc);
  if Succeeded(hr) and (swapChainDesc.Width = UINT(ClientWidth)) and (swapChainDesc.Height = UINT(ClientHeight)) then
    Exit;
  FD2dDeviceContext.SetTarget(nil);

  hr := FDxgiSwapChain.ResizeBuffers(swapChainDesc.BufferCount, 0, 0, DXGI_FORMAT_UNKNOWN, 0);
  if (hr = DXGI_ERROR_DEVICE_REMOVED) or (hr = DXGI_ERROR_DEVICE_RESET) then
  begin
    HandleDeviceLost;
  end;
  InitializeRenderTarget;

  Invalidate;
end;

initialization
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  Set8087CW($133F); // <- Floating point overflowの発生抑止
finalization
  CoUninitialize;
end.
