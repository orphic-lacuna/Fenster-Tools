unit LayeredWindowRenderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, Win32Extra, ToolBox, Direct2DRenderer, DX12.D2D1{$IfDef Logging}, logging{$EndIf};

type
  { TLayeredWindowRenderer }

  TLayeredWindowRenderer = class(TDirect2DRenderer)
  strict private
    procedure CleanUpGDIObjects;
  private
    fTargetWnd: HWND;
    fWndRect: TRect;
    fWndSize: TSize;
    blendFunc: TBlendFunction;
    blend_pptSrc: POINT;
    fLayeredWndDC: HDC;
    fLayeredWndBmp: HBITMAP;

    function GetOpacity: Byte;
    procedure SetOpacity(aValue: Byte);
  protected
    procedure UpdateRenderTarget; virtual;
  public
    property WindowRect: TRect read fWndRect;
    property WindowSize: TSize read fWndSize;
    property Opacity: Byte read GetOpacity write SetOpacity;

    procedure Flip;

    constructor Create(aWnd: HWND);
    destructor Destroy; override;
  end;

implementation

{ TLayeredWindowRenderer }

constructor TLayeredWindowRenderer.Create(aWnd: HWND);
begin
  inherited Create;
  fTargetWnd := aWnd;

  blendFunc.BlendOp := AC_SRC_OVER;
  blendFunc.BlendFlags := 0;
  blendFunc.SourceConstantAlpha := 255;
  blendFunc.AlphaFormat := AC_SRC_ALPHA;
  blend_pptSrc.x := 0; blend_pptSrc.y := 0;
end;

procedure TLayeredWindowRenderer.CleanUpGDIObjects;
begin
  if fLayeredWndDC <> 0 then
  begin
    DeleteDC(fLayeredWndDC);
    fLayeredWndDC := 0;
  end;
  if fLayeredWndBmp <> 0 then
  begin
    DeleteObject(fLayeredWndBmp);
    fLayeredWndBmp := 0;
  end;
end;

function TLayeredWindowRenderer.GetOpacity: Byte;
begin
  Result := blendFunc.SourceConstantAlpha;
end;

procedure TLayeredWindowRenderer.SetOpacity(aValue: Byte);
begin
  blendFunc.SourceConstantAlpha := aValue;
end;

procedure TLayeredWindowRenderer.UpdateRenderTarget;
var
  dc: HDC;
begin
  CleanUpGDIObjects;
  GetWindowRect(fTargetWnd, fWndRect{%H-});
  {$IfDef Debug}
    if (fWndRect.Right <= fWndRect.Left) or (fWndRect.Bottom <= fWndRect.Top) then raise Exception.Create('WindowRect of Clipboard window could not be determined.');
  {$EndIf}

  fWndSize.cx := fWndRect.Right-fWndRect.Left; fWndSize.cy := fWndRect.Bottom-fWndRect.Top;
  dc := GetDC(0);
  fLayeredWndDC := CreateCompatibleDC(dc);
  fLayeredWndBmp := CreateCompatibleBitmap(dc, fWndRect.Right-fWndRect.Left, fWndRect.Bottom-fWndRect.Top);
  SelectObject(fLayeredWndDC, fLayeredWndBmp);
  ReleaseDC(0, dc);
  fWndRect.Right := fWndRect.Right-fWndRect.Left; fWndRect.Bottom := fWndRect.Bottom-fWndRect.Top; fWndRect.Left := 0; fWndRect.Top := 0;
  SetRenderTargetDC(fLayeredWndDC, fWndRect);
end;

{
procedure iconToD2D1Bitmap;
var
  hIcon: HICON;
  wicBitmap: IWICBitmap;
  wicConverter: IWICFormatConverter;
  wicFactory: IWICImagingFactory;
  bitmapProps: D2D1_BITMAP_PROPERTIES1;
  bitmap: ID2D1Bitmap1;
begin
  // get a HICON
  hIcon := SendMessage(Handle, WM_GETICON, ICON_BIG, 0);
  try
    // create wic imaging factory
    CoCreateInstance(CLSID_WICImagingFactory, nil, CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IUnknown, wicFactory);

    wicFactory.CreateBitmapFromHICON(hIcon, wicBitmap);
    wicFactory.CreateFormatConverter(wicConverter);

    wicConverter.Initialize(wicBitmap, GUID_WICPixelFormat32bppPBGRA, WICBitmapDitherTypeNone, nil, 0, WICBitmapPaletteTypeMedianCut);

    bitmapProps.bitmapOptions := D2D1_BITMAP_OPTIONS_NONE;
    bitmapProps.pixelFormat.format := DXGI_FORMAT_B8G8R8A8_UNORM;
    bitmapProps.pixelFormat.alphaMode := D2D1_ALPHA_MODE_PREMULTIPLIED;
    bitmapProps.dpiX := 96;
    bitmapProps.dpiY := 96;
    bitmapProps.colorContext := nil;

    // deviceContext should be a valid D2D1DeviceContext
    deviceContext.CreateBitmapFromWicBitmap(wicConverter, @bitmapProps, bitmap);

    // the bitmap variable contains your icon

  except
    //
  end;}

procedure TLayeredWindowRenderer.Flip;
begin
  UpdateLayeredWindow(fTargetWnd, 0, nil, @fWndSize, fLayeredWndDC, @blend_pptSrc, 0, @blendFunc, ULW_ALPHA);
end;

destructor TLayeredWindowRenderer.Destroy;
begin
  CleanUpGDIObjects;
  inherited Destroy;
end;

end.

