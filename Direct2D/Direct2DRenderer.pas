unit Direct2DRenderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DX12.D2D1, DX12.DWrite, DX12.DCommon, DX12.DXGI, ActiveX, Windows, Dialogs;

type

  { TDirect2DRenderer }

  TDirect2DRenderer = class abstract
  strict private
    fTarget: HANDLE;
    fTargetRect: TRect;
    fFactory: ID2D1Factory;
    fWriteFactory: IDWriteFactory;
    fRenderTarget: ID2D1RenderTarget;

    { Is called by Render() if fRenderTarget.EndDraw returns D2DERR_RECREATE_TARGET }
    procedure RecreateTarget;
  protected
    property Factory: ID2D1Factory read fFactory;
    property WriteFactory: IDWriteFactory read fWriteFactory;
    property RenderTarget: ID2D1RenderTarget read fRenderTarget;

    { Overwrite this function in your derived renderer class }
    procedure CreateDeviceDependentResources; virtual; abstract;
    { Overwrite this function in your derived renderer class }
    procedure DiscardDeviceDependentResources; virtual; abstract;
    { Overwrite this function in your derived renderer class to do your rendering. You do not need to call render}
    procedure DoRender; virtual; abstract;
    { Overwrite this function in your derived renderer class to do your rendering. You do not need to call render}
    procedure BeforeRender; virtual;
    { Overwrite this function in your derived renderer class to do your rendering. You do not need to call render}
    procedure AfterRender; virtual;

    { Call this function from your derived renderer class. }
    procedure SetRenderTargetDC(aDC: HDC; const aRect: TRect; dcAlphaMode: TD2D1_ALPHA_MODE = D2D1_ALPHA_MODE_PREMULTIPLIED);
    { Call this function from your derived renderer class. }
    procedure SetRenderTargetHwnd(aWnd: HWND);
  public
    procedure Render;


    constructor Create;
    destructor Destroy; override;
  end;

//function POINT_2F(x, y: Single): TD2D1_POINT_2F;

implementation

const
  D2DERR_RECREATE_TARGET: NativeUInt = $8899000C;

{function POINT_2F(x, y: Single): TD2D1_POINT_2F;
begin
  Result.x := x; Result.y := y;
end;   }

{ TDirect2DRenderer }

constructor TDirect2DRenderer.Create;
var
  pFactoryOptions: TD2D1_FACTORY_OPTIONS;
begin
  // initialize COM-Things
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);

  // Direct2D-fFactory
  pFactoryOptions.debugLevel := D2D1_DEBUG_LEVEL_ERROR;
  D2D1CreateFactory(D2D1_FACTORY_TYPE_MULTI_THREADED, ID2D1Factory, @pFactoryOptions, fFactory);

  DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED, IDWriteFactory, fWriteFactory);
end;

procedure TDirect2DRenderer.SetRenderTargetDC(aDC: HDC; const aRect: TRect; dcAlphaMode: TD2D1_ALPHA_MODE);
var
  rtdc: ID2D1DCRenderTarget;
  prop: TD2D1_RENDER_TARGET_PROPERTIES;
begin
  fTarget := aDC; fTargetRect := aRect;
  if (fRenderTarget <> nil) and (fRenderTarget.QueryInterface(ID2D1DCRenderTarget, rtdc) <> S_OK) then
  begin
    DiscardDeviceDependentResources;
    fRenderTarget := nil;
  end;
  if fRenderTarget = nil then
  begin
    prop := DX12.D2D1.RenderTargetProperties(DX12.D2D1.PixelFormat(DXGI_FORMAT_B8G8R8A8_UNORM, dcAlphaMode));
    fFactory.CreateDCRenderTarget(@prop, ID2D1DCRenderTarget(fRenderTarget));
  end;
  if (fRenderTarget <> nil) and (fRenderTarget.QueryInterface(ID2D1DCRenderTarget, rtdc) = S_OK) then rtdc.BindDC(aDC, @fTargetRect);
end;

procedure TDirect2DRenderer.SetRenderTargetHwnd(aWnd: HWND);
var
  prop: TD2D1_RENDER_TARGET_PROPERTIES;
  prop_hwnd: TD2D1_HWND_RENDER_TARGET_PROPERTIES;
  s: TD2D1_SIZE_U;
begin
  fTarget := aWnd;
  if fRenderTarget <> nil then
  begin
    DiscardDeviceDependentResources;
    fRenderTarget := nil;
  end;

  // Create HWND-fRenderTarget
  prop := DX12.D2D1.RenderTargetProperties(DX12.D2D1.PixelFormat());
  // determine size of render area
  GetWindowRect(fTarget, fTargetRect);
  s.Width := fTargetRect.Right-fTargetRect.left; s.Height := fTargetRect.Bottom-fTargetRect.Top;
  prop_hwnd := DX12.D2D1.HwndRenderTargetProperties(fTarget, s, D2D1_PRESENT_OPTIONS_NONE); // PRESENT IMMEDIATELY means that it should not delay the rendering (sleep) until next vsync occurs
  fFactory.CreateHwndRenderTarget(prop, prop_hwnd, ID2D1HwndRenderTarget(fRenderTarget));
end;

procedure TDirect2DRenderer.RecreateTarget;
var
  rthwnd: ID2D1HwndRenderTarget;
  rtdc: ID2D1DCRenderTarget;
begin
  if fRenderTarget.QueryInterface(ID2D1DCRenderTarget, rtdc) <> S_OK then
  begin
    fRenderTarget := nil;
    SetRenderTargetDC(fTarget, fTargetRect);
  end else if fRenderTarget.QueryInterface(ID2D1HwndRenderTarget, rthwnd) <> S_OK then
  begin
    fRenderTarget := nil;
    SetRenderTargetHwnd(fTarget);
  end;
end;

procedure TDirect2DRenderer.BeforeRender;
begin
  //
end;

procedure TDirect2DRenderer.AfterRender;
begin
  //
end;

procedure TDirect2DRenderer.Render;
  procedure InternalRender;
  begin
    fRenderTarget.BeginDraw();
    DoRender;
    if NativeUInt(fRenderTarget.EndDraw()) = D2DERR_RECREATE_TARGET then
    begin
      RecreateTarget;
      InternalRender;
    end;
  end;
begin
  CreateDeviceDependentResources;
  BeforeRender;
  InternalRender;
  AfterRender;
end;

destructor TDirect2DRenderer.Destroy;
begin
  DiscardDeviceDependentResources;
  fWriteFactory := nil;
  fRenderTarget := nil;
  fFactory := nil;
  CoUninitialize;
  inherited Destroy;
end;

end.

