unit ClipboardWindow.Renderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, Win32Extra, ToolBox, Clipboard.List, ClipboardWindow.Entry, LayeredWindowRenderer, DX12.D2D1, DX12.DWrite{$IfDef Logging}, logging{$EndIf}, Math, ClipboardWindow.Settings;

type
  { TCBWndRenderer }

  TCBWndRenderer = class(TLayeredWindowRenderer)
  strict private
    function GetScrollValueMax: Single;
  private
    fEntryList: TCBWndEntryList;
    fSettings: TCBWND_Settings;

    fScrollVal: Single;

    entryBackgroundBrush, blackBrush, entryTextBrush, timestampTextBrush: ID2D1SolidColorBrush;
    selectedEntry_radialGradientBrushStops1, selectedEntry_radialGradientBrushStops2: Array of TD2D1_GRADIENT_STOP;
    selectedEntry_radialGradientBrushStopCollection1, selectedEntry_radialGradientBrushStopCollection2: ID2D1GradientStopCollection;
    selectedEntry_radialBrush1, selectedEntry_radialBrush2: ID2D1RadialGradientBrush;
    SegmentationLineBrushLight, SegmentationLineBrushDark: ID2D1SolidColorBrush;
    mainLayer: ID2D1Layer;
    mainLayer_gradBrush: ID2D1LinearGradientBrush;
    mainLayer_gradBrushStopCollection: ID2D1GradientStopCollection;
    mainLayer_gradBrushStops: Array of TD2D1_GRADIENT_STOP;
    entryRect: TD2D1_ROUNDED_RECT;
    fTextFormat, fTextFormatTimestamp: IDWriteTextFormat;

    //fPixelOffset: Single;

    //property ScrollValueMax: Single read GetScrollValueMax;
  protected
    procedure CreateDeviceDependentResources; override;
    procedure DiscardDeviceDependentResources; override;
    procedure DoRender; override;
  public
    property ScrollValue: Single read fScrollVal write fScrollVal;
    property Settings: TCBWND_Settings read fSettings write fSettings;

    { This function receives an clipboard window item index and returns the corresponding pixel-wise scroll value for this item. }
    function ItemIndexToScrollValue(aIndex: Integer): Single;
    { Return the item index of nearest item by current scroll value. Additionally saves the pixel offset of current scroll value to determined item index in fPixelOffset. }
    procedure GetItemIndexByCurrentScrollValue(out aItemIndex: Integer; out aPixelOffset: Single);

    procedure UpdateRenderTarget; override;

    constructor Create(aWnd: HWND; aEntryList: TCBWndEntryList);
    destructor Destroy; override;
  end;

implementation

{ TCBWndRenderer }

constructor TCBWndRenderer.Create(aWnd: HWND; aEntryList: TCBWndEntryList);
begin
  inherited Create(aWnd);
  fEntryList := aEntryList;
  fScrollVal := 0;
end;

function TCBWndRenderer.GetScrollValueMax: Single;
begin
  Result := (fSettings.DefaultEntryHeight+fSettings.MarginEntryEntry)*(fEntryList.Count-1);
end;

function TCBWndRenderer.ItemIndexToScrollValue(aIndex: Integer): Single;
begin
  Result := (fSettings.DefaultEntryHeight+fSettings.MarginEntryEntry)*(aIndex);
end;

procedure TCBWndRenderer.GetItemIndexByCurrentScrollValue(out aItemIndex: Integer; out aPixelOffset: Single);
begin
  aItemIndex := Round(fScrollVal / (fSettings.DefaultEntryHeight + fSettings.MarginEntryEntry));
  aPixelOffset := fScrollVal - (aItemIndex*(fSettings.DefaultEntryHeight + fSettings.MarginEntryEntry));
  {$IfDef Logging}Logger.Add('TCBWndRenderer.GetItemIndexByCurrentScrollValue: ItemIndex='+IntToStr(aItemIndex)+'; PixelOffset='+FloatToStr(aPixelOffset) + '; ScrollVal=' + FloatToStr(fScrollVal));{$EndIf}
end;

procedure TCBWndRenderer.UpdateRenderTarget;
begin
  inherited UpdateRenderTarget;

  // 0.5 is an offset for PixelSnapping (Direct2D addresses pixels at their middlepoint !!!) -> you would get actually a 2 pixel wide line for a stroke width of 1
  entryRect.Rect.Init(WindowRect.Left + fSettings.MarginLeft + 0.5, WindowRect.Top - (fSettings.ActiveEntryHeight / 2) + 0.5, WindowRect.Right - fSettings.MarginRight - 0.5, WindowRect.Top - (fSettings.ActiveEntryHeight / 2) + fSettings.DefaultEntryHeight - 0.5);
  entryRect.radiusX := fSettings.EntryRectRoundingRadius;
  entryRect.radiusY := fSettings.EntryRectRoundingRadius;

  SetLength(mainLayer_gradBrushStops, 4);
  mainLayer_gradBrushStops[0].position := 0.05; mainLayer_gradBrushStops[1].position := 0.25;
  mainLayer_gradBrushStops[2].position := 0.75; mainLayer_gradBrushStops[3].position := 0.95;
  mainLayer_gradBrushStops[0].color := ColorF(0, 0, 0, 0); mainLayer_gradBrushStops[1].color := ColorF(0, 0, 0, 1);
  mainLayer_gradBrushStops[2].color := ColorF(0, 0, 0, 1); mainLayer_gradBrushStops[3].color := ColorF(0, 0, 0, 0);

  SetLength(selectedEntry_radialGradientBrushStops1, 2);
  selectedEntry_radialGradientBrushStops1[0].position := 0; selectedEntry_radialGradientBrushStops1[1].position := 1;
  //selectedEntry_radialGradientBrushStops1[2].position := 0.7; selectedEntry_radialGradientBrushStops1[3].position := 1;
  selectedEntry_radialGradientBrushStops1[0].color := ColorF(1, 1, 1, 1); selectedEntry_radialGradientBrushStops1[1].color := ColorF(1, 1, 1, 0);
  //selectedEntry_radialGradientBrushStops1[2].color := ColorF(1, 1, 1, 0.5); selectedEntry_radialGradientBrushStops1[3].color := ColorF(1, 1, 1, 0);

  SetLength(selectedEntry_radialGradientBrushStops2, 4);
  selectedEntry_radialGradientBrushStops2[0].position := 0; selectedEntry_radialGradientBrushStops2[1].position := 0.4;
  selectedEntry_radialGradientBrushStops2[2].position := 0.7; selectedEntry_radialGradientBrushStops2[3].position := 1;
  selectedEntry_radialGradientBrushStops2[0].color := ColorF(1, 1, 1, 1); selectedEntry_radialGradientBrushStops2[1].color := ColorF(1, 1, 1, 1);
  selectedEntry_radialGradientBrushStops2[2].color := ColorF(1, 1, 1, 0.5); selectedEntry_radialGradientBrushStops2[3].color := ColorF(1, 1, 1, 0);

  WriteFactory.CreateTextFormat(PWideChar(fSettings.EntryFont), nil, DWRITE_FONT_WEIGHT_NORMAL, DWRITE_FONT_STYLE_NORMAL, DWRITE_FONT_STRETCH_NORMAL, fSettings.EntryFontsize, PWideChar(ToolBox.GetUserDefaultLocaleName()), fTextFormat);
  fTextFormat.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_LEADING);
  fTextFormat.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_NEAR);
  fTextFormat.SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP);

  WriteFactory.CreateTextFormat(PWideChar(fSettings.TimestampFont), nil, DWRITE_FONT_WEIGHT_NORMAL, DWRITE_FONT_STYLE_NORMAL, DWRITE_FONT_STRETCH_NORMAL, fSettings.TimestampFontsize, PWideChar(ToolBox.GetUserDefaultLocaleName()), fTextFormatTimestamp);
  fTextFormatTimestamp.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_LEADING);
  fTextFormatTimestamp.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_CENTER);
  fTextFormatTimestamp.SetWordWrapping(DWRITE_WORD_WRAPPING_EMERGENCY_BREAK);
end;

procedure TCBWndRenderer.CreateDeviceDependentResources;
var
  infRect: TD2D1_RECT_F;
  lgbp: TD2D1_LINEAR_GRADIENT_BRUSH_PROPERTIES;
  brushProps: TD2D1_BRUSH_PROPERTIES;
  rgbp: TD2D1_RADIAL_GRADIENT_BRUSH_PROPERTIES;
begin
  // create brushes
  if blackBrush = nil then RenderTarget.CreateSolidColorBrush(DX12.D2D1.ColorF(DX12.D2D1.Black), nil, blackBrush);
  if entryBackgroundBrush = nil then RenderTarget.CreateSolidColorBrush(fSettings.EntryBackgroundColor, nil, entryBackgroundBrush);
  if entryTextBrush = nil then RenderTarget.CreateSolidColorBrush(fSettings.EntryFontColor, nil, entryTextBrush);
  if timestampTextBrush = nil then RenderTarget.CreateSolidColorBrush(fSettings.TimestampFontColor, nil, timestampTextBrush);
  if SegmentationLineBrushDark = nil then RenderTarget.CreateSolidColorBrush(fSettings.SegmentationLineDarkColor, nil, SegmentationLineBrushDark);
  if SegmentationLineBrushLight = nil then RenderTarget.CreateSolidColorBrush(fSettings.SegmentationLineLightColor, nil, SegmentationLineBrushLight);

  if mainLayer_gradBrushStopCollection = nil then RenderTarget.CreateGradientStopCollection(@mainLayer_gradBrushStops[0], Length(mainLayer_gradBrushStops), D2D1_GAMMA_1_0, D2D1_EXTEND_MODE_CLAMP, mainLayer_gradBrushStopCollection);
  if mainLayer_gradBrush = nil then
  begin
    lgbp := LinearGradientBrushProperties(POINT2F(WindowRect.Left, WindowRect.Top), POINT2F(WindowRect.Left, WindowRect.Bottom));
    brushProps := BrushProperties();
    RenderTarget.CreateLinearGradientBrush(@lgbp, @brushProps, mainLayer_gradBrushStopCollection, mainLayer_gradBrush);
  end;

  if mainLayer = nil then
  begin
    infRect := InfiniteRect;
    RenderTarget.CreateLayer(@infRect, mainLayer);
  end;

  if selectedEntry_radialGradientBrushStopCollection1 = nil then RenderTarget.CreateGradientStopCollection(@selectedEntry_radialGradientBrushStops1[0], Length(selectedEntry_radialGradientBrushStops1), D2D1_GAMMA_2_2, D2D1_EXTEND_MODE_CLAMP, selectedEntry_radialGradientBrushStopCollection1);
  if selectedEntry_radialBrush1 = nil then
  begin
    rgbp := RadialGradientBrushProperties(Point2F(WindowRect.Left + WindowSize.cx/2, WindowRect.Top + entryRect.rect.top*3.5), Point2F(), WindowSize.cx*1.25, fSettings.DefaultEntryHeight*4/2);
    //brushProps := BrushProperties();
    RenderTarget.CreateRadialGradientBrush(@rgbp, @brushProps, selectedEntry_radialGradientBrushStopCollection1, selectedEntry_radialBrush1);
  end;

  if selectedEntry_radialGradientBrushStopCollection2 = nil then RenderTarget.CreateGradientStopCollection(@selectedEntry_radialGradientBrushStops2[0], Length(selectedEntry_radialGradientBrushStops2), D2D1_GAMMA_1_0, D2D1_EXTEND_MODE_CLAMP, selectedEntry_radialGradientBrushStopCollection2);
  if selectedEntry_radialBrush2 = nil then
  begin
    rgbp := RadialGradientBrushProperties(Point2F(WindowRect.Left + WindowSize.cx/2, WindowRect.Bottom), Point2F(), WindowSize.cx, WindowSize.cy);
    //brushProps := BrushProperties();
    RenderTarget.CreateRadialGradientBrush(@rgbp, @brushProps, selectedEntry_radialGradientBrushStopCollection2, selectedEntry_radialBrush2);
  end;
end;

procedure TCBWndRenderer.DiscardDeviceDependentResources;
begin
  blackBrush := nil;
  entryBackgroundBrush := nil;
  entryTextBrush := nil;
  timestampTextBrush := nil;
  SegmentationLineBrushLight := nil;
  SegmentationLineBrushDark := nil;
  selectedEntry_radialGradientBrushStopCollection1 := nil; selectedEntry_radialGradientBrushStopCollection2 := nil;
  selectedEntry_radialBrush1 := nil; selectedEntry_radialBrush2 := nil;
  mainLayer_gradBrushStopCollection := nil;
  mainLayer_gradBrush := nil;
  mainLayer := nil;

  TRenderingInfo.DiscardDeviceDependentResources;
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


procedure TCBWndRenderer.DoRender;
var
  layerParams: TD2D1_LAYER_PARAMETERS;
  i: Integer;
  curTransformation: TD2D_MATRIX_3X2_F;
  origin: TD2D1_POINT_2F;
  r: TD2D1_ROUNDED_RECT;
  iconRect: TD2D1_RECT_F;
  distance, currentHeight: Single;
  currentTopLine: Single;
begin
  RenderTarget.Clear(fSettings.WindowBackgroundColor);

  RenderTarget.SetTransform(IdentityMatrix);
  //RenderTarget.DrawLine(POINT2F(fWndRect.Left, fWndRect.Top+fWndSize.cy/2), POINT2F(fWndRect.Right, fWndRect.Top+fWndSize.cy/2), blackBrush);

  layerParams := LayerParameters(nil, D2D1_ANTIALIAS_MODE_PER_PRIMITIVE, 1, mainLayer_gradBrush, D2D1_LAYER_OPTIONS_INITIALIZE_FOR_CLEARTYPE);
  RenderTarget.PushLayer(@layerParams, mainLayer);

  currentTopLine := 0;

  fEntryList.Lock;
  for i := 0 to fEntryList.Count-1 do
  begin
    distance := Abs(fScrollVal - i*(fSettings.DefaultEntryHeight+fSettings.MarginEntryEntry));

    currentHeight := distance * (fSettings.DefaultEntryHeight-fSettings.ActiveEntryHeight)/(fSettings.DefaultEntryHeight+fSettings.MarginEntryEntry) + fSettings.ActiveEntryHeight;
    currentHeight := Max(fSettings.DefaultEntryHeight, currentHeight);
    currentHeight := Min(fSettings.ActiveEntryHeight, currentHeight);

    entryRect.rect.Bottom := entryRect.rect.Top - 1 + currentHeight;

    curTransformation := Matrix3x2F_Translation(0, currentTopLine + (WindowSize.cy/2) - fScrollVal);

    currentTopLine := currentTopLine+currentHeight+fSettings.MarginEntryEntry;

    // if transformed entryRect is still visible, do not process it
    if (curTransformation.TransformPoint(POINT2F(0, entryRect.rect.bottom)).y < 0) or
      (curTransformation.TransformPoint(POINT2F(0, entryRect.rect.top)).y > WindowSize.cy) then continue;

    RenderTarget.SetTransform(curTransformation);
    //if fEntryList[i].TextLayout = nil then
      fEntryList[i].MakeLayout(WriteFactory, fTextFormat, entryRect.Rect.right-entryRect.Rect.left-fSettings.PaddingLeft-fSettings.PaddingRight-fSettings.IconMarginLeft-fSettings.IconMarginRight-fSettings.IconWidth, entryRect.Rect.bottom-entryRect.Rect.top-fSettings.PaddingTop-fSettings.PaddingBottom);
    //if fEntryList[i].TextLayoutTimestamp = nil then
      fEntryList[i].MakeLayoutTimestamp(WriteFactory, fTextFormatTimestamp, fSettings.IconWidth, fSettings.ActiveEntryHeight-fSettings.PaddingTop-fSettings.PaddingBottom-fSettings.IconHeight);
      //fEntryList[i].MakeLayoutTimestamp(WriteFactory, fTextFormatTimestamp, entryRect.Rect.right-entryRect.Rect.left-PaddingLeft-PaddingRight, ActiveEntryHeight-PaddingTop-PaddingBottom);

    RenderTarget.FillRoundedRectangle(entryRect, entryBackgroundBrush);

    if fEntryList[i].IconAsBitmap = nil then fEntryList[i].ConvertIcon(RenderTarget);
    iconRect.Left := entryRect.Rect.Left + fSettings.IconMarginLeft; iconRect.Right := iconRect.Left + fSettings.IconWidth;
    iconRect.Top := entryRect.Rect.Top + fSettings.IconMarginTop; iconRect.Bottom := iconRect.Top + fSettings.IconHeight;
    RenderTarget.DrawBitmap(fEntryList[i].IconAsBitmap, @iconRect, 1);

    if fEntryList.ToIndex = i then
    begin
      r := entryRect; r.rect.top := r.rect.top - 0.5; r.rect.bottom := r.rect.bottom + 0.5;
      r.rect.left := r.rect.left - 0.5; r.rect.right := r.rect.right + 0.5;
      RenderTarget.DrawRoundedRectangle(r, SegmentationLineBrushLight);
      r := entryRect; r.rect.top := r.rect.top + 0.5; r.rect.bottom := r.rect.bottom - 0.5;
      r.rect.left := r.rect.left + 0.5; r.rect.right := r.rect.right - 0.5;
      RenderTarget.DrawRoundedRectangle(r, SegmentationLineBrushLight);
      RenderTarget.DrawRoundedRectangle(entryRect, blackBrush);

      //r := entryRect;
      //r.rect.bottom := r.rect.top + (r.rect.bottom-r.rect.top)*0.5;
      //RenderTarget.FillRoundedRectangle(r, selectedEntry_radialBrush1);
      //r := entryRect;
      //r.rect.top := r.rect.top + (r.rect.bottom-r.rect.top)*0.5;
      //RenderTarget.FillRoundedRectangle(r, selectedEntry_radialBrush2);
    end;

    if currentHeight > fSettings.DefaultEntryHeight then
    begin
      RenderTarget.PushAxisAlignedClip(entryRect.rect, D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
      origin.x := entryRect.Rect.left+fSettings.PaddingLeft;
      origin.y := entryRect.Rect.Top+fSettings.PaddingTop+fSettings.IconHeight;
      RenderTarget.DrawTextLayout(origin, fEntryList[i].TextLayoutTimestamp, timestampTextBrush, D2D1_DRAW_TEXT_OPTIONS_CLIP);
      RenderTarget.PopAxisAlignedClip();
    end;

    // activate pixel-snapping (achieved by disabling anti-aliasing for this horizontal lines)
    //RenderTarget.SetAntialiasMode(D2D1_ANTIALIAS_MODE_ALIASED);
    if i > 0 then RenderTarget.DrawLine(POINT2F(entryRect.Rect.left + fSettings.SegmentationLineMargin, entryRect.rect.top - (fSettings.MarginEntryEntry / 2) - 0.5), POINT2F(entryRect.Rect.right - fSettings.SegmentationLineMargin, entryRect.rect.top - (fSettings.MarginEntryEntry / 2) - 0.5), SegmentationLineBrushDark, 1);
    if i > 0 then RenderTarget.DrawLine(POINT2F(entryRect.Rect.left + fSettings.SegmentationLineMargin, entryRect.rect.top - (fSettings.MarginEntryEntry / 2) + 0.5), POINT2F(entryRect.Rect.right - fSettings.SegmentationLineMargin, entryRect.rect.top - (fSettings.MarginEntryEntry / 2) + 0.5), SegmentationLineBrushLight, 1);
    // re-activate anti-aliasing
    //RenderTarget.SetAntialiasMode(D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);

    origin.x := entryRect.Rect.left+fSettings.PaddingLeft+fSettings.IconWidth+fSettings.IconMarginLeft+fSettings.IconMarginRight;
    origin.y := entryRect.Rect.top + fSettings.PaddingTop;

    {$IfDef Debug}
    if fEntryList = nil then raise Exception.Create('EntryList is nil');
    if fEntryList[i] = nil then raise Exception.Create('EntryList[i] is nil');
    if fEntryList[i].TextLayout = nil then raise Exception.Create('EntryList[i].TextLayout is nil');
    {$EndIf}

    RenderTarget.DrawTextLayout(origin, fEntryList[i].TextLayout, EntryTextBrush, D2D1_DRAW_TEXT_OPTIONS_CLIP);
  end;
  fEntryList.Unlock;

  RenderTarget.PopLayer();
end;

destructor TCBWndRenderer.Destroy;
begin
  fTextFormat := nil;
  inherited Destroy;
end;

end.

